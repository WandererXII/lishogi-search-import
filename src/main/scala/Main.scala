package lishogi

import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._
import akka.util.ByteString
import java.nio.file.Paths
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api.libs.json._
import play.api.libs.ws._
import play.api.libs.ws.ahc._
import reactivemongo.akkastream.{ cursorProducer, State }
import reactivemongo.api.ReadPreference
import reactivemongo.api.bson._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import shogi.variant.{ Standard, Variant }
import lishogi.DB.BSONDateTimeHandler
import lishogi.game.BSONHandlers._
import lishogi.game.{ Game }

object Main extends App {

  implicit val system = ActorSystem()
  // implicit val materializer = system.materializer
  val wsClient = StandaloneAhcWSClient()

  val datePattern   = "yyyy-MM-dd"
  val dateFormatter = DateTimeFormat forPattern datePattern
  def parseDate(str: String) =
    scala.util.Try(dateFormatter parseDateTime str).toOption

  val endpoint = args lift 0 getOrElse {
    sys error "Missing endpoint argument"
  }
  val search = new Search(wsClient, endpoint)

  val sinceOption: Option[Either[Unit, DateTime]] = for {
    str <- args lift 1
    res <- if (str == "reset") Some(Left(())) else parseDate(str) map Right.apply
  } yield res

  val since = sinceOption match {
    case None => sys error "Missing since date argument"
    case Some(Right(date)) =>
      println(s"Resume since $date")
      date
    case _ =>
      println("Reset game index")
      Await.result(search.putMapping, 20.seconds)
      parseDate("2020-01-01").get
  }

  /**
    * Given an operation that produces a T, returns a Future containing the result of T, unless an exception is thrown,
    * in which case the operation will be retried after _delay_ time, if there are more possible retries, which is configured through
    * the _retries_ parameter. If the operation does not succeed and there is no retries left, the resulting Future will contain the last failure.
    */
  def retry[T](
      op: => Future[T],
      delay: FiniteDuration,
      retries: Int
  ): Future[T] =
    op recoverWith {
      case e if retries > 0 =>
        println(e.getMessage)
        akka.pattern.after(delay, system.scheduler)(
          retry(op, delay, retries - 1)
        )
    }

  DB.get foreach {
    case (db, dbClose) =>
      val gameSource = db.gameColl
        .find(
          BSONDocument(
            "ca" -> BSONDocument("$gt" -> since),
            "s"  -> BSONDocument("$gte" -> shogi.Status.Mate.id)
          ),
          Some(
            BSONDocument(
              Game.BSONFields.oldPgn            -> false,
              Game.BSONFields.huffmanPgn        -> false,
              Game.BSONFields.binaryPieces      -> false,
              Game.BSONFields.moveTimes         -> false,
              Game.BSONFields.senteClockHistory -> false,
              Game.BSONFields.goteClockHistory  -> false
            )
          )
        )
        .sort(BSONDocument("ca" -> 1))
        .cursor[Game.WithAnalysed]()
        .documentSource(maxDocs = Int.MaxValue)
      // .documentSource(maxDocs = 100000)

      val tickSource =
        Source.tick(Reporter.freq, Reporter.freq, None)

      gameSource
        .buffer(20000, OverflowStrategy.backpressure)
        .map(g => Some(g))
        .merge(tickSource, eagerComplete = true)
        .via(Reporter)
        .grouped(1000)
        .mapAsyncUnordered(10) { games =>
          val payload = JsObject(games map {
            case Game.WithAnalysed(g, a) =>
              g.id -> JsString(Json.stringify(search.toDoc(g, a)))
          })
          retry(search.store(payload), 10 seconds, 20)
        }
        .runWith(Sink.ignore) andThen {
        case state =>
          search.refresh map { _ =>
            dbClose()
            wsClient.close()
            system.terminate()
          }
      }
  }
}
