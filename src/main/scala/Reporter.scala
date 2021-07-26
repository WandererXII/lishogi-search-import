package lishogi

import akka.stream._
import akka.stream.scaladsl._
import akka.stream.stage._
import scala.concurrent.duration._

import lishogi.game.Game
import org.joda.time.DateTime
import org.joda.time.format._

object Reporter extends GraphStage[FlowShape[Option[Game.WithAnalysed], Game.WithAnalysed]] {

  val freq = 5.seconds

  val in             = Inlet[Option[Game.WithAnalysed]]("reporter.in")
  val out            = Outlet[Game.WithAnalysed]("reporter.out")
  override val shape = FlowShape.of(in, out)

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic =
    new GraphStageLogic(shape) {

      private val formatter = DateTimeFormat forStyle "MS"

      private var counter                = 0
      private var prev                   = 0
      private var date: Option[DateTime] = None

      setHandler(
        in,
        new InHandler {
          override def onPush() = {
            grab(in) match {
              case Some(g) => {
                counter += 1
                date = Some(g.game.createdAt)
                push(out, g)
              }
              case None => {
                val gps = (counter - prev) / freq.toSeconds
                println(s"${date.fold("-")(formatter.print)} $counter $gps/s")
                prev = counter
                pull(in)
              }
            }
          }

          setHandler(
            out,
            new OutHandler {
              override def onPull() = {
                pull(in)
              }
            }
          )

          //       override def onUpstreamFinish(): Unit = {
          //         println("finished?")
          //         completeStage()
          //       }
        }
      )

    }
}
