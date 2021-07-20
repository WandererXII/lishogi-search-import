package lila.game

import shogi.format.{ FEN, Uci, Forsyth }
import shogi.variant.Variant
import shogi.{
  CheckCount,
  Clock,
  Color,
  Sente,
  Gote,
  Mode,
  Status,
}
import org.joda.time.DateTime
import reactivemongo.api.bson._
import scala.util.{ Failure, Success, Try }

import shogi.Centis
import lila.db.{ BSON, ByteArray }

object BSONHandlers {

  import lila.db.ByteArray.ByteArrayBSONHandler
  import lila.db.BSON._

  implicit val StatusBSONHandler = tryHandler[Status](
    {
      case BSONInteger(v) =>
        Status(v).fold[Try[Status]](Failure(new Exception(s"No such status: $v")))(Success.apply)
    },
    x => BSONInteger(x.id)
  )

  implicit val gameBSONHandler: BSON[Game] = new BSON[Game] {

    import Game.BSONFields._
    import Player.playerBSONHandler

    private val emptyPlayerBuilder = playerBSONHandler.read(BSONDocument())

    def reads(r: BSON.Reader): Game = {
      val winC               = r boolO winnerColor map Color.apply
      val (senteId, goteId) = r str playerIds splitAt 4
      val uids               = r.getO[List[String]](playerUids) getOrElse Nil
      val (senteUid, goteUid) =
        (uids.headOption.filter(_.nonEmpty), uids.lift(1).filter(_.nonEmpty))
      def player(
          field: String,
          color: Color,
          id: Player.Id,
          uid: Player.UserId
      ): Player = {
        val builder =
          r.getO[Player.Builder](field)(playerBSONHandler) | emptyPlayerBuilder
        val win = winC map (_ == color)
        builder(color)(id)(uid)(win)
      }

      val g = Game(
        id = r str id,
        sentePlayer = player(sentePlayer, Sente, senteId, senteUid),
        gotePlayer = player(gotePlayer, Gote, goteId, goteUid),
        status = r.get[Status](status),
        turns = r int turns,
        startedAtTurn = r intD startedAtTurn,
        daysPerTurn = r intO daysPerTurn,
        mode = Mode(r boolD rated),
        variant = Variant(r intD variant) | shogi.variant.Standard,
        createdAt = r date createdAt,
        movedAt = r.dateD(movedAt, r date createdAt),
        metadata = Metadata(
          source = r intO source flatMap Source.apply,
          tournamentId = r strO tournamentId,
          analysed = r boolD analysed
        )
      )

      val gameClock = r.getO[Color => Clock](clock)(
        clockBSONReader(
          g.createdAt,
          g.sentePlayer.berserk,
          g.gotePlayer.berserk
        )
      ) map (_(g.turnColor))

      g.copy(clock = gameClock)
    }

    def writes(w: BSON.Writer, o: Game) = ???
  }

  implicit val gameWithAnalysedBSONHandler: BSON[Game.WithAnalysed] =
    new BSON[Game.WithAnalysed] {
      def reads(r: BSON.Reader): Game.WithAnalysed = {
        Game.WithAnalysed(
          gameBSONHandler.reads(r),
          r boolD Game.BSONFields.analysed
        )
      }

      def writes(w: BSON.Writer, o: Game.WithAnalysed) = ???
    }

  private def periodEntries(color: Color, clockHistory: Option[ClockHistory]) =
    for {
      history <- clockHistory
    } yield BinaryFormat.periodEntries.writeSide(history.periodEntries(color))

  private def clockHistory(
      color: Color,
      clockHistory: Option[ClockHistory],
      clock: Option[Clock],
      flagged: Option[Color]
  ) =
    for {
      clk     <- clock
      history <- clockHistory
      times = history(color)
    } yield BinaryFormat.clockHistory.writeSide(
      clk.limit,
      times,
      flagged contains color
    )

  private[game] def clockBSONReader(since: DateTime, senteBerserk: Boolean, goteBerserk: Boolean) =
    new BSONReader[Color => Clock] {
      def readTry(bson: BSONValue): Try[Color => Clock] =
        bson match {
          case bin: BSONBinary =>
            ByteArrayBSONHandler readTry bin map { cl =>
              BinaryFormat.clock(since).read(cl, senteBerserk, goteBerserk)
            }
          case b => lila.db.BSON.handlerBadType(b)
        }
    }
}
