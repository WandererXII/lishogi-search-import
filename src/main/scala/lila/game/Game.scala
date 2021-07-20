package lila.game

import scala.concurrent.duration._

import shogi.Color.{ Gote, Sente }
import shogi.format.{ FEN, Uci }
import shogi.opening.{ FullOpening, FullOpeningDB }
import shogi.variant.{ FromPosition, Standard, Variant }
import shogi.{
  History => ShogiHistory,
  Centis,
  CheckCount,
  Clock,
  Color,
  Mode,
  MoveOrDrop,
  Speed,
  Status,
  Game => ShogiGame,
  StartingPosition
}
import org.joda.time.DateTime

import lila.common.Sequence
import lila.db.ByteArray

case class Game(
    id: String,
    sentePlayer: Player,
    gotePlayer: Player,
    status: Status,
    turns: Int, // = ply
    startedAtTurn: Int,
    clock: Option[Clock] = None,
    daysPerTurn: Option[Int],
    mode: Mode = Mode.default,
    variant: Variant = Variant.default,
    createdAt: DateTime = DateTime.now,
    movedAt: DateTime = DateTime.now,
    metadata: Metadata
) {

  val players = List(sentePlayer, gotePlayer)

  def player(color: Color): Player = color.fold(sentePlayer, gotePlayer)

  def player(playerId: String): Option[Player] =
    players find (_.id == playerId)

  def player(c: Color.type => Color): Player = player(c(Color))

  def isPlayerFullId(player: Player, fullId: String): Boolean =
    (fullId.size == Game.fullIdSize) && player.id == (fullId drop 8)

  def player: Player = player(turnColor)

  def playerByUserId(userId: String): Option[Player] = players.find(_.userId contains userId)

  def opponent(p: Player): Player = opponent(p.color)

  def opponent(c: Color): Player = player(!c)

  lazy val firstColor = Color(sentePlayer before gotePlayer)
  def firstPlayer     = player(firstColor)
  def secondPlayer    = player(!firstColor)

  def turnColor = Color((turns & 1) == 0)

  def turnOf(p: Player): Boolean = p == player
  def turnOf(c: Color): Boolean  = c == turnColor

  def playedTurns = turns - startedAtTurn

  def flagged = if (status == Status.Outoftime) Some(turnColor) else None

  // we can't rely on the clock,
  // because if moretime was given,
  // elapsed time is no longer representing the game duration
  def durationSeconds: Option[Int] =
    (movedAt.getMillis / 1000 - createdAt.getMillis / 1000) match {
      case seconds if seconds > 60 * 60 * 12 => None // no way it lasted more than 12 hours, come on.
      case seconds                           => Some(seconds.toInt)
    }

  def correspondenceClock: Option[CorrespondenceClock] =
    daysPerTurn map { days =>
      val increment   = days * 24 * 60 * 60
      val secondsLeft = (movedAt.getMillis / 1000 + increment - System.currentTimeMillis() / 1000).toInt max 0
      CorrespondenceClock(
        increment = increment,
        senteTime = turnColor.fold(secondsLeft, increment).toFloat,
        goteTime = turnColor.fold(increment, secondsLeft).toFloat
      )
    }

  def speed = shogi.Speed(clock.map(_.config))

  def perfKey  = PerfPicker.key(this)
  def perfType = lila.rating.PerfType(perfKey)

  def started = status >= Status.Started

  def notStarted = !started

  def aborted = status == Status.Aborted

  def playedThenAborted = aborted && bothPlayersHaveMoved

  def playable = status < Status.Aborted && !imported

  def playableEvenImported = status < Status.Aborted

  def playableBy(p: Player): Boolean = playable && turnOf(p)

  def playableBy(c: Color): Boolean = playableBy(player(c))

  def playableByAi: Boolean = playable && player.isAi

  def mobilePushable = isCorrespondence && playable && nonAi

  def alarmable = hasCorrespondenceClock && playable && nonAi

  def continuable =
    status != Status.Mate && status != Status.Stalemate && status != Status.Impasse27 && status != Status.PerpetualCheck // todo

  def aiLevel: Option[Int] = players find (_.isAi) flatMap (_.aiLevel)

  def hasAi: Boolean = players.exists(_.isAi)
  def nonAi          = !hasAi

  def aiPov: Option[Pov] = players.find(_.isAi).map(_.color) map pov

  def mapPlayers(f: Player => Player) =
    copy(
      sentePlayer = f(sentePlayer),
      gotePlayer = f(gotePlayer)
    )

  def boosted = rated && finished && bothPlayersHaveMoved && playedTurns < 10

  def rated  = mode.rated
  def casual = !rated

  def finished = status >= Status.Mate

  def finishedOrAborted = finished || aborted

  def ratingVariant =
    if (metadata.tournamentId.isDefined && variant == shogi.variant.FromPosition) shogi.variant.Standard
    else variant

  def fromPosition = variant == shogi.variant.FromPosition || source.contains(Source.Position)

  def imported = source contains Source.Import

  def fromPool  = source contains Source.Pool
  def fromLobby = source contains Source.Lobby

  def winner = players find (_.wins)

  def loser = winner map opponent

  def winnerColor: Option[Color] = winner map (_.color)

  def winnerUserId: Option[String] = winner flatMap (_.userId)

  def loserUserId: Option[String] = loser flatMap (_.userId)

  def wonBy(c: Color): Option[Boolean] = winnerColor map (_ == c)

  def lostBy(c: Color): Option[Boolean] = winnerColor map (_ != c)

  def drawn = finished && winner.isEmpty

  def isCorrespondence = speed == shogi.Speed.Correspondence

  def hasClock = clock.isDefined

  def hasCorrespondenceClock = daysPerTurn.isDefined

  def isUnlimited = !hasClock && !hasCorrespondenceClock

  def estimateClockTotalTime = clock.map(_.estimateTotalSeconds)

  def estimateTotalTime =
    estimateClockTotalTime orElse
      correspondenceClock.map(_.estimateTotalTime) getOrElse 1200

  def onePlayerHasMoved    = playedTurns > 0
  def bothPlayersHaveMoved = playedTurns > 1

  def startColor = Color(startedAtTurn % 2 == 0)

  def playerMoves(color: Color): Int =
    if (color == startColor) (playedTurns + 1) / 2
    else playedTurns / 2

  def playerHasMoved(color: Color) = playerMoves(color) > 0

  def isBeingPlayed = !finishedOrAborted

  def olderThan(seconds: Int) = movedAt isBefore DateTime.now.minusSeconds(seconds)

  def unplayed = !bothPlayersHaveMoved && (createdAt isBefore Game.unplayedDate)

  def forecastable = started && playable && isCorrespondence && !hasAi

  def userIds = playerMaps(_.userId)

  def userRatings = playerMaps(_.rating)

  def averageUsersRating =
    userRatings match {
      case a :: b :: Nil => Some((a + b) / 2)
      case a :: Nil      => Some((a + 1500) / 2)
      case _             => None
    }

  def source = metadata.source

  def resetTurns = copy(turns = 0, startedAtTurn = 0)

  def synthetic = id == Game.syntheticId

  private def playerMaps[A](f: Player => Option[A]): List[A] = players flatMap f

  def pov(c: Color) = Pov(this, c)
  def sentePov      = pov(Sente)
  def gotePov       = pov(Gote)
}

object Game {

  type ID = String

  case class WithAnalysed(game: Game, analysed: Boolean)

  val syntheticId = "synthetic"

  val maxPlayingRealtime = 100 // plus 200 correspondence games

  val analysableVariants: Set[Variant] = Set(
    shogi.variant.Standard,
    shogi.variant.FromPosition
  )

  val unanalysableVariants: Set[Variant] = Variant.all.toSet -- analysableVariants

  val variantsWhereSenteIsBetter: Set[Variant] = Set()

  val blindModeVariants: Set[Variant] = Set(
    shogi.variant.Standard,
    shogi.variant.FromPosition
  )

  def allowRated(variant: Variant, clock: Clock.Config) =
    variant.standard || clock.estimateTotalTime >= Centis(3000)

  val gameIdSize   = 8
  val playerIdSize = 4
  val fullIdSize   = 12
  val tokenSize    = 4

  val unplayedHours = 24
  def unplayedDate  = DateTime.now minusHours unplayedHours

  val abandonedDays = 21
  def abandonedDate = DateTime.now minusDays abandonedDays

  val aiAbandonedHours = 6
  def aiAbandonedDate  = DateTime.now minusHours aiAbandonedHours

  def takeGameId(fullId: String)   = fullId take gameIdSize
  def takePlayerId(fullId: String) = fullId drop gameIdSize

  object BSONFields {

    val id                = "_id"
    val sentePlayer       = "p0"
    val gotePlayer        = "p1"
    val playerIds         = "is"
    val playerUids        = "us"
    val playingUids       = "pl"
    val oldPgn            = "pg"
    val huffmanPgn        = "hp"
    val binaryPieces      = "ps"
    val status            = "s"
    val turns             = "t"
    val startedAtTurn     = "st"
    val clock             = "c"
    val checkCount        = "cc"
    val daysPerTurn       = "cd"
    val moveTimes         = "mt"
    val senteClockHistory = "cw"
    val goteClockHistory  = "cb"
    val periodsSente      = "pw"
    val periodsGote       = "pb"
    val rated             = "ra"
    val analysed          = "an"
    val variant           = "v"
    val crazyData         = "hs"
    val createdAt         = "ca"
    val movedAt           = "ua" // ua = updatedAt (bc)
    val source            = "so"
    val tournamentId      = "tid"
    val simulId           = "sid"
    val winnerColor       = "w"
    val winnerId          = "wid"
    val initialFen        = "if"
  }
}

// Represents at what turn we entered a new period
case class PeriodEntries(sente: Vector[Int], gote: Vector[Int]) {
  def apply(c: Color) =
    c.fold(sente, gote)

  def update(c: Color, t: Int) =
    c.fold(copy(sente = sente :+ t), copy(gote = gote :+ t))

  def byoyomi(c: Color): Boolean =
    c.fold(!sente.isEmpty, !gote.isEmpty)

  def turnIsPresent(c: Color, t: Int): Boolean = c.fold(sente contains t, gote contains t)
  def dropTurn(c: Color, t: Int) =
    c.fold(
      copy(sente = sente filterNot (_ == t)),
      copy(gote = gote filterNot (_ == t))
    )

  def first(c: Color): Option[Int] = c.fold(sente.headOption, gote.headOption)
  def last(c: Color): Option[Int]  = c.fold(sente.lastOption, gote.lastOption)
}

object PeriodEntries {
  val default = PeriodEntries(Vector(), Vector())
  val zeroes  = PeriodEntries(Vector(0), Vector(0))
}

case class ClockHistory(
    sente: Vector[Centis] = Vector.empty,
    gote: Vector[Centis] = Vector.empty,
    periodEntries: PeriodEntries = PeriodEntries.default
) {

  def update(color: Color, f: Vector[Centis] => Vector[Centis]): ClockHistory =
    color.fold(copy(sente = f(sente)), copy(gote = f(gote)))

  def record(color: Color, clock: Clock, finalRecord: Boolean = false): ClockHistory = {
    if (finalRecord && clock.isUsingByoyomi(color)) {
      val remTime = clock.remainingTime(color)
      update(color, _ :+ (clock.byoyomi - remTime))
    } else update(color, _ :+ clock.remainingTimeTurn(color))
  }

  def validateStart(clock: Clock): ClockHistory = {
    if (clock.startsAtZero && periodEntries(Sente).isEmpty && periodEntries(Gote).isEmpty)
      copy(periodEntries = PeriodEntries.zeroes)
    else this
  }

  def reset(color: Color) = update(color, _ => Vector.empty)

  def apply(color: Color): Vector[Centis] = color.fold(sente, gote)

  def last(color: Color) = apply(color).lastOption

  def size = sente.size + gote.size

  def updateWithByoTime(color: Color, byo: Centis): Vector[Centis] = {
    val standardTimes = color.fold(
      sente.take(turnByoyomiStarted(color) - 1),
      gote.take(turnByoyomiStarted(color) - 1)
    )
    standardTimes.padTo(color.fold(sente.size, gote.size), byo)
  }

  def turnByoyomiStarted(color: Color): Int =
    periodEntries.first(color) getOrElse 300

  def enteredNewPeriod(color: Color, turn: Int) =
    copy(periodEntries = periodEntries.update(color, turn))

  def turnIsPresent(color: Color, turn: Int): Boolean =
    periodEntries.turnIsPresent(color, turn)

  def dropTurn(color: Color, turn: Int) = {
    if (turnIsPresent(color, turn)) copy(periodEntries = periodEntries.dropTurn(color, turn))
    else this
  }

  // first state is of the color that moved first.
  def bothClockStates(firstMoveBy: Color, byo: Centis): Vector[Centis] =
    Sequence.interleave(
      firstMoveBy.fold(updateWithByoTime(Sente, byo), updateWithByoTime(Gote, byo)),
      firstMoveBy.fold(updateWithByoTime(Gote, byo), updateWithByoTime(Sente, byo))
    )
}
