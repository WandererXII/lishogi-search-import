package lishogi.game

import org.joda.time.DateTime
import scala.collection.Searching._
import scala.util.Try

import shogi._
import shogi.variant.Variant

import lishogi.db.ByteArray

import org.lishogi.clockencoder.{ Encoder => ClockEncoder }

object BinaryFormat {

  object pgn {

    def write(moves: PgnMoves): ByteArray = ???

    def read(ba: ByteArray): PgnMoves =
      format.pgn.Binary.readMoves(ba.value.toList).get

    def read(ba: ByteArray, nb: Int): PgnMoves =
      format.pgn.Binary.readMoves(ba.value.toList, nb).get
  }

  object clockHistory {

    def writeSide(start: Centis, times: Vector[Centis], flagged: Boolean) = ???

    def readSide(start: Centis, ba: ByteArray, flagged: Boolean) = {
      val decoded: Vector[Centis] =
        ClockEncoder.decode(ba.value, start.centis).view.map(Centis.apply).toVector
      if (flagged) decoded :+ Centis(0) else decoded
    }

    def read(start: Centis, bw: ByteArray, bb: ByteArray, pe: PeriodEntries, flagged: Option[Color],  gameId: String) =
      Try {
        ClockHistory(
          readSide(start, bw, flagged contains Sente),
          readSide(start, bb, flagged contains Gote),
          pe
        )
      }.fold(
        e => { println(s"Exception decoding history on game $gameId", e); none },
        some
      )
  }

  object moveTime {

    private type MT = Int // centiseconds
    private val size = 16
    private val buckets =
      List(10, 50, 100, 150, 200, 300, 400, 500, 600, 800, 1000, 1500, 2000, 3000, 4000, 6000)
    private val encodeCutoffs = buckets
      .zip(buckets.tail)
      .map {
        case (i1, i2) => (i1 + i2) / 2
      }
      .toVector

    private val decodeMap: Map[Int, MT] = buckets.zipWithIndex.map(x => x._2 -> x._1).toMap

    def write(mts: Vector[Centis]): ByteArray = ???

    def read(ba: ByteArray, turns: Int): Vector[Centis] = {
      def dec(x: Int) = decodeMap get x getOrElse decodeMap(size - 1)
      ba.value map toInt flatMap { k =>
        Array(dec(k >> 4), dec(k & 15))
      }
    }.take(turns).view.map(Centis.apply).toVector
  }

  case class clock(start: Timestamp) {

    def legacyElapsed(clock: Clock, color: Color) =
      clock.limit - clock.players(color).remaining

    def computeRemaining(config: Clock.Config, legacyElapsed: Centis) =
      config.limit - legacyElapsed

    def write(clock: Clock): ByteArray = ???

    def read(ba: ByteArray, senteBerserk: Boolean, goteBerserk: Boolean): Color => Clock =
      color => {
        val ia = ba.value map toInt

        // ba.size might be greater than 12 with 5 bytes timers
        // ba.size might be 8 if there was no timer.
        // #TODO remove 5 byte timer case! But fix the DB first!
        val timer = {
          if (ia.size >= 12) readTimer(readInt(ia(8), ia(9), ia(10), ia(11)))
          else None
        }

        val byo = {
          if (ia.size == 14) ia(12)
          else if (ia.size == 10) ia(8)
          else 0
        }

        val per = {
          if (ia.size == 14) ia(13)
          else if (ia.size == 10) ia(9)
          else 1
        }

        ia match {
          case Array(b1, b2, b3, b4, b5, b6, b7, b8, _*) => {
            val config      = Clock.Config(readClockLimit(b1), b2, byo, per)
            val legacySente = Centis(readSignedInt24(b3, b4, b5))
            val legacyGote  = Centis(readSignedInt24(b6, b7, b8))
            Clock(
              config = config,
              color = color,
              players = Color.Map(
                ClockPlayer
                  .withConfig(config)
                  .copy(berserk = senteBerserk)
                  .setRemaining(computeRemaining(config, legacySente)),
                ClockPlayer
                  .withConfig(config)
                  .copy(berserk = goteBerserk)
                  .setRemaining(computeRemaining(config, legacyGote))
              ),
              timer = timer
            )
          }
          case _ => sys error s"BinaryFormat.clock.read invalid bytes: ${ba.showBytes}"
        }
      }

    private def writeTimer(timer: Timestamp) = {
      val centis = (timer - start).centis
      /*
       * A zero timer is resolved by `readTimer` as the absence of a timer.
       * As a result, a clock that is started with a timer = 0
       * resolves as a clock that is not started.
       * This can happen when the clock was started at the same time as the game
       * For instance in simuls
       */
      val nonZero = centis max 1
      writeInt(nonZero)
    }

    private def readTimer(l: Int) =
      if (l != 0) Some(start + Centis(l)) else None

    private def writeClockLimit(limit: Int): Byte = {
      // The database expects a byte for a limit, and this is limit / 60.
      // For 0.5+0, this does not give a round number, so there needs to be
      // an alternative way to describe 0.5.
      // The max limit where limit % 60 == 0, returns 180 for limit / 60
      // So, for the limits where limit % 30 == 0, we can use the space
      // from 181-255, where 181 represents 0.25 and 182 represents 0.50...
      (if (limit % 60 == 0) limit / 60 else limit / 15 + 180).toByte
    }

    private def readClockLimit(i: Int) = {
      if (i < 181) i * 60 else (i - 180) * 15
    }
  }

  object clock {
    def apply(start: DateTime) = new clock(Timestamp(start.getMillis))
  }

  object periodEntries {

    def writeSide(v: Vector[Int]): ByteArray = ???

    def readSide(ba: ByteArray): Vector[Int] = {
      def backToInt(b: Array[Byte]): Int =
        b map toInt match {
          case Array(b1, b2) => (b1 << 8) + b2
          case _             => 0
        }
      val pairs = ba.value.grouped(2)
      (pairs map (backToInt _)).toVector
    }
    def read(bw: ByteArray, bb: ByteArray) =
      Try {
        PeriodEntries(readSide(bw), readSide(bb))
      }.fold(
        e => { println(s"Exception decoding period entries", e); none },
        some
      )
  }

  object piece {

    def write(pieces: PieceMap): ByteArray = ???

    def read(ba: ByteArray, variant: Variant): PieceMap = {
      def splitInts(b: Byte) = b.toInt
      def intPiece(int: Int): Option[Piece] =
        intToRole(int & 15, variant) map { role =>
          Piece(Color((int & 16) == 0), role)
        }
      val pieceInts = ba.value map splitInts
      (Pos.all zip pieceInts).view
        .flatMap { case (pos, int) =>
          intPiece(int) map (pos -> _)
        }
        .to(Map)
    }

    // cache standard start position
    val standard = write(Board.init(shogi.variant.Standard).pieces)

    private def intToRole(int: Int, variant: Variant): Option[Role] =
      int match {
        case 1  => Some(King)
        case 2  => Some(Gold)
        case 3  => Some(Silver)
        case 4  => Some(Knight)
        case 5  => Some(Lance)
        case 6  => Some(Bishop)
        case 7  => Some(Rook)
        case 8  => Some(Tokin)
        case 9  => Some(PromotedLance)
        case 10 => Some(PromotedKnight)
        case 11 => Some(PromotedSilver)
        case 12 => Some(Horse)
        case 13 => Some(Dragon)
        case 14 => Some(Pawn)
        case _  => None
      }
    private def roleToInt(role: Role): Int =
      role match {
        case King           => 1
        case Gold           => 2
        case Silver         => 3
        case Knight         => 4
        case Lance          => 5
        case Bishop         => 6
        case Rook           => 7
        case Tokin          => 8
        case PromotedLance  => 9
        case PromotedKnight => 10
        case PromotedSilver => 11
        case Horse          => 12
        case Dragon         => 13
        case Pawn           => 14
        case _              => 15
      }
  }

  @inline private def toInt(b: Byte): Int = b & 0xff

  def writeInt24(int: Int) = {
    val i = if (int < (1 << 24)) int else 0
    Array((i >>> 16).toByte, (i >>> 8).toByte, i.toByte)
  }

  private val int23Max = 1 << 23
  def writeSignedInt24(int: Int) = {
    val i = if (int < 0) int23Max - int else math.min(int, int23Max)
    writeInt24(i)
  }

  def readInt24(b1: Int, b2: Int, b3: Int) = (b1 << 16) | (b2 << 8) | b3

  def readSignedInt24(b1: Int, b2: Int, b3: Int) = {
    val i = readInt24(b1, b2, b3)
    if (i > int23Max) int23Max - i else i
  }

  def writeInt(i: Int) =
    Array(
      (i >>> 24).toByte,
      (i >>> 16).toByte,
      (i >>> 8).toByte,
      i.toByte
    )

  def readInt(b1: Int, b2: Int, b3: Int, b4: Int) = {
    (b1 << 24) | (b2 << 16) | (b3 << 8) | b4
  }
}
