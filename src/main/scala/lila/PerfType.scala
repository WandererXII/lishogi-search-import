package lishogi.rating

import shogi.Centis

import shogi.Speed

sealed abstract class PerfType(
    val id: Int,
    val key: String,
    val name: String,
    val title: String,
    val iconChar: Char
) {

  def iconString = iconChar.toString
}

object PerfType {

  case object UltraBullet
      extends PerfType(
        0,
        key = "ultraBullet",
        name = Speed.UltraBullet.name,
        title = Speed.UltraBullet.title,
        iconChar = '{'
      )

  case object Bullet
      extends PerfType(
        1,
        key = "bullet",
        name = Speed.Bullet.name,
        title = Speed.Bullet.title,
        iconChar = 'T'
      )

  case object Blitz
      extends PerfType(
        2,
        key = "blitz",
        name = Speed.Blitz.name,
        title = Speed.Blitz.title,
        iconChar = ')'
      )

  case object Rapid
      extends PerfType(
        6,
        key = "rapid",
        name = Speed.Rapid.name,
        title = Speed.Rapid.title,
        iconChar = '#'
      )

  case object Classical
      extends PerfType(
        3,
        key = "classical",
        name = Speed.Classical.name,
        title = Speed.Classical.title,
        iconChar = '+'
      )

  case object Correspondence
      extends PerfType(
        4,
        key = "correspondence",
        name = "Correspondence",
        title = "Correspondence (days per turn)",
        iconChar = ';'
      )

  case object Standard
      extends PerfType(
        5,
        key = "standard",
        name = shogi.variant.Standard.name,
        title = "Standard rules of shogi",
        iconChar = '8'
      )

  case object Puzzle
      extends PerfType(
        20,
        key = "puzzle",
        name = "Training",
        title = "Training puzzles",
        iconChar = '-'
      )

  val all: List[PerfType] = List(
    UltraBullet,
    Bullet,
    Blitz,
    Rapid,
    Classical,
    Correspondence,
    Standard,
    Puzzle
  )
  val byKey = all.map { p => (p.key, p) }.toMap
  val byId  = all.map { p => (p.id, p) }.toMap

  val default = Standard

  def apply(key: String): Option[PerfType] = byKey get key
  def orDefault(key: String): PerfType     = apply(key) getOrElse default

  def apply(id: Int): Option[PerfType] = byId get id

  def name(key: String): Option[String] = apply(key) map (_.name)

  def id2key(id: Int): Option[String] = byId get id map (_.key)

  val nonPuzzle: List[PerfType] = List(
    UltraBullet,
    Bullet,
    Blitz,
    Rapid,
    Classical,
    Correspondence
  )
  val nonGame: List[PerfType] = List(Puzzle)
  val leaderboardable: List[PerfType] = List(
    Bullet,
    Blitz,
    Rapid,
    Classical,
    UltraBullet
  )
  val variants: List[PerfType] = List()

  val standard: List[PerfType] =
    List(Bullet, Blitz, Rapid, Classical, Correspondence)

  def isGame(pt: PerfType) = !nonGame.contains(pt)

  val nonPuzzleIconByName = nonPuzzle.map { pt =>
    pt.name -> pt.iconString
  }.toMap

  def variantOf(pt: PerfType): shogi.variant.Variant =
    pt match {
      case _             => shogi.variant.Standard
    }

  def byVariant(variant: shogi.variant.Variant): Option[PerfType] =
    variant match {
      case _                           => None
    }

  lazy val totalTimeRoughEstimation: Map[PerfType, Centis] = nonPuzzle.map { pt =>
    pt -> Centis(pt match {
      case UltraBullet    => 25 * 100
      case Bullet         => 90 * 100
      case Blitz          => 7 * 60 * 100
      case Rapid          => 12 * 60 * 100
      case Classical      => 15 * 60 * 100
      case Correspondence => 60 * 60 * 100
      case _              => 7 * 60 * 100
    })
  }.toMap

  def iconByVariant(variant: shogi.variant.Variant): Char =
    byVariant(variant).fold('C')(_.iconChar)
}
