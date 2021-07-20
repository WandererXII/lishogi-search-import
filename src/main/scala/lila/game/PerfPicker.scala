package lila.game

import shogi.Speed
import lila.rating.PerfType

object PerfPicker {

  def key(speed: Speed, variant: shogi.variant.Variant, daysPerTurn: Option[Int]): String =
    if (variant.standard) {
      if (daysPerTurn.isDefined || speed == Speed.Correspondence) PerfType.Correspondence.key
      else speed.key
    } else variant.key

  def key(game: Game): String = key(game.speed, game.ratingVariant, game.daysPerTurn)
}
