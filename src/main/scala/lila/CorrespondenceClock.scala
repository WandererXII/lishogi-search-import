package lishogi.game

import shogi.Color

// times are expressed in seconds
case class CorrespondenceClock(
    increment: Int,
    senteTime: Float,
    goteTime: Float
) {

  def daysPerTurn = increment / 60 / 60 / 24

  def emerg = 60 * 10

  def remainingTime(c: Color) = c.fold(senteTime, goteTime)

  def outoftime(c: Color) = remainingTime(c) == 0

  // in seconds
  def estimateTotalTime = increment * 40 / 2

  def incrementHours = increment / 60 / 60
}
