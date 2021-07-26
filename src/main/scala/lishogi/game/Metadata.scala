package lishogi.game

import lishogi.db.ByteArray
import org.joda.time.DateTime

private[game] case class Metadata(
    source: Option[Source],
    tournamentId: Option[String],
    analysed: Boolean
) {

  def isEmpty = this == Metadata.empty
}

private[game] object Metadata {

  val empty = Metadata(None, None, false)
}
