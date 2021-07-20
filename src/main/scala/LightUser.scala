package lishogi

case class LightUser(id: String, name: String, title: Option[String] = None)

case class Users(sente: LightUser, gote: LightUser) {

  def apply(color: shogi.Color) = color.fold(sente, gote)
}
