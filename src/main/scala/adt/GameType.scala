package adt

sealed trait GameType {
  def handLength: Int
}
object GameType {
  final case object Texas extends GameType {
    override def handLength: Int = 2
  }
  final case object Omaha extends GameType {
    override def handLength: Int = 4
  }
}
