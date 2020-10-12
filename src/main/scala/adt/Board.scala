package adt

sealed abstract case class Board private (cards: List[Card])
object Board{
  def create(cards: List[Card]): Either[String, Board] = {
    val duplicates = listOfDuplicates(cards)

    cards match {
      case _ if cards.length != 5   => Left(s"Board must contain 5 cards, but found ${cards.length}")
      case _ if duplicates.nonEmpty => Left(s"Board contain duplicate cards: ${duplicates.mkString(", ")}")
      case _                        => Right(new Board(cards) {})
    }
  }

  private def listOfDuplicates[T](list: List[T]): Iterable[T] = {
    list.groupBy(identity).collect {
      case (t, values) if values.lengthIs > 1 => t
    }
  }
}
