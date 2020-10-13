package adt

sealed abstract case class TestCase private (board: Board, hands: List[Hand], gameType: GameType)
object TestCase {
  def create(board: Board, hands: List[Hand], gameType: GameType): Either[String, TestCase] = {
    val handCards = hands.foldLeft(List[Card]())((x, hand) => x ++ hand.cards)
    val duplicates = listOfDuplicates(board.cards ++ handCards)
    val isHandsGameTypeEqual = hands.forall(_.gameType == gameType)

    (board, hands, gameType) match {
      case _ if duplicates.nonEmpty => Left(s"Test case contain duplicate cards: ${duplicates.mkString(", ")}")
      case _ if !isHandsGameTypeEqual => Left(s"Test case contain hands for Texas and Omaha")
      case _ => Right(new TestCase(board, hands, gameType) {})
    }
  }

  private def listOfDuplicates[T](list: List[T]): Iterable[T] = {
    list.groupBy(identity).collect {
      case (t, values) if values.lengthIs > 1 => t
    }
  }

}
