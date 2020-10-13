package adt

import scala.annotation.tailrec

sealed abstract case class Hand private (cards: List[Card], gameType: GameType)
object Hand {
  def create(cards: List[Card], gameType: GameType): Either[String, Hand] = {
    val duplicates = listOfDuplicates(cards)

    (cards, gameType) match {
      case _ if cards.length != gameType.handLength => Left(s"Hand must contain ${gameType.handLength} cards, but found ${cards.length}")
      case _ if duplicates.nonEmpty                 => Left(s"Hand contain duplicate cards: ${duplicates.mkString(", ")}")
      case _                                        => Right(new Hand(cards, gameType) {})
    }
  }

  private def listOfDuplicates[T](list: List[T]): Iterable[T] = {
    list.groupBy(identity).collect {
      case (t, values) if values.lengthIs > 1 => t
    }
  }
}