package adt

//sealed abstract case class Suit private(value: Char)
//object Suit {
//  def create(value: Char): Either[String, Suit] = value match {
//    case 'c' | 'd' | 'h' | 's' => Right(new Suit(value) {})
//    case _ => Left(s"Suit symbol not correct: '$value'")
//  }
//}

sealed trait Suit

object Suit {
  final case object Clubs extends Suit
  final case object Diamonds extends Suit
  final case object Hearts extends Suit
  final case object Spades extends Suit

  def create(value: Char): Either[String, Suit] = value match {
    case 'c' => Right(Clubs)
    case 'd' => Right(Diamonds)
    case 'h' => Right(Hearts)
    case 's' => Right(Spades)
    case _   => Left(s"Suit symbol '$value' is not valid.")
  }

}

