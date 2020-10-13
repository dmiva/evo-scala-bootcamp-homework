package adt

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

// Alternative using value class (does not compiles due to incompatibility with existing solution)
//final case class Suit private (value: Char) extends AnyVal
//object Suit {
//  def create(value: Char): Either[String, Suit] = value match {
//        case 'c' | 'd' | 'h' | 's' => Right(Suit(value))
//        case _   => Left(s"Suit symbol '$value' is not valid.")
//  }
//}


