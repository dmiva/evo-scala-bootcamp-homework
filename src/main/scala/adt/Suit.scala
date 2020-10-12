package adt

//sealed abstract case class Suit private(value: Char)
//object Suit {
//  def create(value: Char): Either[String, Suit] = value match {
//    case 'c' | 'd' | 'h' | 's' => Right(new Suit(value) {})
//    case _ => Left(s"Suit symbol not correct: '$value'")
//  }
//}

sealed trait Suit {
  def value: Char
}

object Suit {
  final case object Clubs extends Suit {
    def value = 'c'
  }
  final case object Diamonds extends Suit {
    def value = 'd'
  }
  final case object Hearts extends Suit {
    def value = 'h'
  }
  final case object Spades extends Suit {
    def value = 's'
  }

  // Smart constructor
  def create(value: Char): Either[String, Suit] = value match {
    case 'c' => Right(Clubs)
    case 'd' => Right(Diamonds)
    case 'h' => Right(Hearts)
    case 's' => Right(Spades)
    case _   => Left(s"Suit symbol not correct: '$value'")
  }

}

