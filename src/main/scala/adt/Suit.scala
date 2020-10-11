package adt

sealed abstract case class Suit private(value: Char)
object Suit {
  def create(value: Char): Either[String, Suit] = value match {
    case 'c' | 'd' | 'h' | 's' => Right(new Suit(value) {})
    case _ => Left(s"Suit symbol not correct: '$value'")
  }
}
