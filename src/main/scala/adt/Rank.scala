package adt

sealed abstract case class Rank private(value: Char)
object Rank {
  def create(value: Char): Either[String, Rank] = value match {
    case '2' | '3' | '4' | '5' | '6' |
         '7' | '8' | '9' | 'T' | 'J' |
         'Q' | 'K' | 'A'    => Right(new Rank(value) {})
    case _                  => Left(s"Rank symbol not correct: '$value'")
  }
}
