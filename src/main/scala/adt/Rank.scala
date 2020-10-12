package adt

sealed trait Rank

object Rank {
  final case object Two extends Rank
  final case object Three extends Rank
  final case object Four extends Rank
  final case object Five extends Rank
  final case object Six extends Rank
  final case object Seven extends Rank
  final case object Eight extends Rank
  final case object Nine extends Rank
  final case object Ten extends Rank
  final case object Jack extends Rank
  final case object Queen extends Rank
  final case object King extends Rank
  final case object Ace extends Rank

  def create(value: Char): Either[String, Rank] = value match {
    case '2' => Right(Two)
    case '3' => Right(Three)
    case '4' => Right(Four)
    case '5' => Right(Five)
    case '6' => Right(Six)
    case '7' => Right(Seven)
    case '8' => Right(Eight)
    case '9' => Right(Nine)
    case 'T' => Right(Ten)
    case 'J' => Right(Jack)
    case 'Q' => Right(Queen)
    case 'K' => Right(King)
    case 'A' => Right(Ace)
    case _   => Left(s"Rank symbol '$value' is not valid.")
  }
}

//sealed abstract case class Rank private(value: Char)
//object Rank {
//  def create(value: Char): Either[String, Rank] = value match {
//    case '2' | '3' | '4' | '5' | '6' |
//         '7' | '8' | '9' | 'T' | 'J' |
//         'Q' | 'K' | 'A'    => Right(new Rank(value) {})
//    case _                  => Left(s"Rank symbol not correct: '$value'")
//  }
