package adt

//sealed abstract case class Rank private(value: Char)
//object Rank {
//  def create(value: Char): Either[String, Rank] = value match {
//    case '2' | '3' | '4' | '5' | '6' |
//         '7' | '8' | '9' | 'T' | 'J' |
//         'Q' | 'K' | 'A'    => Right(new Rank(value) {})
//    case _                  => Left(s"Rank symbol not correct: '$value'")
//  }
//}

sealed trait Rank {
  def value: Char
}

object Rank {
  final case object Two extends Rank {
    def value = '2'
  }
  final case object Three extends Rank {
    def value = '3'
  }
  final case object Four extends Rank {
    def value = '4'
  }
  final case object Five extends Rank {
    def value = '5'
  }
  final case object Six extends Rank {
    def value = '6'
  }
  final case object Seven extends Rank {
    def value = '7'
  }
  final case object Eight extends Rank {
    def value = '8'
  }
  final case object Nine extends Rank {
    def value = '9'
  }
  final case object Ten extends Rank {
    def value = 'T'
  }
  final case object Jack extends Rank {
    def value = 'J'
  }
  final case object Queen extends Rank {
    def value = 'Q'
  }
  final case object King extends Rank {
    def value = 'K'
  }
  final case object Ace extends Rank {
    def value = 'A'
  }

  // Smart constructor
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
    case _   => Left(s"Rank symbol not correct: '$value'")
  }

}
