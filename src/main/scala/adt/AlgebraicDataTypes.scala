package adt

object AlgebraicDataTypes {
  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.

  // Attributions and useful links:
  // https://nrinaudo.github.io/scala-best-practices/definitions/adt.html
  // https://alvinalexander.com/scala/fp-book/algebraic-data-types-adts-in-scala/
  // https://en.wikipedia.org/wiki/Algebraic_data_type


  final case class Card private (rank: Rank, suit: Suit)

  // 2. Rank
  sealed trait Rank
  case object Two extends Rank
  case object Three extends Rank
  case object Four extends Rank
  case object Five extends Rank
  case object Six extends Rank
  case object Seven extends Rank
  case object Eight extends Rank
  case object Nine extends Rank
  case object Ten extends Rank
  case object Jack extends Rank
  case object Queen extends Rank
  case object King extends Rank
  case object Ace extends Rank

  def main(args: Array[String]): Unit = {
    val suit1 = Suit.create('g')
    val suit2 = Suit.create('c')
    println((suit1))
    println((suit2))

    val rank1 = Rank.create('R')
    val rank2 = Rank.create('J')
    println((rank1))
    println((rank2))

    val card1 = for {
      suit <- Suit.create('4')
      rank <- Rank.create('T')
    } yield Card(Rank.create('a'), suit)




  }

}
