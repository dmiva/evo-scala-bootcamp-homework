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


  // It ended up being verbose...
  
  final case class TestResult(testCase: TestCase, handsRanked: List[Hand])

  sealed trait PokerCombination
  // TODO: Add hand strength value
  object PokerCombination {
    final case class HighCard(cards: List[Card]) extends PokerCombination     // defines by 5 cards in asc. order
    final case class Pair(cards: List[Card]) extends PokerCombination         // defines by Rank of 2 equal cards + 3 highest cards
    final case class TwoPairs(cards: List[Card]) extends PokerCombination     // defines by two Ranks of 2 equal cards + 1 highest card
    final case class ThreeOfAKind(cards: List[Card]) extends PokerCombination // defines by Rank of 3 equal cards + 2 cards
    final case class Straight(cards: List[Card]) extends PokerCombination     // defines by Rank of highest card in sequence
    final case class Flush(cards: List[Card]) extends PokerCombination        // defines by sequence of 5 cards with equal suit
    final case class FullHouse(cards: List[Card]) extends PokerCombination    // defines by Rank of 3 equal cards + Rank of 2 equal cards
    final case class FourOfAKind(cards: List[Card]) extends PokerCombination  // defines by Rank of 4 equal cards + 1 highest card
    final case class StraightFlush(cards: List[Card]) extends PokerCombination// defines by Rank of highest card in sequence with equal suit
  }


  def main(args: Array[String]): Unit = {
    import Rank._
    import Suit._

    val suit1 = Suit.create('d')
    val suit2 = Suit.create('d')
    println(suit1)
    println(suit2)
//    val rank3 = Rank('r')
    val rank1 = Rank.create('R')
    val rank2 = Rank.create('J')
    println(rank1)
    println(rank2)
    val rank5 = Rank.create('a')


    // Incorrect length
    val hand1 = Hand.create(
      List(
        Card(Eight, Clubs),
        Card(Nine, Clubs),
        Card(Ten, Clubs)),
      GameType.Omaha
    )
    println(hand1)

    // Duplicates
    val hand2 = Hand.create(List(Card(Eight, Clubs),Card(Eight, Clubs)), GameType.Texas)
    println(hand2)

    // Correct
    val hand3 = Hand.create(List(Card(Eight, Clubs),Card(Eight, Diamonds)), GameType.Texas)
    println(hand3)

    // Correct
    val hand4 = Hand.create(List(Card(Jack, Clubs),Card(Eight, Diamonds)), GameType.Texas)
    println(hand4)

    // Correct
    val hand5 = Hand.create(List(Card(Ace, Hearts),Card(Eight, Clubs)), GameType.Texas)
    println(hand5)

    // Incorrect length
    val board1 = Board.create(List(Card(Eight, Clubs)))
    println(board1)

    // Duplicates
    val board2 = Board.create(List(Card(Eight, Diamonds),Card(Eight, Diamonds),Card(Nine, Clubs),Card(Ten, Spades),Card(Eight, Hearts)))
    println(board2)

    // Correct
    val board3 = Board.create(
      List(
        Card(Eight, Diamonds),
        Card(Eight, Clubs),
        Card(Nine, Clubs),
        Card(Ten, Spades),
        Card(Eight, Hearts)
      )
    )
    println(board3)

    // Duplicates in board and hands
    val testCase1 = for {
      board <- board3
      handA <- hand1
      handB <- hand4
      handC <- hand5
    } yield TestCase.create(board, List(handA, handB, handC), GameType.Texas)
    println(testCase1)


  }

}
