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



//  object Card {
//    def parse(str: String): Card = {
//
//    }
//  }

final case class Card(rank: Rank, suit: Suit)

  sealed trait GameType {
    def handLength: Int
  }
  object GameType {
    final case object Texas extends GameType {
      override def handLength: Int = 2
    }
    final case object Omaha extends GameType {
      override def handLength: Int = 4
    }
  }



  final case class Board(cards: List[Card])

  final case class TestCase(board: Board, hands: List[Hand])
  object TestCase {
    def parse(input: String): TestCase = ???
  }

  final case class TestResult(board: Board, hands: List[Hand])

  sealed trait PokerCombination
  object PokerCombination {
    final case class HighCard(cards: List[Card]) extends PokerCombination
    final case class OnePair(cards: List[Card]) extends PokerCombination
    final case class TwoPairs(cards: List[Card]) extends PokerCombination
    final case class ThreeOfAKind(cards: List[Card]) extends PokerCombination
    final case class Straight(cards: List[Card]) extends PokerCombination
    final case class Flush(cards: List[Card]) extends PokerCombination
    final case class FullHouse(cards: List[Card]) extends PokerCombination
    final case class FourOfAKind(cards: List[Card]) extends PokerCombination
    final case class StraightFlush(cards: List[Card]) extends PokerCombination
  }


  def main(args: Array[String]): Unit = {

    val suit1 = Suit.create('d')
    val suit2 = Suit.create('d')
    println((suit1))
    println((suit2))
//    val rank3 = Rank('r')
    val rank1 = Rank.create('R')
    val rank2 = Rank.create('J')
    println((rank1))
    println((rank2))
    val rank5 = Rank.create('a')
//    val card1 = Card(Rank.Two, Suit.Diamonds)
//    println(card1)


    // Incorrect length
    val hand1 = Hand.create(List(Card(Rank.Eight, Suit.Clubs)), GameType.Omaha)
    println(hand1)

    // Duplicates
    val hand2 = Hand.create(List(Card(Rank.Eight, Suit.Clubs),Card(Rank.Eight, Suit.Clubs)), GameType.Texas)
    println(hand2)

    val card2 = for {
      rank <- Rank.create('2')
      suit <- Suit.create('f')

    } yield Card(rank, suit)

    println(s"card2 $card2")

    val dup = List(Card(Rank.Two, Suit.Clubs),Card(Rank.Four, Suit.Clubs))
    val dup1 = dup.groupBy(identity).collect {
      case (card, listOfSameCards) if listOfSameCards.lengthIs > 1 => card
    }

    val dups = dup.distinctBy(identity)
    println(dup1)
    println(dups)

    def duplicates[T](list: List[T]): Iterable[T] = {
      list.groupBy(identity).collect {
        case (t, values) if values.lengthIs > 1 => t
      }
    }

//    val card1 = for {
//      suit <- Suit.create('4')
//      rank <- Rank.create('T')
//    } yield Card(Rank.create('a'), suit)

  }

}
