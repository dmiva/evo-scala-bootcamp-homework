package adt

final case class Card(rank: Rank, suit: Suit)

// No checks, because rank and suit are constructed correctly