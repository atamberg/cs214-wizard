package apps.app64

import cs214.webapp.UserId

case class Stake(tricks: Int, bets: Int)

enum Suit:
  case Hearts, Diamonds, Clubs, Spades

case class Card(suit: Suit, value: Int):
  override def toString =
    s"$value of ${suit}"

type Hand = Set[Card]
type IdMap[T] = Map[UserId, T]

case class State(
  players: Vector[UserId],
  stakes: IdMap[Stake],
  hands:  IdMap[Hand],
  scores: IdMap[Int],
  trumpSuit: Suit,
  currentSuit: Suit,
  round: Int,
  phase: Phase
)

enum Phase:
  case Bet, Play, RoundEnd, GameEnd

enum Event:
  case AnnounceBet(bet: Int)
  case PlayCard(card: Card)

case class View(
  phaseView: PhaseView,
  scoreView: IdMap[Int]
)

enum PhaseView:
  case CardSelecting(hand: Hand)
  case BetSelecting(bet: Int)
  case Waiting(ready: IdMap[Boolean])
