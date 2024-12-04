package apps.app64

import cs214.webapp.UserId
import upickle.default.*

case class Stake(tricks: Int, bets: Int) derives ReadWriter

enum Suit derives ReadWriter:
  case Hearts, Diamonds, Clubs, Spades


case class Card(suit: Suit, value: Int) derives ReadWriter:
  override def toString =
    s"$value of ${suit}"

type Hand = Set[Card]

case class State(
  players: Vector[UserId],
  stakes: Map[UserId, Stake],
  hands:  Map[UserId, Hand],
  scores: Map[UserId, Int],
  trumpSuit: Suit,
  currentSuit: Suit,
  round: Int,
  phase: Phase
)

enum Phase:
  case Bet, Play, RoundEnd, GameEnd

enum Event derives ReadWriter:
  case AnnounceBet(bet: Int)
  case PlayCard(card: Card)

case class View (
  phaseView: PhaseView,
  scoreView: Map[UserId, Int]
) derives ReadWriter

enum PhaseView derives ReadWriter:
  case CardSelecting(hand: Hand, stakes: Map[UserId, Stake])
  case BetSelecting(bet: Int)
  case Waiting(ready: Map[UserId, Boolean])
