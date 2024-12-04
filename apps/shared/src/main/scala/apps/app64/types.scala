package apps.app64

import scala.util.Random.between
import cs214.webapp.UserId
import upickle.default.*

case class Stake(tricks: Int, bets: Int) derives ReadWriter

enum Suit derives ReadWriter:
  case Hearts, Diamonds, Clubs, Spades
object Suit:
  def random = Suit.values(between(0, Suit.values.size))

case class Card(suit: Suit, value: Int) derives ReadWriter:
  override def toString =
    s"$value of ${suit}"
  def scoreAgainst(others: List[Card], trump: Suit, current: Suit): Int =
    // TODO: This does not implement the functionality needed for jokers/wizards
    if (this.suit == current || this.suit == trump)
       && others.forall(c => 
          c.suit != trump || 
          this.suit == trump ||
          c.suit != this.suit  || 
          c.value < this.value) 
       then 1
    else 0

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
