package apps.app64

import cs214.webapp.UserId
import upickle.default.*

type Hand = Set[Card]

enum Phase:
  case Bid, Play, PlayEnd, RoundEnd, GameEnd

enum Event derives ReadWriter:
  case AnnounceBid(bid: Int)
  case PlayCard(card: Card)

case class View (
  phaseView: PhaseView,
  scoreView: Map[UserId, Int],
  stateView: StateView
) derives ReadWriter

case class StateView(
  players:     Vector[UserId],
  stakes:      Map[UserId, Stake],
  cardsPlayed: Vector[(UserId, Card)],
  trumpSuit:   Suit,
  currentSuit: Suit,
  round:       Int,
  trickWinner: UserId
) derives ReadWriter

enum PhaseView derives ReadWriter:
  case CardSelecting(validHand: Set[(Card, Boolean)])
  case BidSelecting(hand: Hand)
  case Waiting(hand: Hand)
  case PlayEnding(hand: Hand, trickWinner: UserId)
  case RoundEnding
  case GameEnding
