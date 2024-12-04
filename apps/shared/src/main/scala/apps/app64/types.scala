package apps.app64

import scala.util.Random.{shuffle, between}
import cs214.webapp.UserId
import upickle.default.*

case class Stake(tricksWon: Int, bid: Int) derives ReadWriter

enum Suit derives ReadWriter:
  case Hearts, Diamonds, Clubs, Spades, None
object Suit:
  def random = Suit.values(between(0, Suit.values.size))

object Deck:
  private def shuffledCards: Set[Card] = shuffle:
    (for 
      suit <- Suit.values
      value <- (2 to 15)
    yield
      Card(suit, value)).toSet
  def dealNCards(n: Int, players: Vector[UserId]): Map[UserId, Hand] =
    require(shuffledCards.size/players.size >= n)
    var mutCards = shuffledCards
    players.map(_ -> mutCards.take(n).toSet).toMap

case class Card(suit: Suit, value: Int) derives ReadWriter:
  override def toString =
    s"$value of ${suit}"
  def scoreAgainst(others: List[Card], trump: Suit, current: Suit): Int =
    // TODO: This does not implement the functionality needed for jokers/wizards
    if this.suit == current && others.forall(c => 
          c.suit != trump && (
          c.suit != this.suit  || 
          c.value < this.value )
          )
    || this.suit == trump && others.forall(c => 
          c.suit != this.suit  || 
          c.value < this.value)
       then 
       1
    else 
      0

type Hand = Set[Card]

case class State(
  players: Vector[UserId],
  stakes: Map[UserId, Stake],
  cardsPlayed:  Vector[(UserId, Card)],
  hands:  Map[UserId, Hand],
  scores: Map[UserId, Int],
  trumpSuit: Suit,
  currentSuit: Suit,
  round: Int,
  phase: Phase
):
  lazy val allReady = players.forall(stakes.keySet.contains)
  // TODO: Check that allReady get uninitialized after a copy! We don't want the old ready to persist in the next state!

  def placeBid(id: UserId, bid: Int): State =
    require(phase == Phase.Bid)
    require(!stakes.keySet.contains(id))
    copy(stakes = stakes + (id -> Stake(0, bid)))

  def playCard(id: UserId, card: Card): State =
    require(players.head == id)
    require(hands(id).contains(card))
    val newHands = hands.dropAtKey(id, card)
    val nextCardsPlayed = cardsPlayed :+ (id, card)
    val prevState = copy(cardsPlayed = nextCardsPlayed, hands = newHands)
    if cardsPlayed.isEmpty then
      prevState.copy(currentSuit = card.suit)
    else
      prevState

  def nextPhase(phase: Phase): State =
    copy(phase = phase)

  def nextPlayer: State =
    copy(players = players.tail :+ players.head)

  def isHandEmpty(id: UserId): Boolean =
    hands(id).isEmpty


enum Phase:
  case Bid, Play, RoundEnd, GameEnd

enum Event derives ReadWriter:
  case AnnounceBid(bid: Int)
  case PlayCard(card: Card)

case class View (
  phaseView: PhaseView,
  scoreView: Map[UserId, Int]
) derives ReadWriter

enum PhaseView derives ReadWriter:
  case CardSelecting(hand: Hand, stakes: Map[UserId, Stake])
  case BidSelecting(bid: Int)
  case Waiting(ready: Map[UserId, Boolean])


extension [K,V](m: Map[K,V])
  def updateAtKey(k: K, op: V => V): Map[K,V] =
    m + (k -> (op(m(k))))

extension [K, T](m: Map[K, Seq[T]])
  def appendAtKey(k: K, toAppend: T): Map[K, Seq[T]] =
    m.updateAtKey(k, _ :+ toAppend)

  def prependAtKey(k: K, toPrepend: T): Map[K, Seq[T]] =
    m.updateAtKey(k, toPrepend +: _)

extension [K, T](m: Map[K, Set[T]])
  def dropAtKey(k: K, toDrop: T): Map[K, Set[T]] =
    m.updateAtKey(k, _ - toDrop)

