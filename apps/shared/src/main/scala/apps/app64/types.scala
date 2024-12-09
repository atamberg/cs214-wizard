package apps.app64

import scala.util.Random.{shuffle, between}
import cs214.webapp.UserId
import upickle.default.*

case class Stake(tricksWon: Int, bid: Int) derives ReadWriter:
  // TODO: Check values against ruleset, am too lazy to look them up
  lazy val score = if tricksWon == bid then 
      bid * 10 + 10 else math.abs(tricksWon - bid) * (-10)

enum Suit derives ReadWriter:
  case Hearts, Diamonds, Clubs, Spades, None;
  override def toString(): String = 
    this match
      case Hearts =>   "♥️"
      case Diamonds => "♦️"
      case Clubs =>    "♣️"
      case Spades =>   "♠️"
      case None =>     ""

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
  import Suit.*
  val isWizard = value == 15
  val isJester = value == 1
  // Jester = 1, 2-10 = 2-10,  Jack = 11, Queen = 12, King = 13, Ace = 14, Wizard = 15
  override def toString =
    (this.suit, this.value) match
      case (_, 1)         => "🃏"
      case (_, 15)        => "🧙"

      case (Hearts, 2)    => "🂲"
      case (Hearts, 3)    => "🂳"
      case (Hearts, 4)    => "🂴"
      case (Hearts, 5)    => "🂵"
      case (Hearts, 6)    => "🂶"
      case (Hearts, 7)    => "🂷"
      case (Hearts, 8)    => "🂸"
      case (Hearts, 9)    => "🂹"
      case (Hearts, 10)   => "🂺"
      case (Hearts, 11)   => "🂻"
      case (Hearts, 12)   => "🂽"
      case (Hearts, 13)   => "🂾"
      case (Hearts, 14)   => "🂱"
      case (Diamonds, 2)  => "🃂"
      case (Diamonds, 3)  => "🃃"
      case (Diamonds, 4)  => "🃄"
      case (Diamonds, 5)  => "🃅"
      case (Diamonds, 6)  => "🃆"
      case (Diamonds, 7)  => "🃇"
      case (Diamonds, 8)  => "🃈"
      case (Diamonds, 9)  => "🃉"
      case (Diamonds, 10) => "🃊"
      case (Diamonds, 11) => "🃋"
      case (Diamonds, 12) => "🃍"
      case (Diamonds, 13) => "🃎"
      case (Diamonds, 14) => "🃁"
      case (Clubs, 2)     => "🃒"
      case (Clubs, 3)     => "🃓"
      case (Clubs, 4)     => "🃔"
      case (Clubs, 5)     => "🃕"
      case (Clubs, 6)     => "🃖"
      case (Clubs, 7)     => "🃗"
      case (Clubs, 8)     => "🃘"
      case (Clubs, 9)     => "🃙"
      case (Clubs, 10)    => "🃚"
      case (Clubs, 11)    => "🃛"
      case (Clubs, 12)    => "🃝"
      case (Clubs, 13)    => "🃞"
      case (Clubs, 14)    => "🃑"
      case (Spades, 2)    => "🂢"
      case (Spades, 3)    => "🂣"
      case (Spades, 4)    => "🂤"
      case (Spades, 5)    => "🂥"
      case (Spades, 6)    => "🂦"
      case (Spades, 7)    => "🂧"
      case (Spades, 8)    => "🂨"
      case (Spades, 9)    => "🂩"
      case (Spades, 10)   => "🂪"
      case (Spades, 11)   => "🂫"
      case (Spades, 12)   => "🂭"
      case (Spades, 13)   => "🂮"
      case (Spades, 14)   => "🂡"

      case _              => "🂠"

  def scoreAgainst(others: Vector[Card], trump: Suit, current: Suit): Int =
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
  players:     Vector[UserId],
  stakes:      Map[UserId, Stake],
  cardsPlayed: Vector[(UserId, Card)],
  hands:       Map[UserId, Hand],
  scores:      Map[UserId, Int],
  trumpSuit:   Suit,
  currentSuit: Suit,
  round:       Int,
  phase:       Phase
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

  private def withPlayerNext(user: UserId): State =
    // TODO: I think using mutability is usefull here, needs proper testing obviously
    require(players.contains(user))
    var playerQueue = players
    while playerQueue.head != user do
      playerQueue = playerQueue.tail :+ playerQueue.head
    copy(players = playerQueue)

  private def updateStakes(): Map[UserId, Stake]  = 
    require(phase == Phase.GameEnd)
    (for
      (player, card) <- cardsPlayed
      otherCards = cardsPlayed.dropWhile(_ != (player, card)).map(_._2)
    yield
      // TODO: This is missing some jester edge cases
      if !card.isWizard then
      player -> (
        stakes(player).copy(
          (stakes(player).tricksWon + card.scoreAgainst(otherCards, trumpSuit, currentSuit))
          ,stakes(player).bid)
      )
      else 
        val cardIndex = cardsPlayed.indexOf((player, card))
        player -> (
          stakes(player).copy(
            (stakes(player).tricksWon + (if cardsPlayed.take(cardIndex).exists((_, c) => c.isWizard) then 0 else 1))
          ,stakes(player).bid)
      )
      ).toMap 


  def nextPlay: State = 
    val nextStakes = updateStakes()
    val winner: UserId = nextStakes.filter((u, s) => (s.score - stakes(u).score) > 0).head._1
    withPlayerNext(winner).copy(
      stakes = nextStakes,
      cardsPlayed = Vector(),
      currentSuit = Suit.None,
      phase = Phase.Play
      )

  private def updateScores(): Map[UserId, Int] = 
    for
      (player, stake) <- stakes
    yield
      player -> (scores(player) + stake.score)

  def nextRound: State =
    copy(
      players = players.tail :+ players.head,
      stakes = Map(),
      scores = updateScores(),
      cardsPlayed = Vector(),
      hands = hands.map((u, h) => (u, h.empty)), 
      currentSuit = Suit.None,
      trumpSuit = Suit.random,
      round = round + 1,
      phase = Phase.Bid
      )

  def isHandEmpty(id: UserId): Boolean =
    hands(id).isEmpty


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
  round:       Int
) derives ReadWriter


enum PhaseView derives ReadWriter:
  case CardSelecting(hand: Hand)
  case BidSelecting
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

