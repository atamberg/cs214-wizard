package apps.app64

import scala.util.Random.{shuffle, between}
import cs214.webapp.UserId
import upickle.default.*

case class Stake(tricksWon: Int, bid: Int) derives ReadWriter:
  def score =
    if tricksWon == bid then 20 + (tricksWon * 10)
    else math.abs(tricksWon - bid) * (-10)

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
  val allSuits: Array[Suit] = Suit.values.filterNot(_==Suit.None)
  def random = allSuits(between(0, allSuits.size))

object Deck:
  lazy val orderedDeck = {
    for
      suit <- Suit.allSuits
      value <- (1 to 15)
    yield Card(suit, value)
  }.toSet

  lazy val size = orderedDeck.size
  
  private def shuffledCards: Set[Card] = shuffle(orderedDeck)

  def dealNCards(n: Int, players: Vector[UserId]): Map[UserId, Hand] =
    require(
      size/players.size >= n,
      s"orderedDeck.size / players.size < n"
    )
    var mutCards = shuffledCards
    players.map(_ -> {
      val hand = mutCards.take(n).toSet
      mutCards = mutCards.drop(n)
      hand
    }).toMap

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
    if !others.exists(_.isWizard) && (this.suit == current && others.forall(c => 
          c.suit != trump && (
          c.suit != this.suit  || 
          c.value < this.value )
          )
    || this.suit == trump && others.forall(c => 
          c.suit != this.suit  || 
          c.value < this.value))
       then 
       1
    else 
      0

type Hand = Set[Card]

object State:
  def defaultState(clients: Seq[UserId]): State = 
    State(
      players     = clients.toVector, 
      stakes      = Map(),
      cardsPlayed = Vector(),
      hands       = clients.map(_ -> Set()).toMap, 
      scores      = clients.map(_ -> 0).toMap,
      trumpSuit   = Suit.random,
      currentSuit = Suit.None,
      round       = 1,
      phase       = Phase.Bid,
      trickWinner = ""
    )

case class State(
  players:     Vector[UserId],
  stakes:      Map[UserId, Stake],
  cardsPlayed: Vector[(UserId, Card)],
  hands:       Map[UserId, Hand],
  scores:      Map[UserId, Int],
  trumpSuit:   Suit,
  currentSuit: Suit,
  round:       Int,
  phase:       Phase,
  trickWinner: UserId
):
  lazy val allReady = players.forall(stakes.keySet.contains)
  // TODO: Check that allReady get uninitialized after a copy! We don't want the old ready to persist in the next state!

  def placeBid(id: UserId, bid: Int): State =
    require(
      phase == Phase.Bid,
      s"phase = $phase is not Bid"
    )
    require(
      !stakes.keySet.contains(id),
      s"stakes.keySet = ${stakes.keySet} does not contain id = $id"
    )
    copy(stakes = stakes + (id -> Stake(0, bid)))

  def playCard(id: UserId, card: Card): State =
    require(
      players.head == id,
      s"players.head = ${players.head} does not equal id = $id"
    )
    require(
      hands(id).contains(card),
      s"hand = ${hands(id)} does not contain card = $card"
    )
    val newHands = hands.dropAtKey(id, card)
    val nextCardsPlayed = cardsPlayed :+ (id, card)
    val prevState = copy(cardsPlayed = nextCardsPlayed, hands = newHands)
    if currentSuit == Suit.None && !(card.isWizard || card.isJester) then
      prevState.copy(currentSuit = card.suit)
    else
      prevState

  def nextPhase(phase: Phase): State =
    copy(phase = phase)

  def withNewCards: State =
    copy(
        hands = Deck.dealNCards(round, players)
      )

  def nextPlayer: State =
    copy(players = players.tail :+ players.head)

  def withPlayerNext(user: UserId): State =
    // TODO: I think using mutability is usefull here, needs proper testing obviously
    require(
      players.contains(user),
      s"players = $players does not contain user = $user"
    )
    var playerQueue = players
    while playerQueue.head != user do
      playerQueue = playerQueue.tail :+ playerQueue.head
    copy(players = playerQueue)

  private def computeTricks: Map[UserId, Stake]  =
    require(
      phase == Phase.PlayEnd,
      s"phase = $phase is not PlayEnd"
    )
    (for
      (player, card) <- cardsPlayed
      otherCards = cardsPlayed.filter(_ != (player, card)).map(_._2)
    yield
      if !(card.isWizard || card.isJester) then
      player -> (
        stakes(player).copy(
          (stakes(player).tricksWon + card.scoreAgainst(otherCards, trumpSuit, currentSuit)),stakes(player).bid)
      )
      else if card.isJester then
        player -> (
          stakes(player).copy(
            (stakes(player).tricksWon + (if otherCards.forall(_.isJester) && cardsPlayed.head == card then 1 else 0))
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

  def computeWinner: State =
    val nextStakes = computeTricks
    val winner = nextStakes.filter((u, s) => (s.tricksWon - stakes(u).tricksWon) > 0).head._1
    copy(
      stakes = nextStakes,
      trickWinner = winner
    )

  def nextPlay: State =
    withPlayerNext(trickWinner).copy(
      cardsPlayed = Vector(),
      currentSuit = Suit.None,
      phase = Phase.Play,
      )

  private def updateScores(): Map[UserId, Int] = 
    for
      (player, stake) <- stakes
    yield
      player -> (scores(player) + stake.score)

  /**
    * When called on a state in RoundEnd, transitions to next Bid phase
    *
    * @return new state with 
    */
  def nextRound: State =
    require(
      this.phase == Phase.RoundEnd,
      s"phase = ${this.phase} is not RoundEnd"
    )
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
      ).withNewCards

  private def isValid(card: Card): Boolean =
    val highestCurrentSuit = cardsPlayed.map(_._2)
      .filter(_.suit == currentSuit)
      .filterNot(_.isWizard)
      .map(_.value)
      .maxOption

    val highestCurrentTrump = cardsPlayed.map(_._2)
      .filter(_.suit == trumpSuit)
      .filterNot(_.isWizard)
      .map(_.value)
      .maxOption

    card.isWizard
    || card.isJester
    || (card.suit == currentSuit && card.value > highestCurrentSuit.getOrElse(0))
    || (card.suit == trumpSuit && card.value > highestCurrentTrump.getOrElse(0))
  end isValid

  def getValidHand(userId: UserId): Set[(Card, Boolean)] = 
    val validCards = (for
        card <- hands(userId)
      yield
        (card, isValid(card)))
    if !validCards.exists(_._2) || currentSuit == Suit.None then 
      hands(userId).map((_, true))
    else validCards
  end getValidHand

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

