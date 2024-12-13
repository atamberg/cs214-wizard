package apps.app64

import cs214.webapp.*
import scala.util.Random.{shuffle, between}
import upickle.default.*

object Deck:
  /** A set that contains every card, in ascending order */
  lazy val orderedDeck = {
    for
      suit <- Suit.allSuits
      value <- (1 to 15)
    yield Card(suit, value)
  }.toList

  /** The amount of cards in the deck */
  lazy val size = orderedDeck.size

  /** This method returns a map where each player is associated to a hand of n randomly selected cards */
  def dealNCards(n: Int, players: Vector[UserId]): Map[UserId, Hand] =
    require(
      size/players.size >= n,
      s"orderedDeck.size / players.size < n"
    )
    var mutCards = shuffle(orderedDeck)
    players.map(_ -> {
      val hand = mutCards.take(n).toSet
      mutCards = mutCards.drop(n)
      hand
    }).toMap

/** Suit describes the "color" of a card */
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
  /** An array of all suits without None */
  val allSuits: Array[Suit] = Suit.values.filterNot(_==Suit.None)
  /** Returns a randomly selected suit (without None) */
  def random = allSuits(between(0, allSuits.size))

/**
  * The Card class describes a standard playing card (and wizards as well as jesters)
  *
  * @param suit The suit of the card
  * @param value The value of the card (1 for jester, 15 for wizard)
  */
case class Card(suit: Suit, value: Int) derives ReadWriter:
  import Suit.*
  /** Indicates whether the card is a wizard or not */
  val isWizard = value == 15
  /** Indicates whether the card is a jester or not */
  val isJester = value == 1
  /**
    * A toString override to match cards to emojis
    * Jester = 1, 2-10 = 2-10,  Jack = 11, Queen = 12, King = 13, Ace = 14, Wizard = 15
    *
    * @return A string that contains an emoji of the given card
    */
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
  /**
    * A method to determine whether a card wins agains some other cards
    * !!! This method does not include logic to resolve tricks that contain multiple wizards or only jesters !!!
    *    => This case is handled in the game state
    * @param others A vector of all the other cards that were played in the same trick
    * @param trump The trump suit for the current trick
    * @param current The current suit for the current trick
    * @return 1 if the card wins, 0 if another wins
    */
  def scoreAgainst(others: Vector[Card], trump: Suit, current: Suit): Int =
    val noWizardInPlay = !others.exists(_.isWizard)
    val isCurrentSuit = this.suit == current
    val isTrumpSuit = this.suit == trump
    val beatsOthersNormal = others.forall(c => 
          (c.suit != trump || c.value == 1) && (
          c.suit != this.suit  || 
          c.value < this.value ))
    val beatsOthersTrump = this.suit == trump && others.forall(c => 
          c.suit != this.suit  || 
          c.value < this.value)
    // This formulates the basic rule set for which card beats which
    if  noWizardInPlay && (isCurrentSuit && beatsOthersNormal || isTrumpSuit && beatsOthersTrump) then 1 else 0
