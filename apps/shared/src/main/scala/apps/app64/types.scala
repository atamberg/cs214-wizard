package apps.app64

import cs214.webapp.UserId
import upickle.default.*

/**
  * A Hand is simply a set of Cards.
  */
type Hand = Set[Card]


/**
  * Phase is an enum which contains types of phases
  * which are used to signal the state:
    - Bid:      accept player bids
    - Play:     allow cards to be played
    - PlayEnd:  display details regarding the trick ending
    - RoundEnd: display details regarding the round ending
    - GameEnd:  display details regarding the game  ending
  */
enum Phase:
  case Bid, Play, PlayEnd, RoundEnd, GameEnd


/**
  * Event is an enum which describes possible
  * events the user can send to the state, which
  * in turn sends back actions.
  * Deriving ReadWriter from upickle allows enums to easily
  * integrate with the upickle library. In other words,
  * deriving contextually describes upickle and the enum,
  * allowing upickles's readers and writers to operate.
  */
enum Event derives ReadWriter:

  /**
    * An event where a player announces a bid.
    *
    * @param bid The bid value announced by the player.
    */
  case AnnounceBid(bid: Int)

  /**
    * An event where a player plays a card.
    *
    * @param card The card played by the player.
    */
  case PlayCard(card: Card)


/**
  * Represents the overall view of the state as presented to a user.
  *
  * @param phaseView The current phase of the game, represented by a PhaseView.
  * @param scoreView A mapping of each user to their current score.
  * @param stateView A detailed view of the state, including players, stakes, and played cards.
  */
case class View (
  phaseView: PhaseView,
  scoreView: Map[UserId, Int],
  stateView: StateView
) derives ReadWriter


/**
  * Represents the state of the game, including player-specific
  * details and the current round status. This proves useful in
  * the frontend as it packages useful data in a safe manner
  * (no user hands or incriminating data)
  *
  * @param players     A vector of users representing the players in the game.
  * @param stakes      A mapping of each user to their current state.
  * @param cardsPlayed A vector of tuples, where each tuple contains a user and the card they played.
  * @param trumpSuit   The trump suit of the game.
  * @param currentSuit The current suit being played in the ongoing round.
  * @param round       The current round number.
  * @param trickWinner The user Id of the player who won the latest trick.
  */
case class StateView(
  players:     Vector[UserId],
  stakes:      Map[UserId, Stake],
  cardsPlayed: Vector[(UserId, Card)],
  trumpSuit:   Suit,
  currentSuit: Suit,
  round:       Int,
  trickWinner: UserId
) derives ReadWriter


/**
  * Enumerates the possible phases of the game, capturing the
  * actions and states available in each phase.
  */
enum PhaseView derives ReadWriter:
/**
    * The phase where players select cards,
    * indicating valid cards and whether they can be played.
    *
    * @param validHand A set of tuples where each tuple contains
    *                  a Card and a boolean indicating its validity.
    */
  case CardSelecting(validHand: Set[(Card, Boolean)])

  /**
    * The phase where players place their bids.
    *
    * @param hand The Hand of the player making the bid.
    */
  case BidSelecting(hand: Hand)

  /**
    * A waiting phase where players await their turn or the resolution of actions.
    *
    * @param hand The Hand of the player waiting.
    */
  case Waiting(hand: Hand)

  /**
    * The phase at the end of a play where the winner of the trick is determined.
    *
    * @param hand        The Hand of the player.
    * @param trickWinner The UserId of the player who won the trick.
    */
  case PlayEnding(hand: Hand, trickWinner: UserId)

  /**
    * The phase marking the end of a round.
    */
  case RoundEnding

  /**
    * The phase marking the end of the game.
    */
  case GameEnding
