package apps.app64

import scala.annotation.tailrec
import cs214.webapp.*
object State:
  /**
    * Default state to which any game is reset upon initialisation
    *
    * @param clients All clients that requested to join the game
    * @return The initial state of the game
    */
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

/**
  * The game state that contains and maintains the current game
  *
  * @param players All players that are part of the game (2-8)
  * @param stakes The current stake of each player
  * @param cardsPlayed The cards that were played in this trick, up to this point, in order of play
  * @param hands The current hand for each player
  * @param scores The current score for each player
  * @param trumpSuit The suit that is trump for this trick/play
  * @param currentSuit The suit that was played first in this trick/play
  * @param round The current round number
  * @param phase The current game phase
  * @param trickWinner The player that won the last trick, empty string if there is no winner
  */
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
  /** Determines if all players put in their wager */
  lazy val allReady = players.forall(stakes.keySet.contains)
  
  /**
    * Method to place a bid for a user
    *
    * @param id Id of the user
    * @param bid Amount of tricks to be bid
    * @return A new state where the bid was associated to the given user
      *         and it is the next players turn to bid.
    */
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
  
    /**
      * Method to play a card for a user.
      * Requires that it is the users turn and the card is in that users hand.
      * @param id Id of the user in question
      * @param card Card to be played (Always valid)
      * @return A new state where the card was played for the user, 
      *         and it is the next players turn to play.
      */
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
  /** Method to switch phase, @returns a copy of the state with the phase changed */
  def nextPhase(phase: Phase): State =
    copy(phase = phase)

  /** Method to deal new cards, @returns a copy of the state with cards dealt */
  def withNewCards: State =
    copy(
        hands = Deck.dealNCards(round, players)
      )

  /** Method go to the next player, @returns a copy of the state with the next player to play */
  def nextPlayer: State =
    copy(players = players.tail :+ players.head)
  /**
    * Method to make a given player be next in turn
    *
    * @param user Id of the user that is next
    * @return A state where the players vector was sufficiently rotateted
    */
  def withPlayerNext(user: UserId): State = {
    require(
      players.contains(user),
      s"players = $players does not contain user = $user"
    )
    @tailrec
    def rotatePlayers(p: Vector[UserId], targetPlayer: UserId): Vector[UserId] =
      if p.head == targetPlayer then p else rotatePlayers(p.tail :+ p.head, targetPlayer)
    copy(players = rotatePlayers(players, user))
  } ensuring(res => res.players.size == players.size && res.players.forall(p => players.contains(p)))

  private def computeTricks: Map[UserId, Stake]  = {
    require(
      phase == Phase.PlayEnd,
      s"phase = $phase is not PlayEnd"
    )
    (for
      (player, card) <- cardsPlayed
      otherCards = cardsPlayed.filter(_ != (player, card)).map(_._2)
    yield
      if !(card.isWizard || card.isJester) then {
        player -> (
          stakes(player).copy(
            (stakes(player).tricksWon + card.scoreAgainst(otherCards, trumpSuit, currentSuit)),stakes(player).bid) 
        )}
      else if card.isJester then
        player -> (
          stakes(player).copy(
            (stakes(player).tricksWon + (if otherCards.forall(_.isJester) && (cardsPlayed.head._1 == player) then 1 else 0))
          ,stakes(player).bid)
      )
      else 
        val cardIndex = cardsPlayed.indexOf((player, card))
        player -> (
          stakes(player).copy(
            (stakes(player).tricksWon + (if cardsPlayed.take(cardIndex).exists((_, c) => c.isWizard) then 0 else 1))
          ,stakes(player).bid)
      )
  ).toMap } ensuring(res => res.exists((u, s) => (s.tricksWon - stakes(u).tricksWon) > 0))

  /** Method to compute the winner of a play/trick, @returns a state with updated stakes and a winner */
  def computeWinner: State =
    val nextStakes = computeTricks
    val winner = nextStakes.filter((u, s) => (s.tricksWon - stakes(u).tricksWon) > 0).head._1
    copy(
      stakes = nextStakes,
      trickWinner = winner
    )

  /** Method that @returns an updated state, ready for the next play */
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
    * @return new state that is ready for the bid phase
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

  /** Method that @returns a new state which is ready for game end */
  def gameEnded: State = 
    require(
      this.phase == Phase.GameEnd,
      s"phase = ${this.phase} is not GameEnd"
    )

    copy(
      stakes = Map(),
      scores = updateScores(),
      cardsPlayed = Vector(),
      hands = hands.map((u, h) => (u, h.empty)), 
      currentSuit = Suit.None,
    )

  private def highestCurrentCard(suit: Suit) = cardsPlayed.map(_._2)
      .filter(_.suit == suit)
      .filterNot(c => c.isWizard || c.isJester)
      .map(_.value)
      .maxOption

  private def isValid(card: Card): Boolean =
    (card.suit == currentSuit && card.value > highestCurrentCard(currentSuit).getOrElse(0)) || 
    (card.suit == trumpSuit && card.value > highestCurrentCard(trumpSuit).getOrElse(0)) || 
    currentSuit == Suit.None || 
    card.isJester || 
    card.isWizard

  /**
    * Method to determine which cards of a given user a valid to be played
    *
    * @param userId Id of the user
    * @return A set where every card is paired with a boolean indicating whether it is valid to be played
    */
  def getValidHand(userId: UserId): Set[(Card, Boolean)] = 
    val validCards = (for
        card <- hands(userId)
      yield
        (card, isValid(card)))
    if !validCards.exists((card, valid)
        => valid && card.suit == currentSuit && !(card.isWizard || card.isJester)
      ) then
      hands(userId).map((_, true))
    else validCards

  /** Cosmetic function to avoid having to look at hands(key).isEmpty, @returns whether hand of user is empty */
  def isHandEmpty(id: UserId): Boolean =
    hands(id).isEmpty
