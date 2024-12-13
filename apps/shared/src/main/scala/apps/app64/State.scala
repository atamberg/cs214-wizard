package apps.app64

import cs214.webapp.*

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

  private def isValid(card: Card): Boolean =
    val highestCurrentSuit = cardsPlayed.map(_._2)
      .filter(_.suit == currentSuit)
      .filterNot(c => c.isWizard || c.isJester)
      .map(_.value)
      .maxOption

    val highestCurrentTrump = cardsPlayed.map(_._2)
      .filter(_.suit == trumpSuit)
      .filterNot(c => c.isWizard || c.isJester)
      .map(_.value)
      .maxOption

    (card.suit == currentSuit && card.value > highestCurrentSuit.getOrElse(0))
    || (card.suit == trumpSuit && card.value > highestCurrentTrump.getOrElse(0))
    || currentSuit == Suit.None
    || card.isJester
    || card.isWizard
  end isValid

  def getValidHand(userId: UserId): Set[(Card, Boolean)] = 
    val validCards = (for
        card <- hands(userId)
      yield
        (card, isValid(card)))
    if !validCards.exists((card, valid) => valid && card.suit == currentSuit && (card.isWizard || card.isJester)) then
      hands(userId).map((_, true))
    else validCards
  end getValidHand

  def isHandEmpty(id: UserId): Boolean =
    hands(id).isEmpty
