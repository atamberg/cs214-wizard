package apps.app64

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import upickle.default.*
import cs214.webapp.server.StateMachine
import cs214.webapp.UserId

class GameStateTest extends AnyFlatSpec with Matchers:
  // Test Suit Enum Conversions
  "Suit" should "be able to derive ReadWriter" in {
    val suits = Seq(Suit.Hearts, Suit.Diamonds, Suit.Clubs, Suit.Spades)
    suits.foreach { suit =>
      val written = write(suit)
      val valueRead = read[Suit](written)
      valueRead shouldBe suit
    }
  }

  "Card" should "have a correct toString representation" in {
    val card = Card(Suit.Hearts, 10)
    card.toString shouldBe "🂺"
  }

  // Test Event Serialization
  "Event" should "serialize and deserialize AnnounceBet" in {
    val betEvent = Event.AnnounceBid(5)
    val written = write(betEvent)
    val valueRead = read[Event](written)
    valueRead shouldBe betEvent
  }

  it should "serialize and deserialize PlayCard" in {
    val card = Card(Suit.Spades, 7)
    val playEvent = Event.PlayCard(card)
    val written = write(playEvent)
    val valueRead = read[Event](written)
    valueRead shouldBe playEvent
  }


  // Test View Serialization
  "View" should "serialize and deserialize correctly" in {
    val validHand = for c <- Set(Card(Suit.Hearts, 10), Card(Suit.Spades, 5)) yield (c, true)
    val stakes = Map(
      "player1" -> Stake(2, 1),
      "player2" -> Stake(3, 2)
    )

    val view = View(
      phaseView = PhaseView.CardSelecting(validHand),
      scoreView = Map("player1" -> 10, "player2" -> 15),
      stateView = StateView(
        Vector("player1", "player2"),
        Map(),
        Vector(),
        Suit.None,
        Suit.None,
        0,
        "player1"
      ))

    val written = write(view)
    val valueRead = read[View](written)
    valueRead shouldBe view
  }

  // Test PhaseView Transitions
  "PhaseView" should "support different phase views" in {
    val cardSelecting = PhaseView.CardSelecting(
      Set((Card(Suit.Hearts, 10), false))
    )
    
    val betSelecting = PhaseView.BidSelecting(Set(Card(Suit.Hearts, 10)))
    
    val waiting = PhaseView.Waiting(Set(Card(Suit.Hearts, 10)))

    // Verify serialization for each phase view
    Seq(cardSelecting, betSelecting, waiting).foreach { phaseView =>
      val written = write(phaseView)
      val valueRead = read[PhaseView](written)
      valueRead shouldBe phaseView
    }
  }

  val sm: StateMachine[Event, State, View] = Logic()
  val UID0: String = "yak"
  val UID1: String = "hut"
  val UID2: String = "kik"

  /** Mock user IDs that can be used in tests */
  val USER_IDS = Seq(UID0, UID1, UID2)
  val hand0 = Set(Card(Suit.Clubs, 2), Card(Suit.Diamonds, 8))
  val hand1 = Set(Card(Suit.Hearts, 10), Card(Suit.Diamonds, 5))
  val hand2 = Set(Card(Suit.Clubs, 7), Card(Suit.Spades, 2))
  val stake0 = Stake(3, 0)
  val stake1 = Stake(3, 4)
  val stake2 = Stake(5, 6)
  val stakeView1 = Map(USER_IDS(0) -> stake0, USER_IDS(1) -> stake1, USER_IDS(2) -> stake2)
  val scoreView1 = Map(USER_IDS(0) -> 2, USER_IDS(1) -> 3, USER_IDS(2) -> 1)
  val state1 = State.defaultState(USER_IDS)
  val stateView1 = StateView(
    USER_IDS.toVector, 
    state1.stakes,
    state1.cardsPlayed,
    state1.trumpSuit,
    state1.currentSuit,
    state1.round,
    state1.trickWinner
    )


  "Event wire" should "work correctly" in {
    val events = List(
      Event.AnnounceBid(10),
      Event.AnnounceBid(20),
      Event.PlayCard(Card(Suit.Hearts, 5))
    )
    import apps.app64.Wire.eventFormat._
    for (event <- events) {
      decode(encode(event)).get shouldBe event
    }
  }

  "View wire" should "work correctly" in {
    val views = List(
      View(PhaseView.Waiting(hand0), scoreView1, stateView1),
      View(PhaseView.CardSelecting(hand1.map(c => (c, c.value % 2 == 0))), scoreView1, stateView1),
      View(PhaseView.CardSelecting(hand0.map(c => (c, c.value % 2 == 0))), scoreView1, stateView1)
    )
    import apps.app64.Wire.viewFormat._
    views.foreach { view =>
      decode(encode(view)).get shouldBe view
    }
  }

  "Stake" should "score as intended" in {
    stake0.score shouldBe -30
    stake1.score shouldBe -10
    stake2.score shouldBe -10
  }
  //-------withPlayerNext--------
  val UID3: String = "zap"
  val USER_IDS_4 = Seq(UID0, UID1, UID2, UID3)

  "withPlayerNext" should "rotate to the specified player when the player is first in current order" in {
    val state = State.defaultState(USER_IDS)
    val result = state.withPlayerNext(UID0)
    result.players shouldBe Vector(UID0, UID1, UID2)
  }

  it should "rotate to the specified player when the player is in the middle of the current order" in {
    val state = State.defaultState(USER_IDS)
    val result = state.withPlayerNext(UID1)
    result.players shouldBe Vector(UID1, UID2, UID0)
  }

  it should "rotate to the specified player when the player is last in the current order" in {
    val state = State.defaultState(USER_IDS)
    val result = state.withPlayerNext(UID2)
    result.players shouldBe Vector(UID2, UID0, UID1)
  }

  it should "work with a 4-player game, rotating to a player in different positions" in {
    val state = State.defaultState(USER_IDS_4)
    
    // Rotate to first player
    val resultFirst = state.withPlayerNext(UID0)
    resultFirst.players shouldBe Vector(UID0, UID1, UID2, UID3)

    // Rotate to second player
    val resultSecond = state.withPlayerNext(UID1)
    resultSecond.players shouldBe Vector(UID1, UID2, UID3, UID0)

    // Rotate to third player
    val resultThird = state.withPlayerNext(UID2)
    resultThird.players shouldBe Vector(UID2, UID3, UID0, UID1)

    // Rotate to last player
    val resultLast = state.withPlayerNext(UID3)
    resultLast.players shouldBe Vector(UID3, UID0, UID1, UID2)
  }

  it should "throw an exception when trying to rotate to a player not in the game" in {
    val state = State.defaultState(USER_IDS)
    
    an [IllegalArgumentException] should be thrownBy {
      state.withPlayerNext("non-existent-player")
    }
  }

  it should "preserve all original players when rotating" in {
    val state = State.defaultState(USER_IDS)
    
    val resultFirst = state.withPlayerNext(UID0)
    resultFirst.players shouldBe Vector(UID0, UID1, UID2)

    val resultMiddle = state.withPlayerNext(UID1)
    resultMiddle.players shouldBe Vector(UID1, UID2, UID0)

    val resultLast = state.withPlayerNext(UID2)
    resultLast.players shouldBe Vector(UID2, UID0, UID1)
  }

  def createTestState(
    currentSuit: Suit = Suit.None, 
    trumpSuit: Suit = Suit.Spades, 
    cardsPlayed: Vector[(String, Card)] = Vector(),
    hands: Map[String, Set[Card]] = Map()
  ): State = 
    State.defaultState(USER_IDS).copy(
      currentSuit = currentSuit,
      trumpSuit = trumpSuit,
      cardsPlayed = cardsPlayed,
      hands = hands.withDefaultValue(Set())
    )

  // Some sample cards
  val heartTwo = Card(Suit.Hearts, 2)
  val heartTen = Card(Suit.Hearts, 10)
  val diamondFive = Card(Suit.Diamonds, 5)
  val spadesSeven = Card(Suit.Spades, 7)
  val clubsJack = Card(Suit.Clubs, 11)
  val wizardCard = Card(Suit.None, 15)
  val jesterCard = Card(Suit.None, 1)

  "getValidHand" should "return all cards as valid when no cards have been played" in {
    val hand = Set(heartTwo, diamondFive, spadesSeven)
    val state = createTestState(hands = Map(UID0 -> hand))
    
    val validHand = state.getValidHand(UID0)
    
    validHand shouldBe hand.map((_, true))
  }

  it should "return all cards as valid when no current suit is set" in {
    val hand = Set(heartTwo, diamondFive, spadesSeven)
    val state = createTestState(
      currentSuit = Suit.None, 
      cardsPlayed = Vector(UID1 -> heartTen),
      hands = Map(UID0 -> hand)
    )
    
    val validHand = state.getValidHand(UID0)
    
    validHand shouldBe hand.map((_, true))
  }

  it should "validate cards correctly when current suit is set" in {
    val hand = Set(
      heartTwo,     // matching current suit
      diamondFive,  // non-matching suit
      spadesSeven,  // non-matching suit
      heartTen      // higher value in current suit
    )
    val state = createTestState(
      currentSuit = Suit.Hearts,
      cardsPlayed = Vector(UID1 -> Card(Suit.Hearts, 5)),
      hands = Map(UID0 -> hand)
    )
    
    val validHand = state.getValidHand(UID0)
    
    validHand shouldBe Set(
      (heartTwo, false),     // lower than played card
      (diamondFive, false),   // different suit
      (spadesSeven, true),   // different suit
      (heartTen, true)       // higher than played card
    )
  }

  it should "handle trump suit validation correctly" in {
    val hand = Set(
      heartTwo,     // non-trump, non-current suit
      diamondFive,  // trump suit
      spadesSeven   // current suit
    )
    val state = createTestState(
      currentSuit = Suit.Spades,
      trumpSuit = Suit.Diamonds,
      cardsPlayed = Vector(
        UID1 -> Card(Suit.Spades, 6),
        UID2 -> Card(Suit.Diamonds, 8)
      ),
      hands = Map(UID0 -> hand)
    )
    
    val validHand = state.getValidHand(UID0)
    
    validHand shouldBe Set(
      (heartTwo, false),      // different suit
      (diamondFive, false),   // lower trump
      (spadesSeven, true)   // higher current suit card
    )
  }

  it should "always allow wizard and jester cards" in {
    val hand = Set(
      heartTwo,     // normal card
      wizardCard,   // wizard
      jesterCard    // jester
    )
    val state = createTestState(
      currentSuit = Suit.Hearts,
      cardsPlayed = Vector(UID1 -> Card(Suit.Hearts, 10)),
      hands = Map(UID0 -> hand)
    )
    
    val validHand = state.getValidHand(UID0)
    
    validHand shouldBe Set(
      (heartTwo, true),     // can be played because no other card in the hand has the current suit and is valid
      (wizardCard, true),    // wizard always valid
      (jesterCard, true)     // jester always valid
    )
  }

  it should "work with multiple cards played" in {
    val hand = Set(
      heartTwo,
      heartTen,
      diamondFive
    )
    val state = createTestState(
      currentSuit = Suit.Hearts,
      cardsPlayed = Vector(
        UID1 -> Card(Suit.Hearts, 5),
        UID2 -> Card(Suit.Hearts, 7)
      ),
      hands = Map(UID0 -> hand)
    )
    val validHand = state.getValidHand(UID0)
    
    validHand shouldBe Set(
      (heartTwo, false),     // lower than played cards
      (heartTen, true),      // higher than played cards
      (diamondFive, false)   // different suit, can play heart 10
    )
  }

  it should "handle empty hand correctly" in {
    val state = createTestState(
      hands = Map(UID0 -> Set())
    )
    
    val validHand = state.getValidHand(UID0)
    
    validHand shouldBe Set()
  }


  def createTestStateForNextPlay(
  currentSuit: Suit = Suit.None, 
  trumpSuit: Suit = Suit.None, 
  cardsPlayed: Vector[(UserId, Card)] = Vector.empty,
  players: Vector[UserId] = Vector(UID0, UID1, UID2),
): State = 
  State.defaultState(USER_IDS).copy(
    players = players,
    cardsPlayed = cardsPlayed,
    trumpSuit = trumpSuit,
    currentSuit = currentSuit,
    phase = Phase.PlayEnd,
  )

  "nextPlay" should "determine a winner and update stakes correctly" in {
    val initialStakes = Map(
      UID0 -> Stake(0, 3),
      UID1 -> Stake(0, 2),
      UID2 -> Stake(0, 4)
    )
    
    val s1 = createTestStateForNextPlay(
      currentSuit = Suit.Hearts,
      trumpSuit = Suit.Diamonds,
      cardsPlayed = Vector(
        (UID0, Card(Suit.Hearts, 6)), 
        (UID1, Card(Suit.Diamonds, 7)), 
        (UID2, Card(Suit.Hearts, 11))
      )
    ).copy(
      stakes = initialStakes,
      phase = Phase.PlayEnd
    )
    val winnerState = s1.computeWinner
    val nextState = winnerState.nextPlay
    // Verify the winner (UID2) gets their tricks won incremented
    nextState.stakes(UID2).tricksWon shouldBe 0
    nextState.stakes(UID0).tricksWon shouldBe 0
    nextState.stakes(UID1).tricksWon shouldBe 1
    
    // Verify other state changes
    nextState.phase shouldBe Phase.Play
    nextState.cardsPlayed shouldBe empty
    nextState.currentSuit shouldBe Suit.None
    nextState.players.head shouldBe UID1  // Winner becomes first player
  }

  // Additional tests for nextPlay method
  it should "handle complex card interactions with wizards and trump suit" in {
    val initialStakes = Map(
      UID0 -> Stake(0, 3),
      UID1 -> Stake(0, 2),
      UID2 -> Stake(0, 4)
    )
    
    val s1 = createTestStateForNextPlay(
      currentSuit = Suit.Hearts,
      trumpSuit = Suit.Diamonds,
      cardsPlayed = Vector(
        (UID0, Card(Suit.Hearts, 10)),     // Normal heart card 
        (UID1, Card(Suit.None, 15)),       // Wizard 
        (UID2, Card(Suit.Diamonds, 5))     // Lower trump card
      )
    ).copy(
      stakes = initialStakes,
      phase = Phase.PlayEnd
    )
    
    val winnerState = s1.computeWinner
    val nextState = winnerState.nextPlay
    
    // Wizard beats all other cards, so UID1 should win
    nextState.stakes(UID1).tricksWon shouldBe 1
    nextState.stakes(UID0).tricksWon shouldBe 0
    nextState.stakes(UID2).tricksWon shouldBe 0
    
    nextState.phase shouldBe Phase.Play
    nextState.cardsPlayed shouldBe empty
    nextState.currentSuit shouldBe Suit.None
    nextState.players.head shouldBe UID1  // Wizard player becomes first player
  }

  it should "handle a scenario with multiple special cards" in {
    val initialStakes = Map(
      UID0 -> Stake(0, 3),
      UID1 -> Stake(0, 2),
      UID2 -> Stake(0, 4)
    )
    
    val s1 = createTestStateForNextPlay(
      currentSuit = Suit.Hearts,
      trumpSuit = Suit.Spades,
      cardsPlayed = Vector(
        (UID0, Card(Suit.None, 1)),        // Jester 
        (UID1, Card(Suit.Spades, 10)),     // Trump card
        (UID2, Card(Suit.None, 15))        // Wizard
      )
    ).copy(
      stakes = initialStakes,
      phase = Phase.PlayEnd
    )
    
    val winnerState = s1.computeWinner
    val nextState = winnerState.nextPlay
    
    // Wizard beats all, so UID2 should win
    nextState.stakes(UID2).tricksWon shouldBe 1
    nextState.stakes(UID0).tricksWon shouldBe 0
    nextState.stakes(UID1).tricksWon shouldBe 0
    
    nextState.phase shouldBe Phase.Play
    nextState.cardsPlayed shouldBe empty
    nextState.currentSuit shouldBe Suit.None
    nextState.players.head shouldBe UID2  // Wizard player becomes first player
  }

  it should "handle a scenario with no clear winner" in {
    val initialStakes = Map(
      UID0 -> Stake(0, 3),
      UID1 -> Stake(0, 2),
      UID2 -> Stake(0, 4)
    )
    
    val s1 = createTestStateForNextPlay(
      currentSuit = Suit.Hearts,
      trumpSuit = Suit.Hearts,
      cardsPlayed = Vector(
        (UID0, Card(Suit.None, 1)),        // Jester 
        (UID1, Card(Suit.None, 1)),        // Another Jester
        (UID2, Card(Suit.None, 1))         // Yet another Jester
      )
    ).copy(
      stakes = initialStakes,
      phase = Phase.PlayEnd
    )
    
    val winnerState = s1.computeWinner
    val nextState = winnerState.nextPlay

    // When all cards are Jesters, the first player wins
    nextState.stakes(UID0).tricksWon shouldBe 1
    nextState.stakes(UID1).tricksWon shouldBe 0
    nextState.stakes(UID2).tricksWon shouldBe 0
    
    nextState.phase shouldBe Phase.Play
    nextState.cardsPlayed shouldBe empty
    nextState.currentSuit shouldBe Suit.None
    nextState.players.head shouldBe UID0  // First player becomes first player
  }

end GameStateTest
