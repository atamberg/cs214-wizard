package apps.app64

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import upickle.default.*
import cs214.webapp.server.StateMachine

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
    card.toString shouldBe "10 of Hearts"
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

  // Test State Creation and Manipulation
  "State" should "create initial game state correctly" in {
    val players = Vector("player1", "player2")
    val initialState = State(
      players = players,
      stakes = players.map(p => (p, Stake(0, 0))).toMap,
      cardsPlayed = players.map(p => (p, Card(Suit.None, 0))),
      hands = players.map(p => (p, Set.empty[Card])).toMap,
      scores = players.map(p => (p, 0)).toMap,
      trumpSuit = Suit.Hearts,
      currentSuit = Suit.Hearts,
      round = 1,
      phase = Phase.Bid
    )

    initialState.players.size shouldBe 2
    initialState.phase shouldBe Phase.Bid
    initialState.round shouldBe 1
  }

  // Test View Serialization
  "View" should "serialize and deserialize correctly" in {
    val hand = Set(Card(Suit.Hearts, 10), Card(Suit.Spades, 5))
    val stakes = Map(
      "player1" -> Stake(2, 1),
      "player2" -> Stake(3, 2)
    )

    val view = View(
      phaseView = PhaseView.CardSelecting(hand, stakes),
      scoreView = Map("player1" -> 10, "player2" -> 15)
    )

    val written = write(view)
    val valueRead = read[View](written)
    valueRead shouldBe view
  }

  // Test PhaseView Transitions
  "PhaseView" should "support different phase views" in {
    val cardSelecting = PhaseView.CardSelecting(
      Set(Card(Suit.Hearts, 10)),
      Map("player1" -> Stake(1, 1))
    )
    
    val betSelecting = PhaseView.BidSelecting(3)
    
    val waiting = PhaseView.Waiting(
      Map(
        "player1" -> true, 
        "player2" -> false
      )
    )

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


  "Different views" should "not be equal" in {
    val v1 = View(PhaseView.Waiting(Map(USER_IDS(0) -> false)), Map(UID0 -> 10))
    val v2 = View(PhaseView.Waiting(Map(USER_IDS(0) -> true)), Map(UID0 -> 10))
    v1 should not be v2
  }

  "Different events" should "not be equal" in {
    val e1 = Event.AnnounceBid(10)
    val e2 = Event.AnnounceBid(20)
    e1 should not be e2
  }

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
      View(PhaseView.Waiting(Map(USER_IDS(0) -> false)), scoreView1),
      View(PhaseView.CardSelecting(hand1, stakeView1), scoreView1),
      View(PhaseView.CardSelecting(hand2, stakeView1), scoreView1)
    )
    import apps.app64.Wire.viewFormat._
    views.foreach { view =>
      decode(encode(view)).get shouldBe view
    }
  }
end GameStateTest

