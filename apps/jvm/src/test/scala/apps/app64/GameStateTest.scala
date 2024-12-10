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
        0))

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
    state1.round
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

end GameStateTest

