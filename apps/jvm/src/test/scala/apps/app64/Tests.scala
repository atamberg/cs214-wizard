package apps.app64

import cs214.webapp.*
import cs214.webapp.utils.WebappSuite
import cs214.webapp.Action
import cs214.webapp.server.StateMachine

import Wire.*

class Tests extends WebappSuite[Event, State, View]:
  override val sm: StateMachine[Event, State, View] = Logic()
  // Example hands for testing
  val hand0 = Set(Card(Suit.Clubs, 2), Card(Suit.Diamonds, 8))
  val hand1 = Set(Card(Suit.Hearts, 10), Card(Suit.Diamonds, 5))
  val hand2 = Set(Card(Suit.Clubs, 7), Card(Suit.Spades, 2))
  val stake0 = Stake(3, 0)
  val stake1 = Stake(3, 4)
  val stake2 = Stake(5, 6)

  val stakeView1 = Map(USER_IDS(0) -> stake0, USER_IDS(1) -> stake1, USER_IDS(2) -> stake2)
  val scoreView1 = Map(USER_IDS(0) -> 2, USER_IDS(1) -> 3, USER_IDS(2) -> 1)

  test("Different views are not equal (0pt)") {
    val v1 = View(PhaseView.Waiting(Map(USER_IDS(0) -> false)), Map(UID0 -> 10))
    val v2 = View(PhaseView.Waiting(Map(USER_IDS(0) -> true)), Map(UID0 -> 10))
    assert(v1 != v2)
  }

  test("Different events are not equal") {
    val e1 = Event.AnnounceBet(10)
    val e2 = Event.AnnounceBet(20)
    assert(e1 != e2)
  }

  test("Event wire") {
    val events = List(
      Event.AnnounceBet(10),
      Event.AnnounceBet(20),
      Event.PlayCard(Card(Suit.Hearts, 5))
    )
    import eventFormat.*
    for event <- events do
      assertEquals(decode(encode(event)).get, event)
  }

  test("View wire") {
    val views = List(
      View(PhaseView.Waiting(Map(USER_IDS(0) -> false)), scoreView1),
      View(PhaseView.CardSelecting(hand1, stakeView1), scoreView1),
      View(PhaseView.CardSelecting(hand2, stakeView1), scoreView1)
    )
    import viewFormat.*
    views.foreach { view =>
      assertEquals(decode(encode(view)).get, view)
    }
  }

