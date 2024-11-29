package apps.app64

import cs214.webapp.*
import cs214.webapp.utils.WebappSuite
import cs214.webapp.server.StateMachine

class Tests extends WebappSuite[Event, State, View]:
  override val sm: StateMachine[Event, State, View] = Logic()


import org.scalatest.funsuite.AnyFunSuite
import ujson._
import upickle.default._

class SerializationTest extends AnyFunSuite {

  // Sample UserIds for testing
  val UID0 = UserId("user0")
  val UID1 = UserId("user1")

  // Example hands for testing
  val hand1 = Set(Card(Suit.Hearts, 10), Card(Suit.Diamonds, 5))
  val hand2 = Set(Card(Suit.Clubs, 7), Card(Suit.Spades, 2))

  test("Different views are not equal (0pt)") {
    val v1 = View(PhaseView.Waiting(Map(UID0 -> false)), Map(UID0 -> 10))
    val v2 = View(PhaseView.Waiting(Map(UID0 -> true)), Map(UID0 -> 10))
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
    events.foreach { event =>
      // Serialize and deserialize the event, then check equality
      val json = write(event)
      val parsedEvent = read[Event](json)
      assert(event == parsedEvent)
    }
  }

  test("View wire") {
    val views = List(
      View(PhaseView.Waiting(Map(UID0 -> false)), Map(UID0 -> 10)),
      View(PhaseView.CardSelecting(hand1), Map(UID0 -> 20)),
      View(PhaseView.CardSelecting(hand2), Map(UID1 -> 15))
    )
    views.foreach { view =>
      // Serialize and deserialize the view, then check equality
      val json = write(view)
      val parsedView = read[View](json)
      assert(view == parsedView)
    }
  }
}
