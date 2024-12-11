
package apps.app64

import cs214.webapp.*
import cs214.webapp.server.StateMachine
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Try, Success, Failure}
import apps.app64.Wire.*

class SerialisationTest extends AnyFlatSpec with Matchers {
  
  val UID0: String = "yak"
  val UID1: String = "hut"
  val UID2: String = "kik"

  /** Mock user IDs that can be used in tests */
  val USER_IDS = Vector(UID0, UID1, UID2)

  // Helper method to create a default initial state
  private def createInitialState(playerIds: Vector[UserId]): State = {
    State.defaultState(playerIds).withNewCards
  }
  private def createRoundNState(playerIds: Vector[UserId], n: Int): State = {
    State.defaultState(playerIds).copy(
      round = n,
      )
  }

  "View" should "properly encode/decode hands" in {
    val hand0 = Set(Card(Suit.Clubs, 1), Card(Suit.Diamonds, 1))
    val state0 = createRoundNState(USER_IDS, 2).withNewCards
    val testState = state0.copy(
        hands = state0.hands.map((p, h) => if p == UID0 then (p, hand0) else (p, h))
      )
    val testView = View(
      PhaseView.BidSelecting(hand0), 
      testState.scores,
      StateView(testState.players, testState.stakes, testState.cardsPlayed, testState.trumpSuit, testState.currentSuit, testState.round, testState.trickWinner),
    )
    val pickled = viewFormat.encode(testView)
    val wired: Hand = viewFormat.decode(pickled).get.phaseView match
      case PhaseView.BidSelecting(h) => h
      case _ => throw new IllegalArgumentException()

    wired shouldBe hand0
    
    val hand1 = Set(Card(Suit.Clubs, 1), Card(Suit.Spades, 5), Card(Suit.Diamonds, 1))
    val state1 = createRoundNState(USER_IDS, 3).withNewCards
    val testState1 = state1.copy(
        hands = state1.hands.map((p, h) => if p == UID1 then (p, hand0) else (p, h))
      )
    val testView1 = View(
      PhaseView.BidSelecting(hand1), 
      testState.scores,
      StateView(testState1.players, testState1.stakes, testState1.cardsPlayed, testState1.trumpSuit, testState1.currentSuit, testState1.round, testState1.trickWinner),
    )
    val pickled1 = viewFormat.encode(testView1)
    val wired1: Hand = viewFormat.decode(pickled1).get.phaseView match
      case PhaseView.BidSelecting(h) => h
      case _ => throw new IllegalArgumentException()

    wired1 shouldBe hand1
  }


  }
