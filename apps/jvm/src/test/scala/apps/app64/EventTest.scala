package apps.app64

import cs214.webapp.*
import cs214.webapp.server.StateMachine
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Try, Success, Failure}

class EventTest extends AnyFlatSpec with Matchers {
  
  val UID0: String = "yak"
  val UID1: String = "hut"
  val UID2: String = "kik"

  /** Mock user IDs that can be used in tests */
  val USER_IDS = Vector(UID0, UID1, UID2)

  // Helper method to create a default initial state
  private def createInitialState(playerIds: Vector[UserId]): State = {
    State.defaultState(playerIds).withNewCards
  }

  // Test bidding phase transitions
  "Logic" should "allow bidding during Bid phase" in {
    val playerIds = USER_IDS
    val initialState = createInitialState(playerIds)
    val logic = new Logic()

    val bidEvent = Event.AnnounceBid(2)
    val result = logic.transition(initialState)(UID0, bidEvent)

    result shouldBe a [Success[?]]
    val actions = result.get
    val renderAction = actions.head.asInstanceOf[Action.Render[State]]
    renderAction.st.stakes.keySet should contain(UID0)
    renderAction.st.stakes(UID0).bid shouldBe 2
  }

  it should "not allow playing a card during Bid phase" in {
    val playerIds = USER_IDS
    val initialState = createInitialState(playerIds)
    val logic = new Logic()

    val playCardEvent = Event.PlayCard(initialState.hands(UID0).head)
    val result = logic.transition(initialState)(UID0, playCardEvent)

    result shouldBe a [Failure[?]]
    result.failed.get shouldBe an[Exception]
  }

  it should "not allow multiple bids from the same player" in {
    val playerIds = USER_IDS
    val initialState = createInitialState(playerIds)
    val logic = new Logic()

    // First bid
    val firstBidEvent = Event.AnnounceBid(2)
    val firstResult = logic.transition(initialState)(UID0, firstBidEvent)
    firstResult shouldBe a [Success[?]]

    // Try to bid again
    val secondBidEvent = Event.AnnounceBid(3)
    val secondResult = logic.transition(
      firstResult.get.head.asInstanceOf[Action.Render[State]].st // basically state after first guy bids
    )(UID0, secondBidEvent)
    
    secondResult shouldBe a [Failure[?]]
    secondResult.failed.get shouldBe an[Exception] // Any exception (IAE or IME)
  }

  // Test play phase transitions
  it should "allow playing a card during Play phase" in {
    val playerIds = USER_IDS
    val initialState = createInitialState(playerIds)
    val logic = new Logic()
    // First, place bids to move to Play phase
    val bidState = playerIds.foldLeft(initialState) { (state, playerId) => 

      logic.transition(state)(playerId, Event.AnnounceBid(2)).get.last.asInstanceOf[Action.Render[State]].st
    }
    // Get a valid card from the first player's hand
    val firstPlayer = bidState.players.head
    val cardToPlay = bidState.hands(firstPlayer).head

    val playCardResult = logic.transition(bidState)(firstPlayer, Event.PlayCard(cardToPlay))

    playCardResult shouldBe a [Success[?]]
    val actions = playCardResult.get
    val renderAction = actions.head.asInstanceOf[Action.Render[State]]
    renderAction.st.cardsPlayed should have size 1
    renderAction.st.cardsPlayed.head._1 shouldBe firstPlayer
    renderAction.st.cardsPlayed.head._2 shouldBe cardToPlay
  }

  it should "not allow bidding during Play phase" in {
    val playerIds = USER_IDS
    val initialState = createInitialState(playerIds)
    val logic = new Logic()

    // First, place bids to move to Play phase
    val playState = playerIds.foldLeft(initialState) { (state, playerId) =>
      logic.transition(state)(playerId, Event.AnnounceBid(2)).get.head.asInstanceOf[Action.Render[State]].st
    }

    val bidEvent = Event.AnnounceBid(2)
    val result = logic.transition(playState)(UID0, bidEvent)

    result shouldBe a [Failure[?]]
    result.failed.get shouldBe an[Exception]
  }

  // Test end phases
  it should "not allow moves during RoundEnd phase" in {
    val playerIds = USER_IDS
    val initialState = createInitialState(playerIds)
    val logic = new Logic()

    // Create a state in RoundEnd phase
    val roundEndState = initialState.copy(phase = Phase.RoundEnd)

    val bidEvent = Event.AnnounceBid(2)
    val playCardEvent = Event.PlayCard(initialState.hands(UID0).head)

    val bidResult = logic.transition(roundEndState)(UID0, bidEvent)
    val playCardResult = logic.transition(roundEndState)(UID0, playCardEvent)

    bidResult shouldBe a [Failure[?]]
    playCardResult shouldBe a [Failure[?]]
    
    bidResult.failed.get shouldBe an[Exception]
    playCardResult.failed.get shouldBe an[Exception]
  }

  it should "not allow moves during GameEnd phase" in {
    val playerIds = USER_IDS
    val initialState = createInitialState(playerIds)
    val logic = new Logic()

    // Create a state in GameEnd phase
    val gameEndState = initialState.copy(phase = Phase.GameEnd)

    val bidEvent = Event.AnnounceBid(2)
    val playCardEvent = Event.PlayCard(initialState.hands(UID0).head)

    val bidResult = logic.transition(gameEndState)(UID0, bidEvent)
    val playCardResult = logic.transition(gameEndState)(UID0, playCardEvent)

    bidResult shouldBe a [Failure[?]]
    playCardResult shouldBe a [Failure[?]]
    
    bidResult.failed.get shouldBe an[Exception]
    playCardResult.failed.get shouldBe an[Exception]
  }

  it should "be possible to play cards after initial jester play" in {
    val jester = Card(Suit.Hearts, 1)
    val hands0 = Map(
        UID0 -> Set(jester, Card(Suit.Spades, 5)),
        UID1 -> Set(Card(Suit.Hearts, 2), Card(Suit.Hearts, 3)),
      )
    val stakes0 = Map(
      UID0 -> Stake(0,1),
      UID1 -> Stake(0,1)
      )

    val testingState = createInitialState(Vector(UID0, UID1)).copy(
      round = 2,
      hands = hands0,
      phase = Phase.Play,
      stakes = stakes0
    )
    
    val logic = new Logic()

    val playJester = Event.PlayCard(jester)
    val transitionSequence = logic.transition(testingState)(UID0, playJester)
    val crucialState = transitionSequence.get.last.asInstanceOf[Action.Render[State]].st

    val playAnything = Event.PlayCard(Card(Suit.Hearts, 2))
    val endSequence = logic.transition(crucialState)(UID1, playAnything)

    val endState0 = endSequence.get.head.asInstanceOf[Action.Render[State]].st
    val endState1 = endSequence.get.last.asInstanceOf[Action.Render[State]].st

    endState0.cardsPlayed shouldBe Vector((UID0, jester), (UID1, Card(Suit.Hearts, 2)))
    endState1.cardsPlayed shouldBe Vector((UID0, jester), (UID1, Card(Suit.Hearts, 2)))

  }
}
