package apps.app64

import cs214.webapp.*
import cs214.webapp.server.StateMachine

import scala.util.{Try}

class Logic extends StateMachine[Event, State, View]:
  override val appInfo: AppInfo = AppInfo(
    id = "app64",
    name = "Wizard",
    description = "\"Basically gambling Jass\" - anonymous CS-214 assistant",
    // TODO: change this and the preview image before handin
    year = 2024
  )

  override def wire = apps.app64.Wire

  override def init(clients: Seq[UserId]): State = 
    import Suit.*
    import Phase.*
    State.defaultState(clients).withNewCards

  override def transition(state: State)(
    userId: UserId,
    event: Event
  ): Try[Seq[Action[State]]] = Try:
    import Phase.*
    import Event.*
    import Action.*

    state.phase match
      case Bid =>
        event match
          case AnnounceBid(bid) =>
            val nextBidState = state.placeBid(userId, bid)
            if nextBidState.allReady then
              Seq(
                Render(nextBidState),
                Pause(1000),
                Render(nextBidState.nextPlayer
                                   .nextPhase(Play)
                                   )
              )
            else Seq(Render(nextBidState.nextPlayer))
          case _ => throw IllegalMoveException("You must bid during the bidding phase!")
      case Play =>
        event match
          case PlayCard(card) =>
            // The event executes by adding a card for the current player to the trick
            // If there is no suit yet, change current suit to the suit of this card
            val nextPlayerState = if state.currentSuit != Suit.None then 
              state.playCard(userId, card)
            else 
              state.playCard(userId, card).copy(currentSuit = card.suit)
            Seq(
              Render(nextPlayerState),
              Pause(500),
              // If there are as many cards already played as players,
              // move on to the next play.
              if nextPlayerState.cardsPlayed.size == nextPlayerState.players.size then
                val nextPlayState = nextPlayerState.nextPhase(PlayEnd)
                Render(nextPlayState)
                Pause(500)
                // If the current player (the last in rotation) has an empty hand, 
                // enter round end and start a new round.
                if nextPlayState.isHandEmpty(userId) then
                  val nextRoundState = nextPlayState.nextPhase(RoundEnd)
                  // TODO: Here there is some logic missing to end the game if some
                  // number of rounds has been reached.
                  Render(nextRoundState.nextRound)
                else Render(nextPlayState.nextPlay) // Otherwise continue to next play
              else Render(nextPlayerState.nextPlayer) // Otherwise continue to next player
            )
          case _ => throw IllegalMoveException("You must play a card during the playing phase!")
      case RoundEnd | GameEnd | PlayEnd => throw IllegalMoveException("You can only make a move during a round!")

  override def project(state: State)(userId: UserId): View =
    import Phase.*
    import PhaseView.*

    val stateView = StateView(
      players     = state.players,
      stakes      = state.stakes,
      cardsPlayed = state.cardsPlayed,
      trumpSuit   = state.trumpSuit,
      currentSuit = state.currentSuit,
      round       = state.round
    )

    state.phase match
      case Bid =>
        // current player gets selecting view if they haven't chosen their bid yet
        if state.players.head == userId && !state.stakes.keySet(userId) then
          View(
            phaseView = BidSelecting(state.hands(userId)),
            scoreView = state.scores,
            stateView = stateView
          )
        // others must wait
        else
          View(
            phaseView = Waiting(state.hands(userId)),
            scoreView = state.scores,
            stateView = stateView
          )

      case Play =>
        // user distinction is handled in frontend, come at me
        if state.players.head == userId 
          && !state.cardsPlayed.map(_._1).contains(userId) then
          View(
            phaseView = CardSelecting(state.getValidHand(userId)),
            scoreView = state.scores,
            stateView = stateView
          )
        // others must wait
        else
          View(
            phaseView = Waiting(state.hands(userId)),
            scoreView = state.scores,
            stateView = stateView
          )

      case RoundEnd => 
          View(
            phaseView = RoundEnding,
            scoreView = state.scores,
            stateView = stateView
          )

      case GameEnd  => 
          View(
            phaseView = GameEnding,
            scoreView = state.scores,
            stateView = stateView
          )

      case PlayEnd  => 
          View(
            phaseView = PlayEnding(state.hands(userId)),
            scoreView = state.scores,
            stateView = stateView
          )
