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

    State(
      clients.toVector, 
      Map(),
      Vector(),
      clients.map(_ -> Set()).toMap, 
      clients.map(_ -> 0).toMap,
      Spades,
      None,
      1,
      Bid
    )

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
                Render(nextBidState.nextPhase(Play))
              )
            else Seq(Render(nextBidState.nextPlayer))
          case _ => throw IllegalMoveException("You must bid during the bidding phase!")
      case Play =>
        event match
          case PlayCard(card) =>
            val nextPlayState = state.playCard(userId, card)
            Seq(
              Render(nextPlayState),
              Pause(500),
              if state.cardsPlayed.size == state.players.size then
                Render(nextPlayState.nextPhase(PlayEnd))
                Pause(500)
                if state.isHandEmpty(userId) then
                  Render(nextPlayState.nextPhase(RoundEnd).nextRound)
                else Render(nextPlayState.nextPlay)
              else Render(nextPlayState.nextPlayer)
            )
          case _ => throw IllegalMoveException("You must play a card during the playing phase!")
      case RoundEnd | GameEnd | PlayEnd => throw IllegalMoveException("You can only make a move during a round!")

  override def project(state: State)(userId: UserId): View =
    import Phase.*
    import PhaseView.*

    state.phase match
      case Bid =>
        // current player gets selecting view if they haven't chosen their bid yet
        if state.players.head == userId && !state.stakes.keySet(userId) then
          View(
            phaseView = BidSelecting(state.stakes),
            scoreView = state.scores,
            stateView = StateView(
              players     = state.players,
              trumpSuit   = state.trumpSuit,
              currentSuit = state.currentSuit,
              round       = state.round
            )
          )
        // others must wait
        else
          View(
            phaseView = Waiting(state.players.map(p => (p, state.stakes.keySet(p))).toMap),
            scoreView = state.scores,
            stateView = StateView(
              players     = state.players,
              trumpSuit   = state.trumpSuit,
              currentSuit = state.currentSuit,
              round       = state.round
            )
          )

      case Play =>
        // current player gets selecting view
        if state.players.head == userId then
          View(
            phaseView = CardSelecting(state.hands(userId), state.stakes),
            scoreView = state.scores,
            stateView = StateView(
              players     = state.players,
              trumpSuit   = state.trumpSuit,
              currentSuit = state.currentSuit,
              round       = state.round
            )
          )
        // others must wait
        else
          View(
            // TODO: this is temporary, need to find out how to check whether a player has played
            phaseView = Waiting(state.players.map(p => (p, false)).toMap),
            scoreView = state.scores,
            stateView = StateView(
              players     = state.players,
              trumpSuit   = state.trumpSuit,
              currentSuit = state.currentSuit,
              round       = state.round
            )
          )

      case RoundEnd => ???
      case GameEnd  => ???
      case PlayEnd  => ???
