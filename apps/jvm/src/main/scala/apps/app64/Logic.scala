package apps.app64

import cs214.webapp.*
import cs214.webapp.server.StateMachine

import scala.util.{Try}

class Logic extends StateMachine[Event, State, View]:
  override val appInfo: AppInfo = AppInfo(
    id = "app64",
    name = "Wizard",
    description = "\"Basically gambling Jass\" - Goethe",
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
            else Seq(Render(nextBidState))
          case _ => throw IllegalMoveException("You must bid during the bidding phase!")
      case Play =>
        event match
          case PlayCard(card) =>
            val nextPlayState = state.playCard(userId, card)
            Seq(
              Render(nextPlayState),
              Pause(500),
              if state.isHandEmpty(userId) 
                then Render(nextPlayState.nextPhase(RoundEnd)) // TODO: Implement shifting of the first player to play after each round
                else Render(nextPlayState.nextPlayer)
            )
          case _ => throw IllegalMoveException("You must play a card during the playing phase!")
      case RoundEnd | GameEnd => throw IllegalMoveException("You can only make a move during a round!")

  override def project(state: State)(userId: UserId): View =
    import Phase.*

    state.phase match
      case Bid =>
        View(
          phaseView = PhaseView.BidSelecting(state.stakes),
          scoreView = state.scores
        )
      case Play =>
        View(
          phaseView = PhaseView.CardSelecting(state.hands(userId), state.stakes),
          scoreView = state.scores
        )
      case RoundEnd => ???
      case GameEnd => ???