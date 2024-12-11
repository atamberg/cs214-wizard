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
            val playCardState = state.playCard(userId, card) // sets suit if needed
            lazy val playEndState = playCardState.nextPhase(PlayEnd).computeWinner
            lazy val roundEndState = playEndState.nextPhase(RoundEnd)
            lazy val gameEndState = roundEndState.nextPhase(GameEnd)

            lazy val playCardRender = Seq(Render(playCardState), Pause(200))
            lazy val playEndRender = playCardRender ++ Seq(Render(playEndState), Pause(1500))
            lazy val roundEndRender = playEndRender ++ Seq(Render(roundEndState), Pause(3000))
            lazy val gameEndRender = roundEndRender ++ Seq(Render(gameEndState), Pause(60000))

            if playCardState.cardsPlayed.size != playCardState.players.size then
              // next player's turn
              playCardRender :+ Render(playCardState.nextPlayer)
            else
              if playCardState.round + 1 > Deck.size / playCardState.players.size then
                // maximum number of rounds reached
                gameEndRender
              else if playCardState.hands.forall(e => e._2.isEmpty) then
                // all cards in hand played
                roundEndRender :+ Render(roundEndState.nextRound)
              else
                // all players have played a card
                playEndRender :+ Render(playEndState.nextPlay)

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
      round       = state.round,
      trickWinner = state.trickWinner
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
            phaseView = PlayEnding(
              state.hands(userId), state.trickWinner
            ),
            scoreView = state.scores,
            stateView = stateView
          )
