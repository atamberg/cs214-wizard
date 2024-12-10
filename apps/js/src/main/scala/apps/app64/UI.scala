package apps.app64

import cs214.webapp.*
import cs214.webapp.client.*

import scalatags.JsDom.*
import scalatags.JsDom.all.*
import org.scalajs.dom

import scala.scalajs.js.annotation.{JSExportTopLevel}
import cs214.webapp.client.graphics.WebClientAppInstance

@JSExportTopLevel("app64")
object UI extends WSClientApp:
  def appId: String = "app64"
  def uiId: UIId = "html"

  def init(userId: UserId, sendMessage: ujson.Value => Unit, target: dom.Element): ClientAppInstance =
    Instance(userId, sendMessage, target)

class Instance(userId: UserId, sendMessage: ujson.Value => Unit, target: dom.Element)
  extends WebClientAppInstance[Event, View](userId, sendMessage, target):

  override val wire: AppWire[Event, View] = Wire

  override def render(userId: UserId, view: View): Frag =
    frag(
      h2("Wizards - It's basically gambling Jass"),
      renderView(userId, view)
    )

  def renderView(userId: UserId, view: View): Frag =
    val scores = view.scoreView
    val StateView(
      players, stakes, cardsPlayed,
      trumpSuit, currentsuit, round
    ) = view.stateView

    view.phaseView match
      case PhaseView.CardSelecting(hand) =>
        frag(
          div(
            id := "players-grid",

            renderPlayers(userId, players, stakes){
              div(
                id := "player-cards",

                if userId == players.head then {
                  // playable cards
                  (for card <- hand yield div(
                    // TODO: valid and invalid cards => backend
                    cls := "valid-card",
                    card.toString
                  )).toVector
                }
                else {
                  // non-playable cards, just displayed
                  (for card <- hand yield div(
                    cls := "invalid-card",
                    card.toString
                  )).toVector
                }
              )
            },

            renderCards(trumpSuit, cardsPlayed)
          ),

          renderScoreBoard(scores)
        )

      case PhaseView.BidSelecting =>
        frag(
          div(
            id := "players-grid",

            renderPlayers(userId, players, stakes){
              div(
                id := "bid-buttons",

                for i <- (0 to round) yield input(
                  cls := "bid-number",
                  tpe := "button",
                  value := s"$i",
                  onclick := {() => sendEvent(Event.AnnounceBid(i))}
                )
              )
            },

            renderCards(trumpSuit, cardsPlayed)
          ),

          renderScoreBoard(scores)
        )

      case PhaseView.Waiting(ready) => 
        frag(
          div(
            id := "players-grid",

            renderPlayers(userId, players, stakes){
              div(
                id := "wait-for-others",
                "Wait for other players..."
              )
            },

            renderCards(trumpSuit, cardsPlayed)
          ),

          renderScoreBoard(scores)
        )
  end renderView


  def renderPlayers[T <: dom.Element](
    userId: UserId,
    players: Vector[UserId],
    stakes: Map[UserId, Stake]
  )(currUserView: TypedTag[T]) =
    // TODO: maybe we should sort differently
    for player <- players.sorted yield div(
      cls := "player",

      if player == userId then currUserView else frag(),

      div(
        cls := "player-image",
        if player == players.head then cls := "current-player" else frag(),
        if player == userId then cls:= "current-user" else frag(),
        player.head.toUpper.toString
      ),

      div(
        cls := "player-info",
        div(cls := "player-name", player.capitalize),
        {
          val stakeOption = stakes.get(player)
          stakeOption match
            case None => div(cls := "player-stakes", "0 | ?")
            case Some(stake) =>
              div(cls := "player-stakes", s"${stake.tricksWon} | ${stake.bid}")
        }
      )
    )
  end renderPlayers


  def renderCards(
    trumpSuit: Suit, 
    cardsPlayed: Vector[(UserId, Card)]
  ) =
    // TODO: maybe we should sort differently
    val cardsSorted: Vector[Card] = 
      cardsPlayed.sortBy(_._1).map(_._2)
    div(
      id := "cards",
      div (
        id := "card-grid",
        for card <- cardsSorted yield div(
          cls := "played-card", 
          card.toString
        ),
        div(id := "current-trump", trumpSuit.toString()),
      )
    )
  end renderCards


  def renderScoreBoard(scores: Map[UserId, Int]) =
    div(
      id := "scoreboard",
      h2("Scoreboard"),
      div(
        (for (player, score) <- scores yield div(
          cls := "scoreboard-item",
          s"${player.capitalize}: $score"
        )).toVector
      )
    )
  end renderScoreBoard


  override def css: String = super.css +
    """
    | .h2 {
    |   padding: 1rem;
    | }
    |
    | body {
    |   background-color: #f0f0f0;
    |   max-width: 85%
    | }
    |
    | * {
    |   font-family: monospace;
    | }
    |
    | span, div {
    |   font-size: .8rem;
    | }
    |
    | #players-grid {
    |   grid-row: 1;
    |   display: grid;
    |   grid-template-columns: repeat(3, 1fr);
    |   grid-template-rows: repeat(3, 1fr);
    |   gap: 1.5rem;
    | }
    |
    | .player, #cards {
    |   display: grid;
    |   text-align: center;
    |   grid-template-columns: 1fr;
    | }
    |
    | .player {
    |   grid-template-rows: auto 2fr 1fr;
    |   margin: .5rem 1rem;
    | }
    |
    | #bid-buttons {
    |   display: flex;
    |   justify-content: center;
    | }
    |
    | .bid-number {
    |   padding: .3rem .5rem;
    |   background-color: #ffffff;
    |   border-radius: 100%;
    |   border: 1px solid black;
    |   margin: 0rem .2rem;
    |   cursor: pointer;
    | }
    |
    | #player-cards {
    |   display: flex;
    |   justify-content: center;
    |   align-items: center;
    |   flex-flow: wrap;
    | }
    |
    | .valid-card, .invalid-card {
    |   font-size: 2.3rem;
    |   margin: .4rem;
    |   padding-bottom: .4rem;
    | }
    |
    | .valid-card {
    |   border-bottom: 2px solid #ecae03;
    |   cursor: pointer;
    | }
    |
    | .invalid-card {
    |   opacity: .4;
    |   border-bottom: 2px solid #8f8f8f;
    | }
    |
    | #wait-for-others {
    |   font-style: italic;
    |   text-wrap: balance;
    | }
    |
    | .player-image {
    |   grid-row: 2;
    |   align-content: center;
    |   margin: 1rem 3rem;
    |   background-color: #80d4ff;
    |   border-radius: 7px;
    |   font-size: 1.7rem;
    | }
    |
    | .player-image.current-player {
    |   background-color: #ff9777;
    | }
    | 
    | .player-image.current-user {
    |   border: 2px dashed black;
    | }
    |
    | .player-info {
    |   grid-row: 3;
    | }
    |
    | #cards {
    |   grid-row: 2;
    |   grid-column: 2;
    |
    |   border: 1px dashed black;
    |   border-radius: 7px;
    |   grid-template-rows: 1fr;
    |   text-align: center;
    |   padding: 1rem .7rem;
    | }
    |
    | #card-grid {
    |   display: grid;
    |   justify-content: center;
    |   align-items: center;
    |
    |   gap: 1rem;
    |   grid-template-columns: repeat(3, 1fr);
    |   grid-template-rows: repeat(3, 1fr);
    | }
    |
    | #current-trump {
    |   grid-column: 2;
    |   grid-row: 2;
    |
    |   font-size: 2rem;
    |   opacity: .5;
    | }
    |
    | .played-card {
    |   font-size: 2.3rem;
    | }
    |
    | #scoreboard > * {
    |   margin: 1rem 2rem;
    | }
    |
    | #scoreboard h2 {
    |   font-size: 1.5rem;
    |   border-bottom: 2px black solid;
    | }
    |
    | .scoreboard-item {
    |   margin-left: 1rem;
    |   display: grid;
    |   align-items: center;
    |   grid-template-rows: 1fr;
    |   grid-template-columns: 3fr 2fr;
    |
    | }
    |
    | #end-game {
    |   background-color: #6e6e6e;
    |   color: #c0c0c0;
    | }
    |
    | #end-game .player-image {
    |   background-color: #333333;
    | }
    |
    | #end-game-splash {
    |   color: black;
    |   background-color: #f0f0f0;
    |   position: fixed;
    |   top: 60%;
    |   left: 30%;
    |   transform: translate(-34%, -65%);
    |   width: 40%;
    |   height: 60%;
    |
    |   border-radius: 7px;
    | }
    |
    | #end-game-splash h2 {
    |   font-size: 2.3rem;
    | }
    |
    | #end-game-splash div > div {
    |   margin-bottom: 1.3rem;
    | }
    |
    | #end-game-splash span {
    |   font-size: 1.4rem;
    | }
    |
    | #end-game-splash #scoreboard > div > div {
    |   grid-template-columns: 3fr 2fr 1fr;
    | }
    |
    | #crown {
    |   font-size: 2rem !important;
    | }
    |
    | #end-game-buttons {
    |   margin-top: 2rem;
    | }
    |
    | #end-game-buttons input[type="button"] {
    |   max-width: 70%;
    |   margin: 2rem 0;
    |   padding: .5rem 1rem;
    |
    |   border: 1px solid black;
    |   border-radius: 7px;
    | }
    |
    """.stripMargin

  end css