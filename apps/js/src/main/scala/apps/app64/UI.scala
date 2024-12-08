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
      h1("Wizards - It's basically gambling Jass"),
      renderView(userId, view)
    )


  def announceBid(bidInput: String): Unit =
    bidInput.toIntOption match
      case Some(bid) => sendEvent(Event.AnnounceBid(bid))
      case None      => {} // TODO: Resolve this in backend


  def renderView(userId: UserId, view: View): Frag =
    val scores    = view.scoreView
    val players   = view.stateView.players
    val trumpSuit = view.stateView.trumpSuit
    val currSuit  = view.stateView.currentSuit

    view.phaseView match
      case PhaseView.CardSelecting(hand, stakes) => frag()
      case PhaseView.BidSelecting(stakes) =>
        frag(
          div(
            cls := "grid-3x3",
            id  := "players-grid",

            for player <- players yield div(
              cls := "player",

              // TODO: only send this view to the current player in the backend
              if player == userId then {
                val bidInput = input(
                  tpe := "number",
                  id  := "num-bids",
                  placeholder := "Number of bids",
                ).render

                div(
                  bidInput,
                  input(
                    tpe := "button",
                    value := "Submit",
                    id := "submit-bids",
                    onclick := announceBid(bidInput.value)
                  )
                )
              } else frag(),

              div(
                cls := "player-image",
                if player == userId then id := "current-player" else frag(),
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
            ),

            div(
              id := "cards",
              div (
                id := "card-grid", cls := "grid-3x3",
                div(id := "current-trump", trumpSuit.toString())
              )
            )
          ),

          // TODO: Scoreboard
        )
      case PhaseView.Waiting(ready) => frag()
  end renderView

  override def css: String = super.css +
    """
    | .app64 .h1 {
    |   padding: 1rem;
    | }
    | body {
    |   background-color: #f0f0f0;
    | }
    | * {
    |   font-family: monospace;
    |   font-size: .9rem;
    | }
    | h1 {
    |   font-size: 2rem;
    |   padding: 1rem 3rem;
    | }
    | #players-grid {
    |   grid-row: 1;
    |   display: grid;
    |   grid-template-columns: 30% 40% 30%;
    |   grid-template-rows: 1fr 1fr 1fr;
    |   gap: 2rem;
    | }
    | .player, #cards {
    |   display: grid;
    |   text-align: center;
    |   grid-template-columns: 1fr;
    | }
    | .player {
    |   grid-template-rows: 3fr 1fr;
    |   margin: .5rem 1rem;
    | }
    | #num-bids {
    |   padding: .3rem;
    |   background-color: #ffffff;
    |   border-radius: 7px;
    |   border: 1px solid black;
    | }
    | #submit-bids {
    |   padding: .3rem; 
    |   border: 1px solid black;
    |   border-radius: 7px;
    | }
    | #player-cards {
    |   display: flex;
    |   justify-content: center;
    |   align-items: center;
    |   flex-flow: wrap;
    | }
    | .valid-card, .invalid-card {
    |   font-size: 2.3rem;
    |   margin: 0 .4rem;
    |   padding-bottom: .4rem;
    | }
    | .valid-card {
    |   border-bottom: 2px solid #ecae03;
    | }
    | .invalid-card {
    |   opacity: .4;
    |   border-bottom: 2px solid #8f8f8f;
    | }
    | #wait-for-others {
    |   font-style: italic;
    | }
    | .player-image {
    |   align-content: center;
    |   margin: 1rem 3rem;
    |   background-color: #50aaff;
    |   border-radius: 7px;
    |   font-size: 2rem;
    | }
    | .player-image#current-player {
    |   background-color: #f0ab51;
    | }
    | .player-info {
    |   grid-column: 1;
    |   display: grid;
    |   grid-template-columns: 1fr;
    |   grid-template-rows: 1fr 1fr;
    |   gap: .5rem;
    | }
    | #cards {
    |   grid-row: 2;
    |   grid-column: 2;
    |   border: 1px dashed black;
    |   border-radius: 7px;
    |   grid-template-rows: 1fr;
    |   text-align: center;
    |   padding: 1rem .7rem;
    | }
    | #card-grid {
    |   display: grid;
    |   justify-content: center;
    |   align-items: center;
    |   gap: 1rem;
    |   grid-template-columns: 2fr 3fr 2fr;
    |   grid-template-rows: 2fr 3fr 2fr;
    | }
    | #current-trump {
    |   grid-column: 2;
    |   grid-row: 2;
    |   font-size: 3rem;
    |   opacity: .5;
    | }
    | .played-card {
    |   font-size: 2.3rem;
    | }
    | .grid-3x3 > :nth-child(1) {
    |   grid-row: 1;
    |   grid-column: 2;
    | }
    | .grid-3x3 > :nth-child(2) {
    |   grid-row: 2;
    |   grid-column: 1;
    | }
    | .grid-3x3 > :nth-child(3) {
    |   grid-row: 2;
    |   grid-column: 3;
    | }
    | .grid-3x3 > :nth-child(4) {
    |   grid-row: 3;
    |   grid-column: 2;
    | }
    | #scoreboard > * {
    |   margin: 1rem 2rem;
    | }
    | #scoreboard h2 {
    |   font-size: 1.5rem;
    |   border-bottom: 2px black solid;
    | }
    | #scoreboard > div > div {
    |   margin-left: 1rem;
    |   display: grid;
    |   align-items: center;
    |   grid-template-rows: 1fr;
    |   grid-template-columns: 3fr 2fr;
    | }
    | #end-game {
    |   background-color: #6e6e6e;
    |   color: #c0c0c0;
    | }
    | #end-game .player-image {
    |   background-color: #333333;
    | }
    | #end-game-splash {
    |   color: black;
    |   background-color: #f0f0f0;
    |   position: fixed;
    |   top: 60%;
    |   left: 30%;
    |   transform: translate(-34%, -65%);
    |   width: 40%;
    |   height: 60%;
    |   border-radius: 7px;
    | }
    | #end-game-splash h2 {
    |   font-size: 2.3rem;
    | }
    | #end-game-splash div > div {
    |   margin-bottom: 1.3rem;
    | }
    | #end-game-splash span {
    |   font-size: 1.4rem;
    | }
    | #end-game-splash #scoreboard > div > div {
    |   grid-template-columns: 3fr 2fr 1fr;
    | }
    | #crown {
    |   font-size: 2rem !important;
    | }
    | #end-game-buttons {
    |   margin-top: 2rem;
    | }
    | #end-game-buttons input[type="button"] {
    |   max-width: 70%;
    |   margin: 2rem 0;
    |   padding: .5rem 1rem;
    |   border: 1px solid black;
    |   border-radius: 7px;
    | }
    """.stripMargin

  end css