package apps.app64

import cs214.webapp.*
import cs214.webapp.client.*

import scalatags.JsDom.*
import scalatags.JsDom.all.*
import org.scalajs.dom

import scala.scalajs.js.annotation.{JSExportTopLevel}
import cs214.webapp.client.graphics.WebClientAppInstance
import upickle.implicits.key

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
      // TODO: add legend for player colors
      renderView(userId, view)
    )

  def renderView(userId: UserId, view: View): Frag =
    import PhaseView.*
    val scores = view.scoreView
    val StateView(
      players, stakes, cardsPlayed,
      trumpSuit, currentsuit, round, trickWinner
    ) = view.stateView

    view.phaseView match
      case CardSelecting(hand) =>
        frag(
          div(
            id := "players-grid",

            renderPlayers(userId, players, stakes, Set())({
              div(
                id := "player-hand",

                if userId == players.head then
                  renderValidHand(hand)
                else 
                  renderHand(hand.map(_._1))
              )
            }, false),

            renderCards(trumpSuit, cardsPlayed)
          ),

          renderScoreBoard(scores)
        )

      case BidSelecting(hand) =>
        frag(
          div(
            id := "players-grid",

            renderPlayers(userId, players, stakes, hand){
              div(
                id := "bid-buttons",

                div("Bids:"),

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

      case Waiting(hand) =>
        frag(
          div(
            id := "players-grid",

            renderPlayers(userId, players, stakes, hand){
              div(
                id := "wait-for-others",
                "Wait for other players..."
              )
            },

            renderCards(trumpSuit, cardsPlayed)
          ),

          renderScoreBoard(scores)
        )

      case PlayEnding(hand, trickWinner) =>
        frag(
          div(
            id := "players-grid",

            renderPlayers(userId, players, stakes, hand)(div()),

            div(
              id  := "cards",
              cls := "play-end",

              s"${trickWinner.capitalize} scored the trick!"
            )
          )
        )

      case RoundEnding =>
        frag(
          div(
            id := "players-grid",

            renderPlayers(userId, players, stakes, Set())(div()),

            div(
              id  := "cards",
              cls := "round-end",

              "TODO"
            )
          )
        )

      case GameEnding =>
        frag(

        )
  end renderView


  def renderPlayers[T <: dom.Element](
    userId:  UserId,
    players: Vector[UserId],
    stakes:  Map[UserId, Stake],
    hand:    Hand
  )(
    currUserView: TypedTag[T],
    handRender:  Boolean = true
  ) =
    // TODO: maybe we should sort differently
    for player <- players.sorted yield div(
      cls := "player",

      if player == userId 
        then currUserView else frag(),

      if handRender && player == userId 
        then renderHand(hand) else frag(),

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


  def renderHand(hand: Hand) =
    div(
      id := "player-hand",
      (for card <- hand yield div(
        cls := "invalid-card",
        card.toString
      )).toVector
    )
  end renderHand


  def renderValidHand(validHand: Set[(Card, Boolean)]) =
    div(
      id := "player-hand",
      (for (card, valid) <- validHand yield div(
        cls := (if valid then "valid-card" else "invalid-card"),

        if valid then
          onclick := {() => sendEvent(Event.PlayCard(card))}
        else
          frag(),

        card.toString
      )).toVector
    )


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
    |   grid-template-rows: auto repeat(3, 1fr);
    |   margin: .5rem 1rem;
    | }
    |
    | #bid-buttons {
    |   display: flex;
    |   justify-content: center;
    |   align-items: center;
    |   margin-bottom: .3rem
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
    | #player-hand {
    |   grid-row: 2;
    |   display: flex;
    |   justify-content: center;
    |   align-items: center;
    |   flex-flow: wrap;
    | }
    |
    | .valid-card, .invalid-card {
    |   font-size: 2rem;
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
    |   grid-row: 3;
    |   align-content: center;
    |   margin: .7rem 2rem;
    |   background-color: #80d4ff;
    |   border-radius: 7px;
    |   font-size: 1.5rem;
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
    |   grid-row: 4;
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
    | .play-end, .round-end {
    |   background-color: #ffb726;
    |   font-size: 1.2rem;
    | }
    |
    | #card-grid {
    |   display: grid;
    |   justify-content: center;
    |   align-items: center;
    |
    |   gap: .8rem;
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
    |   font-size: 2rem;
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
