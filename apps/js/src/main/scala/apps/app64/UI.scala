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
      link(
        rel := "stylesheet",
        href := "/static/style.css"
      ),
      h2("Wizard - It's basically gambling Jass"),
      div(
        id := "legend",
        div(
          id := "current-swatch",
          div(cls := "swatch", ""), span("Current Player")
        ),
        div(
          id := "you-swatch",
          div(cls := "swatch", ""), span("You")
        ),
        div(
          id := "other-swatch",
          div(cls := "swatch", ""), span("Other players")
        )
      ),
      renderView(userId, view)
    )


  /**
    * Method which renders a given view
    *
    * @param userId The current user
    * @param view   The view to render
    * @return       A frag node containing the HTML tags of the UI
    */
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

            renderCards(trumpSuit, cardsPlayed, players)
          ),

          renderScoreBoard(scores, userId)
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

            renderCards(trumpSuit, cardsPlayed, players)
          ),

          renderScoreBoard(scores, userId)
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

            renderCards(trumpSuit, cardsPlayed, players)
          ),

          renderScoreBoard(scores, userId)
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
          ),

          renderScoreBoard(scores, userId)
        )

      case RoundEnding =>
        frag(
          div(
            id := "players-grid",

            renderPlayers(userId, players, stakes, Set())(div()),

            div(
              id  := "cards",
              cls := "round-end",

              for
                player <- players.sorted
                delta = stakes(player).score
              yield div(
                s"$player: ${if delta >= 0 then '+' else '-'}${math.abs(delta)}"
              )
            )
          ),

          renderScoreBoard(scores, userId)
        )

      case GameEnding =>
        frag(
          div(
            id := "end-game",

            renderScoreBoard(scores, userId, true)
          )
        )
  end renderView


  /**
    * Method to render the players in the player grid, accepting an element
    * to render for the current user
    *
    * @param userId       The current user
    * @param players      The players of the game
    * @param stakes       The current stakes at hand of all players
    * @param hand         The hand of the current player
    * @param currUserView The additional element to render for the current user
    * @param handRender   Flag to specifiy whether to render the hand of the player
    *                     or not, defaults to true.
    * @return             A frag node containing the HTML tags for this UI
    */
  def renderPlayers[T <: dom.Element](
    userId:  UserId,
    players: Vector[UserId],
    stakes:  Map[UserId, Stake],
    hand:    Hand
  )(
    currUserView: TypedTag[T],
    handRender:  Boolean = true
  ) =
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
        player
          .split(" ")
          .foldLeft("")((acc, word) => acc + word.head)
          .toUpperCase()
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


  /**
    * Method to render the played cars in the middle of the players
    *
    * @param trumpSuit   The current trump suit
    * @param cardsPlayed The cards that have been played thus far
    * @param players     The players of the game
    * @return            A frag node containing the HTML tags for this UI
    */
  def renderCards(
    trumpSuit: Suit,
    cardsPlayed: Vector[(UserId, Card)],
    players: Vector[UserId]
  ) =
    def cardsMap: Map[UserId, Card] = cardsPlayed.toMap

    div(
      id := "cards",
      div (
        id := "card-grid",
        for
          player <- players.sorted
          card =
            if cardsMap.keySet(player)
            then cardsMap(player).toString else ""
        yield div(
          cls := "played-card",
          card
        ),
        div(id := "current-trump", trumpSuit.toString()),
      )
    )
  end renderCards


  /**
    * Method to render the hand of the current player during the
    * waiting phase specifically.
    *
    * @param hand The hand of the current player
    * @return     A frag node containing the HTML tags for this UI
    */
  def renderHand(hand: Hand) =
    div(
      id := "player-hand",
      (for (card, index) <- hand.zipWithIndex yield div(
        cls := "invalid-card",
        // Use attr method for custom attributes
        attr("data-key") := s"card-${card.toString}-${index}",
        card.toString
      )).toVector
  )
  end renderHand


  /**
    * Method to render the hand of the current player during the
    * card selecting phase specifically.
    *
    * @param validHand The valid hand of the current player
    * @return          A frag node containing the HTML tags for this UI
    */
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


  /**
    * Method to render the scoreboard
    *
    * @param scores  The current scores of all players
    * @param userId  The current user
    * @param gameEnd Flag to specify whether it is the end of the game and
    *                thus if the crowns should be rendered. Default: false
    * @return        A frag node containing the HTML tags for this UI
    */
  def renderScoreBoard(
    scores: Map[UserId, Int],
    userId: UserId,
    gameEnd: Boolean = false
  ) =
    val scoresSorted = scores.toList.sortBy(_._2).reverse
    val winner = scoresSorted.head._1
    div(
      id := "scoreboard",

      if gameEnd then h1(
        s"Game Over - You ${if userId == winner then "won! 👑" else "lost!"}"
      )
      else frag(),

      h2("Scoreboard"),

      div(
        (for (player, score) <- scoresSorted yield div(
          cls := "scoreboard-item",

          if player == userId then id := "current-user" else frag(),

          span(s"${player.capitalize}:"),

          span(
            cls := "text-right-aligned",
            s"${if score > 0 then '+' else if score < 0 then '-' else ' '}${math.abs(score)}"
          ),

          if gameEnd && player == winner
            then span(cls := "text-right-aligned", "👑") else frag()
        )).toVector,
      )
    )
  end renderScoreBoard


  override def css: String = super.css
