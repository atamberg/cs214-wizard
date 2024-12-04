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
    State(
      clients.toVector, 
      clients.map(u => u -> Stake(0, 0)).toMap, 
      clients.map(u => u -> Set()).toMap, 
      clients.map(u => (u, 0)).toMap,
      Suit.random,
      Suit.Hearts,
      0,
      Phase.Bet
      )

  override def transition(state: State)(
    userId: UserId,
    event: Event
  ): Try[Seq[Action[State]]] = ???

  override def project(state: State)(userId: UserId): View = ???
