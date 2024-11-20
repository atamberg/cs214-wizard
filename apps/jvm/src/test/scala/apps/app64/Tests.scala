package apps.app64

import cs214.webapp.*
import cs214.webapp.utils.WebappSuite
import cs214.webapp.server.StateMachine

class Tests extends WebappSuite[Event, State, View]:
  override val sm: StateMachine[Event, State, View] = Logic()