package apps.app64

import cs214.webapp.*
import scala.util.{Failure, Success, Try}

object Wire extends AppWire[Event, View]:
  import ujson.*

  override object eventFormat extends WireFormat[Event]:
    override def encode(event: Event): Value = ???

    override def decode(js: Value): Try[Event] = ???

  override object viewFormat extends WireFormat[View]:
    override def encode(v: View): Value = ???

    override def decode(js: Value): Try[View] = ???
