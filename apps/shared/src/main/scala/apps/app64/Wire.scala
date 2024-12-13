package apps.app64

import cs214.webapp.*
import scala.util.{Failure, Success, Try}
import upickle.*;
import upickle.default._

object Wire extends AppWire[Event, View]:
  import ujson.*
  import upickle.*
  import Event.*
  import View.*

  override object eventFormat extends WireFormat[Event]:
    override def encode(event: Event): ujson.Value =
      read(default.write(event))
    override def decode(json: ujson.Value): Try[Event] = Try:
      default.read(json)

  override object viewFormat extends WireFormat[View]:
    override def encode(view: View): Value = 
      read(default.write(view))
    override def decode(json: Value): Try[View] = Try:
      default.read(json)

