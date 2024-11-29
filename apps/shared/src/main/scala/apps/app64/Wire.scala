package apps.app64

import cs214.webapp.*
import scala.util.{Failure, Success, Try}
import upickle.*;
import upickle.default._

object Wire extends AppWire[Event, View]:
  import ujson.*
  import upickle.default.*
  import Event.* 
  import View.*

  // Provide implicit reads/writes for types
  implicit val suitRW: ReadWriter[Suit] = macroRW   // Automatically generate for enums
  implicit val cardRW: ReadWriter[Card] = macroRW  // Automatically generate for case classes
  implicit val stakeRW: ReadWriter[Stake] = macroRW
  implicit val phaseRW: ReadWriter[Phase] = macroRW
  implicit val phaseViewRW: ReadWriter[PhaseView] = macroRW
  implicit val userIdRW: ReadWriter[UserId] = macroRW

  // IdMap requires explicit conversion since it's a type alias
  implicit def idMapRW[T: ReadWriter]: ReadWriter[IdMap[T]] =
    implicitly[ReadWriter[Map[UserId, T]]].bimap[IdMap[T]](
      _.asInstanceOf[Map[UserId, T]], // Serialization
      _.asInstanceOf[IdMap[T]]        // Deserialization
    )

  implicit val eventRW: ReadWriter[Event] = macroRW
  implicit val viewRW: ReadWriter[View] = macroRW

  override object eventFormat extends WireFormat[Event]:
    override def encode(event: Event): Value = writeJs(event)
    override def decode(js: Value): Try[Event] = Try(read[Event](js))

  override object viewFormat extends WireFormat[View]:
    override def encode(v: View): Value = writeJs(v)
    override def decode(js: Value): Try[View] = Try(read[View](js))


  