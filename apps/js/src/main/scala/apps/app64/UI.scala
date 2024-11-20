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

  override val wire: AppWire[Event, View] = ???

  override def css: String = ???

  override def render(userId: UserId, view: View): all.Frag = ???