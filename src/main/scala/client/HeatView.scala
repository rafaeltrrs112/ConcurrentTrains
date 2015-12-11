package client

import io.backchat.hookup.TextMessage
import org.freedesktop.cairo.Context
import org.gnome.gdk.{RGBA, Event, EventButton}
import org.gnome.gtk.Widget.ButtonPressEvent
import org.gnome.gtk._
/**
  * Created by rtorres12 on 12/9/15.
  */
class HeatView(width : Int, height : Int, da : DrawingArea)(implicit val client : HeatNode) extends Window {
  val PADDING = 70
  lazy val painter = new Painter(height, width, new Context(da.getWindow))

}
