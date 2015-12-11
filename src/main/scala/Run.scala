import client.HeatNode
import local.MetalCells
import org.gnome.gdk.{Event, EventButton, RGBA}
import org.gnome.gtk.Widget.ButtonPressEvent
import org.gnome.gtk._
import serverdist.{Bottom, Top, HeatNet}
/**
  * Created by rtorres12 on 12/1/15.
  */
object Run extends App {
    Gtk.init(args)
    val topHeat = new HeatNet(8125, 100, 400, Top())
    val bottomHeat = new HeatNet(8126, 100, 400, Bottom())

    topHeat.start()
    bottomHeat.start()

    val window = new Window()
    val grid = new VBox(false, 0)
    val b = new ToggleButton()
    val da = new DrawingArea()
    val da2 = new DrawingArea()

    window.add(grid)

    grid.add(da)
    grid.add(da2)

    grid.showAll()
    b.setSizeRequest(40, 40)


    b.overrideColor(StateFlags.ACTIVE, new RGBA(100, 0, 0 ,1.0))

    b.connect(new ButtonPressEvent {
        override def onButtonPressEvent(widget: Widget, eventButton: EventButton): Boolean = {
            true
        }
    })

    da.setSizeRequest(800, 200)
    da2.setSizeRequest(800, 200)


    window.showAll()

    window.connect(new Window.DeleteEvent() {
        def onDeleteEvent(source: Widget, event: Event): Boolean = {
            Gtk.mainQuit()
            false
        }
    })

    val HeatNode = new HeatNode(8125.toString, da, Top())
    val HeatNode2 = new HeatNode(8126.toString, da2, Bottom())

    HeatNode.boundedNode = Some(HeatNode2)
    HeatNode2.boundedNode = Some(HeatNode)

    Gtk.main()
}
