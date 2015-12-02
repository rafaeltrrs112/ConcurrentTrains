import assignment3.MetalCells
import org.gnome.gtk.Gtk

/**
  * Created by rtorres12 on 12/1/15.
  */
object Run extends App {
    Gtk.init(args)
    val mc = new MetalCells(50, 200, (20, 20))
}