package assignment3;

import org.freedesktop.cairo.Context;
import org.gnome.gdk.Event;
import org.gnome.gtk.*;

/**
 * Java Gnome tutorial
 *
 * This program draws a simple
 * drawing with the Cairo library.
 *
 * @author jan bodnar
 * website zetcode.com
 * last modified March 2009
 */

public class GSimpleDrawing extends Window {

    public GSimpleDrawing() {

        setTitle("Simple drawing");

        initUI();

        connect(new Window.DeleteEvent() {
            public boolean onDeleteEvent(Widget source, Event event) {
                Gtk.mainQuit();
                return false;
            }
        });
        setPosition(WindowPosition.CENTER);
        showAll();
    }

    public void initUI() {
        DrawingArea darea = new DrawingArea();
        darea.connect((Draw) (widget, context) -> {
            drawShape(context);
            return false;
        });
        add(darea);
    }

    public void drawShape(Context cr) {

        int columnPosit = 0;
        int rowPosit = 0;

        Double ratio = 255.0 / 333333;

        for(int j = 0; j < 1080; j++){
            cr.setSource(ratio * j, 0.0, 0.0, 1.0);
            for (int i = 0; i < 1920; i++) {
                cr.rectangle(i, j, 1, 1);
                cr.fill();
            }
        }
    }

    public static void main(String[] args) {
        Gtk.init(args);
        new GSimpleDrawing();
        Gtk.main();
    }
}