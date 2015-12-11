package local

import java.util.concurrent.{TimeUnit, Executors}
import akka.actor.ActorSystem
import cross._
import client._
import cross.HeatConstants
import cross.cellutils.Composition
import org.freedesktop.cairo.Context
import org.gnome.gdk.{EventButton, RGBA, Event}
import org.gnome.gtk.Widget.ButtonPressEvent
import serverdist.Top
import scala.concurrent._
import scala.collection.mutable._
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Random}
import org.gnome.gtk.{Window, Widget, DrawingArea, Gtk, Grid, ToggleButton, WindowPosition}
import cross.cellutils.Calc._
import HeatConstants._

class MetalCells(val numberOfRows : Int, val numberOfColumns : Int) extends Window {
  val TABLE = ListBuffer[ListBuffer[Cell]]()
  implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))

  def initUI() {
    connect(new Window.DeleteEvent() {
      def onDeleteEvent(source: Widget, event: Event): Boolean = {
        Gtk.mainQuit()
        false
      }
    })

    val grid = new Grid()

    val b = new ToggleButton()
    val da = new DrawingArea()


    da.setSizeRequest(numberOfColumns, numberOfRows)

    grid.attach(b, 0, 0, 40, 40)
    grid.attach(da, 50, 50, numberOfColumns, numberOfRows)
    grid.showAll()
    b.setSizeRequest(40, 40)
    add(grid)
    b.connect(new ButtonPressEvent {
      override def onButtonPressEvent(widget: Widget, eventButton: EventButton): Boolean = {
        initPixels(new Context(da.getWindow))
        UIStart
        testRun()
        false
      }
    })
  }

  def initPixels(context: Context): Unit = {
    TABLE ++= (for (row <- 0 to numberOfRows - 1) yield {
      context.setSource(0.0, 0.0, 0.0, 1.0)
      val newRow = ListBuffer[Cell]()
      newRow ++= (for (column <- 0 to numberOfColumns - 1) yield {

        context.rectangle(column, row, 1, 1)

        context.fill()

        val toButton = (location: Location) => (color: RGBA) => GuiCell(location, color, Composition.randomComposition, 0.0, column, row, context, TABLE)
        val preCell = toButton(Location(column, row))

        val cellButton = preCell(RGBA.BLACK)

        cellutils.Identity.setBorderType(cellButton, numberOfRows, numberOfColumns, Top())
        cellButton
      })
      newRow
    })
  }

  def randomTriple: (Double, Double, Double) = {
    val firstRandom: Double = Random.nextDouble() * 100
    val secondRandom: Double = Random.nextDouble * 100
    val thirdRandom: Double = Random.nextDouble * 100

    val total: Double = firstRandom + secondRandom + thirdRandom

    (firstRandom / total, secondRandom / total, thirdRandom / total)
  }

  def getColorRange(progress: Double): RGBA = {
    val hsl = java.awt.Color.getHSBColor(progress.toFloat, 0.5F, 0.5F)
    val red = hsl.getRed
    val green = hsl.getGreen
    val blue = hsl.getBlue

    new RGBA(red, green, blue, 0.8)
  }

  def UIStart = {
    val actorSystem = ActorSystem()

    val scheduler = actorSystem.scheduler
    //this is your on-receive right here
    val task = new Runnable {
      //Client takes in' a
      def run() {
        TABLE.flatten.foreach {
          case cell: GuiCell => cell.updateColor()
          case _ => throw new AssertionError()
        }
      }
    }

    implicit val executor = actorSystem.dispatcher

    scheduler.schedule(
      initialDelay = Duration(1, TimeUnit.SECONDS),
      interval = Duration(1, TimeUnit.SECONDS),
      runnable = task)(ec)
  }

  def isNotCorner(cell: Cell): Boolean = {
    val res = !(cell.cellType.equals(UPPER_LEFT) || cell.cellType.equals(LOWER_RIGHT))
    if(!res) println("Not corner!")
    res
  }

  def testRun(): Unit = {
    print("testRun")

    val total = TABLE.grouped(4).toList.map { (rowGroup) =>
      Future {
        rowGroup.flatten.filter(isNotCorner) map { (cell) =>
          calculateNewTemp(cell)
        }
      }
    }

    val allDone = Future sequence total

    allDone onComplete {
      case Success(list) =>
        val cells = TABLE.flatten
        println("Size " + cells.size)
        cells.foreach(_.updateTemp())
        println("Done")
        testRun()
      case Failure(fail) => throw fail
    }
  }

  def calculateNewTemp(cell: Cell): Double = {
    val vibFuture: Double = vibraniumTotal(cell)
    val croFuture: Double = chromiumTotal(cell)
    val adaFuture: Double = adamantiumTotal(cell)

    cell.temperature = vibFuture + croFuture + adaFuture
    vibFuture + croFuture + adaFuture
  }

  setTitle("ToggleButton")
  initUI()
  setPosition(WindowPosition.CENTER)
  setDefaultSize(numberOfColumns + 70, numberOfRows + 70)
  showAll()
  Gtk.main()
}