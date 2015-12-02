package assignment3

import java.text.DecimalFormat
import java.util.concurrent.atomic.AtomicInteger

import org.gnome.gdk.{EventButton, RGBA, Event}
import org.gnome.gtk.{Widget, Fixed, Gtk}
import org.gnome.pango.FontDescription
import org.gnome.gtk.{Window, ToggleButton, Label}
import org.gnome.gtk.StateFlags
import org.gnome.gtk.WindowPosition
import scala.collection.parallel.ParSeq
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.collection.mutable._
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try, Random}

object Run extends App {
  Gtk.init(args)
  new MetalCells(100, 100, (20, 20))
}

trait Metal{
  val percentage : Double
  val thermalConstant : Double
}

case class Adamantium(override val percentage : Double) extends Metal {
  override val thermalConstant : Double = 0.75
}

case class Vibranium(override val percentage : Double) extends Metal{
  override val thermalConstant : Double = 1.0
}

case class Chromium(override val percentage : Double) extends Metal {
  override val thermalConstant : Double = 1.25
}



case class Composition(adamantium : Adamantium, vibranium: Vibranium, chromium : Chromium)

object Composition {
  def randomComposition : Composition = {
    val firstRandom : Double = Random.nextDouble * 100
    val secondRandom : Double = Random.nextDouble * 100
    val thirdRandom : Double = Random.nextDouble * 100

    val total : Double = firstRandom + secondRandom + thirdRandom

    Composition(Adamantium(firstRandom / total), Vibranium(secondRandom / total), Chromium(thirdRandom / total))
  }
}

class MetalCells(val numberOfRows : Int, val numberOfColumns : Int, cellSize : (Int, Int)) extends Window with ToggleButton.Toggled {
  val CELL_WIDTH = cellSize._1
  val CELL_HEIGHT = cellSize._2
  val CELL_PADDING = 1
  val TABLE = ListBuffer[ListBuffer[Cell]]()

  val UPPER_LEFT = "upperLeft"
  val UPPER_RIGHT = "upperRight"

  val LOWER_LEFT = "lowerLeft"
  val LOWER_RIGHT = "lowerRight"

  val CENTER_CELL = "centerCell"

  val LEFT_BORDER = "leftBorder"
  val RIGHT_BORDER = "rightBorder"
  val UPPER_BORDER = "topBorder"
  val LOWER_BORDER = "bottomBorder"

  val MAX_TEMPERATURE : Double = 10000.0
  val S_VALUE = MAX_TEMPERATURE
  val T_VALUE = 112
  val MAX_RATIO : Double = 255.0 / MAX_TEMPERATURE
  val formatter = new DecimalFormat("##")

  def initUI() {
    connect(new Window.DeleteEvent() {
      def onDeleteEvent(source: Widget, event: Event): Boolean = {
        Gtk.mainQuit()
        false
      }
    })

    val fixed: Fixed = new Fixed

    var rowPos : Int = 20

    TABLE ++= (for(row <- 0 to numberOfRows - 1) yield {
      val newRow  = ListBuffer[Cell]()
      rowPos += CELL_WIDTH + CELL_PADDING
      var columnPos = 0

      newRow ++= (for(column <- 0 to numberOfColumns - 1) yield {
        columnPos += CELL_WIDTH + CELL_PADDING

        val toButton = (width : Int, height: Int, location: Location) => (color : RGBA) => Cell(width, height, location, color, Composition.randomComposition, 10)

        val preCell = toButton(CELL_WIDTH, CELL_HEIGHT, Location(column, row))

        val cellButton = preCell(RGBA.BLACK)

        fixed.put(cellButton, columnPos, rowPos)

        //cellButton.setLabel(cellButton.temperature.toString)
        cellButton.overrideFont(new FontDescription("white, Monospace, 12"))
        cellButton.setSizeRequest(CELL_WIDTH, CELL_HEIGHT)

        cellButton.connect(this)

        setBorderType(cellButton)

        //println(cellButton)
        cellButton
      })

      newRow
    })

    //fixed.put(new Label("Metal Cells"), 20, 20)
    add(fixed)
  }

  def randomTriple : (Double, Double, Double) = {
    val firstRandom : Double = Random.nextDouble() * 100
    val secondRandom : Double = Random.nextDouble * 100
    val thirdRandom : Double = Random.nextDouble * 100

    val total : Double = firstRandom + secondRandom + thirdRandom

    (firstRandom / total, secondRandom / total, thirdRandom / total)
  }


  def onToggled(toggleButton: ToggleButton) {
    testRun()
  }

  case class Location(column : Int, row : Int)

  def isCorner(location: Location): Boolean = {
    val columnEnd : Int = numberOfColumns - 1
    val rowEnd : Int = numberOfRows - 1
    location match {
      case Location(0, 0) => true
      case Location(0, `rowEnd`) => true
      case Location(`columnEnd`, `rowEnd`) => true
      case Location(`columnEnd`, 0) => true
      case _ => false
    }
  }

  def isBorder(location: Location): Boolean = {
    val columnEnd : Int = numberOfColumns - 1
    val rowEnd : Int = numberOfRows - 1

    location match {
      case Location(0, row) => row > 0 && row < numberOfRows - 1
      case Location(column, 0) => column > 0 && column < numberOfColumns - 1
      case Location(`columnEnd`, row) => row > 0 && row < numberOfRows - 1
      case Location(column, `rowEnd`) => column > 0 && column < numberOfColumns
      case _ => false
    }
  }

  def setBorderType(cell: Cell) : String = {
    val columnEnd : Int = numberOfColumns - 1
    val rowEnd : Int = numberOfRows - 1
    if(this.isCorner(cell.location)){
      cell.location match {
        case Location(0, 0) => {
          cell.prev = 500.0
          cell._temperature = 500.0
          UPPER_LEFT
        }
        case Location(0, `rowEnd`) => LOWER_LEFT
        case Location(`columnEnd`, `rowEnd`) => {
          cell._temperature = 1000
          cell.prev = 1000
          LOWER_RIGHT
        }
        case Location(`columnEnd`, 0) => UPPER_RIGHT
        case _ => CENTER_CELL
      }
    } else if(this.isBorder(cell.location)) {
      cell.location match {
        case Location(0, row) => if (row > 0 && row < numberOfRows - 1) LEFT_BORDER else CENTER_CELL
        case Location(column, 0) => if (column > 0 && column < numberOfColumns - 1) UPPER_BORDER else CENTER_CELL
        case Location(`columnEnd`, row) => if(row > 0 && row < numberOfRows - 1) RIGHT_BORDER else CENTER_CELL
        case Location(column, `rowEnd`) => if(column > 0 && column < numberOfColumns) LOWER_BORDER else CENTER_CELL
        case _ => CENTER_CELL
      }
    } else {
      CENTER_CELL
    }
  }

  def getNeighborIndices(cell : Cell) : List[Location] = {
    cell.cellType match {
      case UPPER_LEFT => List[Location](
//        Location(cell.location.column + 1, cell.location.row),
//        Location(cell.location.column, cell.location.row + 1),
//        Location(cell.location.column + 1, cell.location.row + 1)
      )
      case LOWER_LEFT => List[Location](
        Location(cell.location.column + 1, cell.location.row),
        Location(cell.location.column, cell.location.row - 1),
        Location(cell.location.column + 1, cell.location.row - 1)
      )
      case UPPER_RIGHT => List[Location](
        Location(cell.location.column - 1, cell.location.row),
        Location(cell.location.column, cell.location.row + 1),
        Location(cell.location.column - 1, cell.location.row + 1)
      )
      case LOWER_RIGHT => List[Location](
//        Location(cell.location.column - 1, cell.location.row),
//        Location(cell.location.column, cell.location.row - 1),
//        Location(cell.location.column - 1, cell.location.row - 1)
      )

      case LEFT_BORDER => List[Location](
        Location(cell.location.column, cell.location.row - 1),
        Location(cell.location.column, cell.location.row + 1),
        Location(cell.location.column + 1, cell.location.row),
        Location(cell.location.column + 1, cell.location.row - 1),
        Location(cell.location.column + 1 , cell.location.row + 1)
      )
      case RIGHT_BORDER => List[Location](
        Location(cell.location.column, cell.location.row - 1),
        Location(cell.location.column, cell.location.row + 1),
        Location(cell.location.column - 1, cell.location.row),
        Location(cell.location.column - 1, cell.location.row - 1),
        Location(cell.location.column - 1 , cell.location.row + 1)
      )
      case UPPER_BORDER => List[Location](
        Location(cell.location.column - 1, cell.location.row),
        Location(cell.location.column + 1, cell.location.row),
        Location(cell.location.column, cell.location.row + 1),
        Location(cell.location.column - 1, cell.location.row + 1),
        Location(cell.location.column + 1 , cell.location.row + 1)
      )
      case LOWER_BORDER => List[Location](
        Location(cell.location.column - 1, cell.location.row),
        Location(cell.location.column + 1, cell.location.row),
        Location(cell.location.column, cell.location.row - 1),
        Location(cell.location.column - 1, cell.location.row - 1),
        Location(cell.location.column + 1 , cell.location.row - 1)
      )
      case _ => List[Location](
        Location(cell.location.column, cell.location.row + 1),
        Location(cell.location.column, cell.location.row - 1),
        Location(cell.location.column - 1, cell.location.row - 1),
        Location(cell.location.column - 1, cell.location.row),
        Location(cell.location.column - 1 , cell.location.row + 1),
        Location(cell.location.column + 1, cell.location.row - 1),
        Location(cell.location.column + 1, cell.location.row),
        Location(cell.location.column + 1 , cell.location.row + 1)
      )
    }
  }

  case class Cell(width : Int, height : Int, location : Location, defaultColor : RGBA, composition: Composition, var _temperature : Double) extends ToggleButton {
    var cellType = setBorderType(this)
    lazy val neighbors : List[Cell] = getNeighborIndices(this).map((location) => TABLE(location.row)(location.column))

    var prev = _temperature

    setSizeRequest(width, height)
    overrideBackground(StateFlags.NORMAL, defaultColor)
    //setLabel(_temperature.toString)

    override def toString : String = {
      s"Cell(width : ${this.width}, height : ${this.height}, location : ${this.location}, defaultColor : ${this.defaultColor}, composition: ${this.composition}, temperature : ${this.temperature}, cellType : ${this.cellType})"
    }

    def resetColor() = {
      overrideBackground(StateFlags.NORMAL, defaultColor)
    }

    def color_=(bgColor : RGBA) = {
      this.overrideBackground(StateFlags.NORMAL, bgColor)
    }

    //The temperature always refers to label's temperature which holds the previous temperature.
    def temperature : Double = prev

    //the setter for temperature sets the actual temperature.
    def temperature_=(temp : Double) = {
      _temperature = temp
    }

    //the setter for the label.
    def updateCell = {
      //setLabel(formatter.format(_temperature))
      prev = _temperature
      //println(_temperature * MAX_RATIO)
      overrideBackground(StateFlags.NORMAL, new RGBA(_temperature * MAX_RATIO, 0, 0, 1.0))
    }

  }

  def vibraniumTemp(cell : Cell) :  Double = {
    val currentTemp: Double = cell.temperature
    val vibPercent: Double = cell.composition.vibranium.percentage
    currentTemp * vibPercent
  }

  def adamantiumTemp(cell : Cell) : Double = {
    val currentTemp: Double = cell.temperature
    val adaPercent: Double = cell.composition.adamantium.percentage
    currentTemp * adaPercent

  }

  def chromiumTemp(cell : Cell) : Double = {
    val currentTemp: Double = cell.temperature
    val croPercent: Double = cell.composition.chromium.percentage
    currentTemp * croPercent
  }

  def adamantiumTotal(cell : Cell) : Future[Double] = {
    val adaCountList : List[Future[Double]] = cell.neighbors.map {
      (neighbor) => Future[Double]{
        adamantiumTemp(neighbor)
      }
    }
    val sumFuture : Future[List[Double]] = Future sequence adaCountList
    val result = for {
      fin <- sumFuture
    } yield (cell.composition.adamantium.thermalConstant * fin.sum) / cell.neighbors.size
    result
  }

  def chromiumTotal(cell : Cell) : Future[Double] = {
    val croCountList : List[Future[Double]] = cell.neighbors.map {
      (neighbor) => Future[Double]{
        chromiumTemp(neighbor)
      }
    }
    val sumFuture : Future[List[Double]] = Future sequence croCountList
    val result = for {
      fin <- sumFuture
    } yield (cell.composition.chromium.thermalConstant * fin.sum) / cell.neighbors.size
    result
  }

  def vibraniumTotal(cell : Cell) : Future[Double] = {
    val vibraCountList : List[Future[Double]] = cell.neighbors.map {
      (neighbor) => Future[Double]{
        vibraniumTemp(neighbor)
      }
    }
    val sumFuture : Future[List[Double]] = Future sequence vibraCountList
    val result = for {
      fin <- sumFuture
    } yield (cell.composition.vibranium.thermalConstant * fin.sum) / cell.neighbors.size
    result
  }

  def testRun(): Unit = {
      val totalFuture = TABLE.map {
        val count = new AtomicInteger(0)
        _.map {
          (cell) => if(!(cell.cellType.equals(UPPER_LEFT) || cell.cellType.equals(LOWER_RIGHT))) {
            {
              Future {
                val vibFuture: Future[Double] = vibraniumTotal(cell)
                val croFuture: Future[Double] = chromiumTotal(cell)
                val adaFuture: Future[Double] = adamantiumTotal(cell)

                val newTemp : Future[Double] = for {
                  vib <- vibFuture
                  cro <- croFuture
                  ada <- adaFuture
                } yield vib + cro + ada

                newTemp onComplete {
                  case Success(temperature) => {
                    //println(s"New temperature ${temperature}, Old temperature ${cell.temperature} ${temperature > cell.temperature}")
                    cell.temperature = temperature
                    if (count.incrementAndGet() == (numberOfColumns * numberOfRows) - 2) {
                      //println("****LAST ONE *******")
                      TABLE foreach {
                        _ filter ((cell) => !(cell.cellType.equals(UPPER_LEFT) || cell.cellType.equals(LOWER_RIGHT))) foreach {
                          _ updateCell
                        }
                      }
                      testRun()
                    }
                  }
                }
                newTemp
              }
            }
          } else {
            cell.updateCell
          }
        }
      }
  }



  setTitle("ToggleButton")
  initUI()
  setPosition(WindowPosition.CENTER)
  setSizeRequest(350, 220)
  showAll()
  Gtk.main()

}