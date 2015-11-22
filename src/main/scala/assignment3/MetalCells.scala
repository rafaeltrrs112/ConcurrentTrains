package assignment3

import org.gnome.gdk.{EventButton, RGBA, Event}
import org.gnome.gtk._
import java.awt.Point
import org.gnome.pango.FontDescription
import sun.awt.FontDescriptor

import scala.collection.mutable._
import scala.util.Random

object Run extends App {
  Gtk.init(args)
  new MetalCells(5, 10, (70, 70))
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
  val CELL_PADDING = 5
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

  def initUI {
    connect(new Window.DeleteEvent() {
      def onDeleteEvent(source: Widget, event: Event): Boolean = {
        Gtk.mainQuit
        return false
      }
    })

    val fixed: Fixed = new Fixed

    var rowPos : Int = 20

    TABLE ++= (for(row <- 0 to numberOfRows - 1) yield {
      val newRow  = ListBuffer[Cell]()
      rowPos += CELL_WIDTH + CELL_PADDING
      var columnPos = 60

      newRow ++= (for(column <- 0 to numberOfColumns - 1) yield {
        columnPos += CELL_WIDTH + CELL_PADDING

        val toButton = (width : Int, height: Int, location: Location) => (color : RGBA) => Cell(width, height, location, color, Composition.randomComposition, 0)

        val preCell = toButton(CELL_WIDTH, CELL_HEIGHT, Location(column, row))
        val cellLocation = Location(column, row)

        val cellButton = {
          if (isCorner(cellLocation)) {
            preCell(RGBA.RED)
          }
          else if (isBorder(cellLocation)) {
            preCell(RGBA.GREEN)
          } else {
            preCell(RGBA.BLUE)
          }
        }


        fixed.put(cellButton, columnPos, rowPos)

        cellButton.setLabel(cellButton.temperature.toString)
        cellButton.overrideFont(new FontDescription("white, Monospace, 12"))
        cellButton.setSizeRequest(CELL_WIDTH, CELL_HEIGHT)
        cellButton.connect(this)

        cellButton
      })

      newRow
    })

    fixed.put(new Label("Metal Cells"), 20, 20)
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
    toggleButton match {
      case cell : Cell => {
        println(cell)
        if(toggleButton.getActive) {
          getNeighborIndices(cell).foreach((columnRow) => {
            TABLE(columnRow.row)(columnRow.column).overrideBackground(StateFlags.NORMAL, RGBA.BLACK)
          })
        } else {
          getNeighborIndices(cell).foreach((columnRow) => {
            TABLE(columnRow.row)(columnRow.column).resetColor()
          })
        }
      }
    }
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
          cell.temperature = 112
          UPPER_LEFT
        }
        case Location(0, `rowEnd`) => LOWER_LEFT
        case Location(`columnEnd`, `rowEnd`) => {
          cell.temperature = 421
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
        Location(cell.location.column + 1, cell.location.row),
        Location(cell.location.column, cell.location.row + 1),
        Location(cell.location.column + 1, cell.location.row + 1)
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
        Location(cell.location.column - 1, cell.location.row),
        Location(cell.location.column, cell.location.row - 1),
        Location(cell.location.column - 1, cell.location.row - 1)
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

  case class Cell(width : Int, height : Int, location : Location, defaultColor : RGBA, composition: Composition, var temperature : Long) extends ToggleButton {
    var cellType = setBorderType(this)
    lazy val neighbors : List[Cell] = getNeighborIndices(this).map((location) => TABLE(location.row)(location.column))

    setSizeRequest(width, height)
    overrideBackground(StateFlags.NORMAL, defaultColor)

    override def toString : String = {
      s"Cell(width : ${this.width}, height : ${this.height}, location : ${this.location}, defaultColor : ${this.defaultColor}, composition: ${this.composition}, temperature : ${this.temperature}, cellType : ${this.cellType})"
    }

    def resetColor() = {
      overrideBackground(StateFlags.NORMAL, defaultColor)
    }

  }





  setTitle("ToggleButton")
  initUI
  setPosition(WindowPosition.CENTER)
  setSizeRequest(350, 220)
  showAll()
  Gtk.main()
}