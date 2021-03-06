package cross.cellutils

import cross.{HeatConstants, Location, Cell}
import HeatConstants._
import cross.{Location, Cell}
import serverdist.{Bottom, Top, Config}

/**
  * Convenience object for cell identity settings.
  */
object Identity {
  /**
    * Returns the indices of all the neighbors of a cell.
    *
    * @param cell
    *             A cell.
    * @return
    *         The indices of the cell's neighbors.
    */
  def getNeighborIndices(cell : Cell) : List[Location] = {
    cell.cellType match {
      case UPPER_LEFT => List[Location]()
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
      case LOWER_RIGHT => List[Location]()

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

  /**
    * Updates the border type of the cell.
    *
    * @param cell
    *             The cell who border's are being set.
    * @param numberOfRows
    *                     The number of rows in the parent table.
    * @param numberOfColumns
    *                        The number of columns int he parent table.
    * @param config
    *               The configuration of the parent table.
    * @return
    *         The new border type.
    */
  def setBorderType(cell: Cell, numberOfRows : Int, numberOfColumns : Int, config : Config): String = {
    val columnEnd: Int = numberOfColumns - 1
    val rowEnd: Int = numberOfRows - 1

    if (this.isCorner(cell.location, numberOfRows, numberOfColumns)) {
      cell.location match {
        case Location(0, 0) => {
          config match {
            case Top() => cell.prev = 90.0
                          cell.temperature = 90.0
                          UPPER_LEFT
            case Bottom() => cell.prev = 90.0
                             cell.temperature = 90.0
                             UPPER_LEFT
          }

        }
        case Location(0, `rowEnd`) => LOWER_LEFT

        case Location(`columnEnd`, `rowEnd`) => {
          config match {
            case Top() => println("Go corner right low")
                          cell.temperature = 50.0
                          cell.prev = 50.0
                          LOWER_RIGHT
            case Bottom() => println("Go corner right low")
                             cell.temperature = 50.0
                             cell.prev = 50.0
                             LOWER_RIGHT
          }
        }
        case Location(`columnEnd`, 0) => UPPER_RIGHT
        case _ => CENTER_CELL
      }
    } else if (this.isBorder(cell.location, numberOfRows, numberOfColumns)) {
      cell.location match {
        case Location(0, row) => if (row > 0 && row < numberOfRows - 1) LEFT_BORDER else CENTER_CELL
        case Location(column, 0) => if (column > 0 && column < numberOfColumns - 1) UPPER_BORDER else CENTER_CELL
        case Location(`columnEnd`, row) => if (row > 0 && row < numberOfRows - 1) RIGHT_BORDER else CENTER_CELL
        case Location(column, `rowEnd`) => if (column > 0 && column < numberOfColumns) LOWER_BORDER else CENTER_CELL
        case _ => CENTER_CELL
      }
    } else {
      CENTER_CELL
    }
  }

  /**
    *
    * Determines if a cell is a corner cell.
    *
    * @param location
    *                 The cell's location.
    * @param numberOfRows
    *                     The number of rows in the parent table.
    * @param numberOfColumns
    *                        The number of columns int he parent table.
    * @return
    *         True if the cell is on the corners.
    */
  def isCorner(location: Location, numberOfRows : Int, numberOfColumns : Int): Boolean = {
    val columnEnd: Int = numberOfColumns - 1
    val rowEnd: Int = numberOfRows - 1
    location match {
      case Location(0, 0) => true
      case Location(0, `rowEnd`) => true
      case Location(`columnEnd`, `rowEnd`) => true
      case Location(`columnEnd`, 0) => true
      case _ => false
    }
  }

  /**
    * Determines if a cell is a border cell.
    *
    * @param location
    *                 The cell's location.
    * @param numberOfRows
    *                     The number of rows in the parent table.
    * @param numberOfColumns
    *                        The number of columns int he parent table.
    * @return
    *         True if the cell is on the borders.
    */
  def isBorder(location: Location, numberOfRows : Int, numberOfColumns : Int): Boolean = {
    val columnEnd: Int = numberOfColumns - 1
    val rowEnd: Int = numberOfRows - 1

    location match {
      case Location(0, row) => row > 0 && row < numberOfRows - 1
      case Location(column, 0) => column > 0 && column < numberOfColumns - 1
      case Location(`columnEnd`, row) => row > 0 && row < numberOfRows - 1
      case Location(column, `rowEnd`) => column > 0 && column < numberOfColumns
      case _ => false
    }
  }
}