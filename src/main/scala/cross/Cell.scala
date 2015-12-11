package cross

import cross.cellutils.Composition
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Cell trait for all objects that hold cell data.
  */
trait Cell {
  val location : Location
  var composition : Composition
  var temperature : Double
  val table : ListBuffer[ListBuffer[Cell]]
  var prev : Double
  def previousTemp : Double
  def updateTemp(): Double
  var cellType : String
  val neighbors : ArrayBuffer[Cell]
  def serialize : List[Double] = {
    List(temperature, composition.adamantium.percentage, composition.vibranium.percentage, composition.chromium.percentage)
  }
}

/**
  * Location class for storing the current location of a cell.
  *
  * @param column
  *               The column in a table.
  * @param row
  *            The row in a table.
  */
case class Location(column : Int, row : Int)

