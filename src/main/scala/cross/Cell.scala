package cross

import cross.cellutils.Composition

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by rtorres12 on 12/9/15.
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

case class Location(column : Int, row : Int)

