package serverdist

import cross.cellutils.Composition
import cross.{Cell, Location, cellutils}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * A non-gui cell.
  * @param location
  *                 This cell's location in the grid.
  * @param composition
  *                    The metallic composition of this cell.
  * @param temperature
  *                    The current temperature of this cell.
  * @param table
  *              The table this cell belongs to.
  * @param config
  *               The configuration of the table this cell' belongs in.
  */
case class NgCell
      (override val location : Location, override var composition: Composition,
       override var temperature : Double, override val table : ListBuffer[ListBuffer[Cell]], config : Config) extends Cell {

  /**
    * This cell's type.
    */
  override var cellType = cellutils.Identity.setBorderType(this, 100, 400, config)

  /**
    * The neighbors associated with this cell.
    */
  override lazy val neighbors : ArrayBuffer[Cell] = ArrayBuffer[Cell]()

  /**
    * Updates the neighbors of this cell.
    * @return
    *         The new neighbors associated with this
    */
  def updateNeighbors() = {
    val ret = cellutils.Identity.getNeighborIndices(this).map{ (loc) =>
      table(loc.row)(loc.column)
    }
    neighbors ++= ret.toBuffer.asInstanceOf[ArrayBuffer[Cell]]
  }

  /**
    * The previous temperature of this cell. Used in polling when
    * calculating it's neighbors temperatures.
    */
  override var prev = temperature

  override def toString : String = {
    s"Cell(location : ${this.location}, composition: ${this.composition}, temperature : ${this.previousTemp}, cellType : ${this.cellType})"
  }

  /**
    * The previous temperature.
    * @return
    *         The previous temperature
    */
  override def previousTemp : Double = prev

  /**
    * Updates the previous temperature.
    * @return
    *         The new temperature.
    */
  override def updateTemp(): Double = {
    prev = temperature
    prev
  }

}