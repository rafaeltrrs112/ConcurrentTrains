package serverdist

import cross.cellutils.Composition
import cross.{HeatConstants, Cell, Location, cellutils}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by rtorres12 on 12/9/15.
  */
case class NgCell
      (override val location : Location, override var composition: Composition,
       override var temperature : Double, override val table : ListBuffer[ListBuffer[Cell]], config : Config) extends Cell {

  override var cellType = cellutils.Identity.setBorderType(this, 100, 400, config)

  override lazy val neighbors : ArrayBuffer[Cell] = ArrayBuffer[Cell]()

  def updateNeighbors() = {
    val ret = cellutils.Identity.getNeighborIndices(this).map{ (loc) =>
      table(loc.row)(loc.column)
    }
    neighbors ++= ret.toBuffer.asInstanceOf[ArrayBuffer[Cell]]
  }


  override var prev = temperature

  override def toString : String = {
    s"Cell(location : ${this.location}, composition: ${this.composition}, temperature : ${this.previousTemp}, cellType : ${this.cellType})"
  }

  override def previousTemp : Double = prev

  override def updateTemp(): Double = {
    prev = temperature
    prev
  }

}