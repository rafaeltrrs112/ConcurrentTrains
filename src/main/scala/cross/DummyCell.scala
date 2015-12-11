package cross

import cross.HeatConstants._
import cross.cellutils.Composition

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by rtorres12 on 12/10/15.
  */
case class DummyCell() extends Cell {
  override var temperature = 1000.0
  override var prev: Double = temperature
  override var composition : Composition = Composition.randomComposition
  override val location: Location = Location(5,5)

  override def previousTemp: Double = prev

  override def updateTemp(): Double = 0.0

  override var cellType: String = CENTER_CELL
  override val table: ListBuffer[ListBuffer[Cell]] = ListBuffer[ListBuffer[Cell]]()
  override val neighbors: ArrayBuffer[Cell] = ArrayBuffer[Cell]()
}

object DummyCell {
  def generateDummyRow(n : Int) : scala.collection.immutable.IndexedSeq[DummyCell] = {
    val r = for(i <- 0 until n) yield {DummyCell()}
    r
  }
}
