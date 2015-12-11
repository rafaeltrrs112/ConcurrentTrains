package client

import cross._
import HeatConstants._
import cross._
import cross.cellutils.Composition
import org.freedesktop.cairo.Context
import org.gnome.gdk.RGBA
import serverdist.Top

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

case class GuiCell(location : Location, defaultColor : RGBA, override var composition: Composition, var temperature : Double,
                   column : Int, row : Int, context : Context, table : ListBuffer[ListBuffer[Cell]]) extends Cell {

  var cellType = cellutils.Identity.setBorderType(this, 400, 100, Top())

  override lazy val neighbors : ArrayBuffer[Cell] = {
    cellutils.Identity.getNeighborIndices(this).map{(loc) =>
      table(loc.row)(loc.column)
    }.asInstanceOf[ArrayBuffer[Cell]]
  }

  var prev = temperature

  override def toString : String = {
    s"Cell(location : ${this.location}, defaultColor : ${this.defaultColor}, composition: ${this.composition}, temperature : ${this.previousTemp}, cellType : ${this.cellType})"
  }

  def color_=(bgColor : RGBA) = {
    context.setSource(bgColor.getRed, bgColor.getGreen, bgColor.getBlue, bgColor.getAlpha)
  }

  def previousTemp : Double = prev

  def updateTemp(): Double = {
    prev = temperature
    val split = (temperature / 2).toByte
    prev
  }

  def updateColor() : Boolean = {
    val colorRange = MAX_RATIO * temperature

    context.setSource(colorRange, 0.0, 0.0, 1.0)
    context.rectangle(column, row, 1, 1)
    context.fill()

    true
  }
}
