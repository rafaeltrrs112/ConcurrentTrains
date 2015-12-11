package cross.cellutils

import cross.Cell

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by rtorres12 on 12/9/15.
  */
object Calc {
  def vibraniumTemp(cell : Cell): Double = {
    val currentTemp: Double = cell.previousTemp
    val vibPercent: Double = cell.composition.vibranium.percentage
    currentTemp * vibPercent
  }

  def adamantiumTemp(cell: Cell): Double = {
    val currentTemp: Double = cell.previousTemp
    val adaPercent: Double = cell.composition.adamantium.percentage
    currentTemp * adaPercent
  }

  def chromiumTemp(cell: Cell): Double = {
    val currentTemp: Double = cell.previousTemp
    val croPercent: Double = cell.composition.chromium.percentage
    currentTemp * croPercent
  }

  def adamantiumTotal(cell: Cell): Double = {
    val adaList: ArrayBuffer[Double] = cell.neighbors.map {
      (neighbor) => {
        adamantiumTemp(neighbor)
      }
    }
    cell.composition.adamantium.thermalConstant * (adaList.sum / cell.neighbors.size)
  }

  def chromiumTotal(cell: Cell): Double = {
    val chroList: ArrayBuffer[Double] = cell.neighbors.map {
      (neighbor) => {
        chromiumTemp(neighbor)
      }
    }
    cell.composition.chromium.thermalConstant * (chroList.sum / cell.neighbors.size)
  }

  def vibraniumTotal(cell: Cell): Double = {
    val vibraCountList: ArrayBuffer[Double] = cell.neighbors.map {
      (neighbor) => {
        vibraniumTemp(neighbor)
      }
    }
    cell.composition.vibranium.thermalConstant * (vibraCountList.sum / cell.neighbors.size)
  }

}
