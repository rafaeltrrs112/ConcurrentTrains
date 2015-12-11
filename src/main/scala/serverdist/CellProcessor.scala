package serverdist
/**
  * Created by rtorres12 on 12/9/15.
  */
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import cross._
import cross.HeatConstants._
import cross.cellutils.Calc._
import cross.cellutils.Composition
import io.backchat.hookup.{TextMessage, BinaryMessage, JsonMessage}
import org.json4s.JsonAST.{JArray, JDouble}
import scala.collection.immutable
import scala.collection.mutable._
import scala.util.{Failure, Random, Success}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait Config
case class Top() extends Config
case class Bottom() extends Config
class CellProcessor(val numberOfRows : Int, val numberOfColumns : Int, val config: Config)(implicit val serv : HeatNet) {

  val alloy = ListBuffer[ListBuffer[Cell]]()

  val counter = new AtomicInteger(0)

  val INIT = -998
  val START = -999

  //Dummy's just hold temperature

  val borderNeighbors : immutable.IndexedSeq[DummyCell] = DummyCell.generateDummyRow(numberOfColumns)

  createTable()
  solidify()


  def solidify() = {
    config match {
      case Top() => {
        alloy.last.last.temperature = 0.0
        alloy.last.last.prev = 0.0
        val neighIt = borderNeighbors.iterator
      }
      case Bottom() => {
        alloy.head.head.temperature = 0.0
        alloy.head.head.prev = 0.0
        val neighIt = borderNeighbors.iterator
      }
    }
  }

  def createTable() : Unit = {
    alloy ++= (for (row <- 0 until numberOfRows) yield {

      val newRow = ListBuffer[Cell]()

      newRow ++= (for (column <- 0 until numberOfColumns) yield {
        val cellBuild = (location: Location) => NgCell(location, Composition.randomComposition, 1.0, alloy, config)
        val preCell = cellBuild(Location(column, row))
        cellutils.Identity.setBorderType(preCell, numberOfRows, numberOfColumns, config)
        preCell
      })

      val it = borderNeighbors.iterator

      config match {
        case Top()    => if(row == numberOfRows - 1) newRow.foreach(_.neighbors += it.next())
        case Bottom() => if(row == 0) newRow.foreach(_.neighbors += it.next())
      }
      newRow
    })

    alloy.flatten.foreach {
      case ng: NgCell => ng.updateNeighbors()
      case _ =>
    }

    config match {
      case Top()    => alloy.last.last.cellType = CENTER_CELL
      case Bottom() => alloy.head.head.cellType = CENTER_CELL
    }
  }


  def randomTriple: (Double, Double, Double) = {
    val firstRandom: Double = Random.nextDouble() * 100
    val secondRandom: Double = Random.nextDouble * 100
    val thirdRandom: Double = Random.nextDouble * 100

    val total: Double = firstRandom + secondRandom + thirdRandom

    (firstRandom / total, secondRandom / total, thirdRandom / total)
  }

  def isNotCorner(cell: Cell): Boolean = {
    val res = !(cell.cellType.equals(UPPER_LEFT) || cell.cellType.equals(LOWER_RIGHT))
    res
  }

  def calcRowsTemp(rows : ListBuffer[ListBuffer[Cell]]) : Future[ListBuffer[Double]] = {
    Future {
      rows.flatten.filter(isNotCorner).map(calculateNewTemp)
    }
  }

  def initTimer() : Unit = {}

  def calcTotal = alloy.grouped(Runtime.getRuntime.availableProcessors()).toList.map(calcRowsTemp)

  //Called only when a message is received requesting another job be done.
  def start() : Future[List[ListBuffer[Double]]] = {
    val total : List[Future[ListBuffer[Double]]] = calcTotal
    val allDone = Future sequence total
    allDone onComplete {
      case Success(list) =>
        println(counter.incrementAndGet())
        if(counter.get() % 30 == 0) {
          alloy.flatten.toList.foreach(_.updateTemp())
          println(updateMessage().mkString(","))
          serv.sendImg()
        } else serv.sendBorders()
      case Failure(fail) => throw fail
    }
    allDone
  }

  def updateMessage() : Array[Double] = {
    alloy.flatten.map(_.temperature).toArray
  }

  def initData : List[Double] = {
    config match {
      case Top()    => alloy.last.flatMap(_.serialize).toList
      case Bottom() => alloy.head.flatMap(_.serialize).toList
    }
  }

  def initMessage : Array[Double] = {
    initData.toArray
  }

  def borderMessage : Array[Double] = {
    config match {
      case Top()    => alloy.last.map(_.temperature).toArray
      case Bottom() => alloy.head.map(_.temperature).toArray
    }
  }

  def calculateNewTemp(cell: Cell): Double = {
    val vibFuture: Double = vibraniumTotal(cell)
    val croFuture: Double = chromiumTotal(cell)
    val adaFuture: Double = adamantiumTotal(cell)

    cell.temperature = vibFuture + croFuture + adaFuture
    vibFuture + croFuture + adaFuture
  }

  def updateWithInit(body : Array[Double]) = {
    val infoList = body.grouped(4).toList
    val it = borderNeighbors.iterator
    infoList.foreach(initCell(it.next, _))
    start()
  }

  def initCell(cell : DummyCell, info : Array[Double]) : Unit = {
    val temperature = info(0)

    val adaPercent = info(1)
    val vibPercent = info(2)
    val croPercent = info(3)

    cell.temperature = temperature
    cell.prev = temperature

    cell.composition.adamantium.percentage = adaPercent
    cell.composition.adamantium.percentage = vibPercent
    cell.composition.adamantium.percentage = croPercent
  }

  def updateCellTemp(cell : DummyCell, t : Double) : Unit = {
    cell.temperature = t
    cell.prev = t
  }

  def normalUpdate(data : Array[Double]) : Unit = {
    val it = borderNeighbors.iterator
    data.foreach(updateCellTemp(it.next, _))
    start()
  }

}
