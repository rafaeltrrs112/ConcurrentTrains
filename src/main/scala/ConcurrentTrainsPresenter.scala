import java.util.concurrent.atomic.AtomicBoolean
import akka.actor._
import util.RandomName._
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe.typeOf
import scala.util.Random
import scalafx.Includes._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import scalafxml.core.macros.sfxml
import scalafxml.core.{DependenciesByType, FXMLView}

case class Send()

case class Number(number: Int)

case class Job()

case class Seat(direction: Int, name: String)

case class Close()

case class Seated(passenger: String)

case class Batch(batch: scala.collection.mutable.Queue[Seat])

case class Done(done: Int)

object Worker {
  def props(assignedDoor: String, maxOccupancy: Int, direction: Int,
            label: Label, circle: Circle, countLabel: Label,
            button: Button): Props =
    Props(new Worker(assignedDoor, maxOccupancy, direction, label, circle, countLabel, button))
}

/**
 * @param assignedDoor
 * Assigned door string for printing out messages on the console
 * @param maxOccupancy
 * The maximum amount of people the worker will allow on the train before it sends the train off
 * and starts and diverts the remaining people to the line on the platform.
 * @param assignedDirection
 * The train direction the worker is in charge of
 * @param label
 * The label the worker is bound to
 * @param circle
 * The circle indicator that display red if the train is gone or green if it in the station and boarding
 * people
 * @param countLabel
 * The label that contains the number of people on the train
 *
 * @param button
 * The button that triggers the send message to the worker actors so the train is sent away went the button
 * is pressed.
 */
class Worker(val assignedDoor: String, val maxOccupancy: Int, assignedDirection: Int, label: Label, circle: Circle, countLabel: Label,
             button: Button)
  extends Actor {

  val lineQueue = scala.collection.mutable.Queue[Seat]()
  val trainQueue = scala.collection.mutable.Queue[Seat]()

  var onHold: AtomicBoolean = new AtomicBoolean(false)

  var goneTime = System.currentTimeMillis() / 1000

  def currentTime = System.currentTimeMillis() / 1000

  //get count message and print it out
  def receive = {
    //Not sure if workers should handle a batch of people or a bunch of people all at once...hmmm
    case Batch(passengers) => {
      if (onHold.get()) {
        println("Checking train")
        println("The difference in time is " + (currentTime - goneTime))
        if ((currentTime - goneTime) > 5) {
          if (onHold.compareAndSet(true, false)) {
            println("The Train is back passengers are getting of!!!")
            Platform.runLater {
              circle.fill = Color.Green
            }
            trainQueue.clear()
            lineQueue.foreach(trainQueue.enqueue(_))
            lineQueue.clear()
            Platform.runLater {
              countLabel.text.set(trainQueue.size.toString)
            }
          }
        }
      }
      passengers.foreach((passenger) => {
        if (trainQueue.size < maxOccupancy && !onHold.get()) {
          println("Sending " + passenger.name + " to " + assignedDoor)
          Platform.runLater {
            label.text.set("Sending " + passenger.name + " to " + assignedDoor)
          }
          trainQueue.enqueue(passenger)
          Platform.runLater {
            countLabel.text.set(trainQueue.size.toString)
          }
        } else {
          //If
          println("Platform at dangerous capacity!! " + passenger.name + " to the waiting line on platform " + assignedDoor)
          Platform.runLater {
            circle.fill = Color.Red
          }
          Platform.runLater {
            label.text.set("Sending " + passenger.name + " to the waiting line on platform " + assignedDoor)
          }
          lineQueue.enqueue(passenger)
          if (onHold.compareAndSet(false, true)) {
            goneTime = System.currentTimeMillis() / 1000
          }
        }
      })
    }
    case Send() =>
      println("Send")
      Platform.runLater {
        if (onHold.compareAndSet(false, true)) {
          goneTime = System.currentTimeMillis() / 1000
          circle.fill = Color.Red
        }
      }
  }
}

object TrainStation {
  val NORTH = 0
  val SOUTH = 1
}

class TrainStation extends Actor {
  val workerOne = context.actorOf(Worker.props("North", 8, TrainStation.NORTH,
    View.labelMap(TrainStation.NORTH), View.circleMap(TrainStation.NORTH), View.countMap(TrainStation.NORTH),
    View.dispatchMap(TrainStation.NORTH)), "northWorker")

  val workerTwo = context.actorOf(Worker.props("South", 8, TrainStation.SOUTH,
    View.labelMap(TrainStation.SOUTH), View.circleMap(TrainStation.SOUTH), View.countMap(TrainStation.SOUTH),
    View.dispatchMap(TrainStation.SOUTH)), "southWorker")

  val southButton = View.dispatchMap(TrainStation.SOUTH)
  southButton.setOnAction({
    (_: ActionEvent) => {
      workerTwo ! Send()
    }
  })

  val northButton = View.dispatchMap(TrainStation.NORTH)
  northButton.setOnAction({
    (_: ActionEvent) => {
      workerOne ! Send()
    }
  })

  override def receive = {
    case batch: Batch => {
      //Here the train station receives the people coming in and sorts them by desired travel direction
      val northernBatch = batch.batch.filter(_.direction == TrainStation.NORTH)
      val southernBatch = batch.batch.filter(_.direction == TrainStation.SOUTH)
      workerOne ! Batch(northernBatch)
      workerTwo ! Batch(southernBatch)
      batch.batch.clear()
    }
  }
}

object View {
  var labelMap = scala.collection.mutable.Map[Int, Label]()
  var circleMap = scala.collection.mutable.Map[Int, Circle]()
  var countMap = scala.collection.mutable.Map[Int, Label]()
  var dispatchMap = scala.collection.mutable.Map[Int, Button]()
}

class Listener extends Actor {

  var systemOpen: AtomicBoolean = new AtomicBoolean(true)

  //Create a router that will hold up to four worker threads
  //Create a worker actor that will print out number
  override def receive: Receive = {
    case Close =>
      println("Closing system")
      context.system.shutdown()
  }
}

/**
 *
 * @param northDispatch
 * Dispatch button for the north
 * @param northPassenger
 * Passenger label for the north
 * @param northStatus
 * Status circle icon for the north
 * @param northCount
 * Count label for the north train poplulation
 * @param southDispatch
 * Dispatch button for the south
 * @param southPassenger
 * Passenger label for the south
 * @param southStatus
 * Status circle icon for the south
 * @param southCount
 * Count label for the south train poplulation
 */
@sfxml
class ConcurrentTrainsPresenter(private val northDispatch: Button,
                                private val northPassenger: Label,
                                private val northStatus: Circle,
                                private val northCount: Label,
                                private val southDispatch: Button,
                                private val southPassenger: Label,
                                private val southStatus: Circle,
                                private val southCount: Label
                                 ) {

  //Map containing the labels that stores the passenger boarding
  View.labelMap = scala.collection.mutable.Map(TrainStation.NORTH -> northPassenger, TrainStation.SOUTH -> southPassenger)
  //Map containing the status icons
  View.circleMap = scala.collection.mutable.Map(TrainStation.NORTH -> northStatus, TrainStation.SOUTH -> southStatus)
  //Map containing the count of people on the train
  View.countMap = scala.collection.mutable.Map(TrainStation.NORTH -> northCount, TrainStation.SOUTH -> southCount)
  //Map containing the buttons used for dispatching trains
  View.dispatchMap = scala.collection.mutable.Map(TrainStation.NORTH -> northDispatch, TrainStation.SOUTH -> southDispatch)

  //Sleep the thread while the UI builds and binds
  Thread.sleep(4000)

  val system = ActorSystem("TrainTest")
  //The listener waits for the message to shut down the whole system
  val listener = system.actorOf(Props(new Listener), name = "Listener")
  //This queue holds the queue of random people
  val people = scala.collection.mutable.Queue[Seat]()
  val master = system.actorOf(Props(new TrainStation), name = "StationSystem")


  //Thread that sends people up to the train randomly, alternating between north and south
  new Thread() {
    override def run(): Unit = {
      while (true) {
        Thread.sleep(2000)
        val randomDirection = Random.shuffle(ArrayBuffer(TrainStation.NORTH, TrainStation.SOUTH)).head
        val newPassengers = for (i <- 1 to Random.nextInt() % 4) yield Seat(randomDirection, randomName)
        newPassengers.foreach(people.enqueue(_))
        master ! Batch(people)
      }
    }
  }.start()
}

//Java fx app starter object.
object ConcurrentTrainsFXML extends JFXApp {
  val root = FXMLView(getClass.getResource("concurrentrains.fxml"),
    new DependenciesByType(Map(
      typeOf[UnitConverters] -> new UnitConverters(InchesToMM, MMtoInches))))

  stage = new PrimaryStage() {
    title = "Unit conversion"
    scene = new Scene(root)
  }
}