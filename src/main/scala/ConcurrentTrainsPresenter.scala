
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.parallel.mutable
import scala.reflect.runtime.universe.typeOf
import akka.actor._
import util.RandomName._

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scalafx.Includes._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.Platform.runLater
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.binding.StringBinding
import scalafx.beans.property.IntegerProperty
import scalafx.collections.ObservableMap
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import scalafxml.core.{DependenciesByType, FXMLView}
import scalafxml.core.macros.sfxml

case class Number(number : Int)
case class Job()
case class Seat(direction : Int, name: String)
case class Close()
case class Seated(passenger : String)
case class Batch(batch : scala.collection.mutable.Queue[Seat])
case class Done(done : Int)
object Worker{
  def props(assignedDoor: String, maxOccupancy : Int, direction : Int, label : Label, circle : Circle, countLabel: Label) : Props =
    Props(new Worker(assignedDoor, maxOccupancy, direction, label, circle, countLabel))
}
class Worker(val assignedDoor : String, val maxOccupancy : Int, assignedDirection : Int, label: Label, circle: Circle, countLabel : Label)
  extends Actor {

  val lineQueue = scala.collection.mutable.Queue[Seat]()
  val trainQueue = scala.collection.mutable.Queue[Seat]()

  var onHold : AtomicBoolean = new AtomicBoolean(false)

  var goneTime = System.currentTimeMillis() / 1000
  def currentTime = System.currentTimeMillis() / 1000

  //get count message and print it out
  def receive = {
      //Not sure if workers should handle a batch of people or a bunch of people all at once...hmmm
    case Batch(passengers) => {
      if(onHold.get()){
        println("Checking train")


        println("The difference in time is " + (currentTime-goneTime))


        if((currentTime - goneTime) > 5){


          println("The Train is back passengers are getting of!!!")
          onHold.set(false)
          Platform.runLater{
            circle.fill = Color.Green
          }
          trainQueue.dequeue()
        }
      }
      passengers.foreach((passenger) => {
        if(trainQueue.size < maxOccupancy && !onHold.get()){
          println("Sending " + passenger.name + " to " + assignedDoor)
          Platform.runLater{
            label.text.set("Sending " + passenger.name + " to " + assignedDoor)
          }
          trainQueue.enqueue(passenger)
        }else{
          //If
          println("Platform at dangerous capacity!! " + passenger.name + " to the waiting line on platform " + assignedDoor)
          Platform.runLater{
            circle.fill = Color.Red

          }
          Platform.runLater{
            label.text.set("Sending " + passenger.name + " to the waiting line on platform " + assignedDoor)
          }
          lineQueue.enqueue(passenger)
          if(!onHold.get()) {
            onHold.set(true)
            goneTime = System.currentTimeMillis() / 1000
          }
        }
      })
//      Right now the actors cannot stop enable Done message to return to normal functionality
//      sender ! Done(assignedDirection)
    }
  }
}

object TrainStation{
  val NORTH = 0
  val SOUTH = 1
}
class TrainStation extends Actor{

  var number = 0

  var northDone = new AtomicBoolean(false)
  var southDone = new AtomicBoolean(false)

  //A router aka akka executor service maintains a thread/actor pool of
  //4 and assigns them jobs round robin style

  val workerOne = context.actorOf(Worker.props("North", 8, TrainStation.NORTH,
  View.labelMap(TrainStation.NORTH), View.circleMap(TrainStation.NORTH), View.countMap(TrainStation.NORTH)), "northWorker")

  val workerTwo = context.actorOf(Worker.props("South", 8, TrainStation.SOUTH,
  View.labelMap(TrainStation.SOUTH), View.circleMap(TrainStation.SOUTH), View.countMap(TrainStation.SOUTH)),"southWorker")

  override def receive = {


    case batch : Batch => {
      //Here the train station receives the people coming in and sorts them by desired travel direction
      val northernBatch = batch.batch.filter( _.direction == TrainStation.NORTH)
      val southernBatch = batch.batch.filter( _.direction == TrainStation.SOUTH)
      workerOne ! Batch(northernBatch)
      workerTwo ! Batch(southernBatch)
      batch.batch.clear()
    }
    case Done(done) => {
      if(done == TrainStation.NORTH && !northDone.get()){
        northDone.set(true)
        println("North Train Full!!")
      }
      else if(done == TrainStation.SOUTH && !southDone.get()) {
        southDone.set(true)
        println("South Train Full!")
      }
    }
  }
}
object View{
  var labelMap = scala.collection.mutable.Map[Int, Label]()
  var circleMap = scala.collection.mutable.Map[Int, Circle]()
  var countMap = scala.collection.mutable.Map[Int, Label]()
}
class Listener extends Actor {

  var systemOpen : AtomicBoolean = new AtomicBoolean(true)

  //Create a router that will hold up to four worker threads
  //Create a worker actor that will print out number
  override def receive: Receive = {
    case Close => {
      println("Closing system")
      context.system.shutdown()
    }
  }
}

@sfxml
class ConcurrentTrainsPresenter(
                                 private val northDispatch: Button,
                                 private val northPassenger : Label,
                                 private val northStatus : Circle,
                                 private val northCount : Label,
                                 private val southDispatch: Button,
                                 private val southPassenger : Label,
                                 private val southStatus : Circle,
                                 private val southCount : Label
                                 ){
  val system = ActorSystem("TrainTest")
  //The listener waits for the message to shut down the whole system
  val listener = system.actorOf(Props(new Listener), name = "Listener")
  //This queue holds the queue of random people
  val people = scala.collection.mutable.Queue[Seat]()
  val master = system.actorOf(Props(new TrainStation), name = "StationSystem")

//  val centerButton = new Button(){
//    text = "Click to Kill"
//    onMouseClicked = (me: MouseEvent) => {
//      runLater{
//        println(people.size)
//        system.shutdown()
//      }
//    }
//  }
  View.labelMap = scala.collection.mutable.Map(TrainStation.NORTH -> northPassenger, TrainStation.SOUTH -> southPassenger)
  View.circleMap = scala.collection.mutable.Map(TrainStation.NORTH -> northStatus, TrainStation.SOUTH -> southStatus)
  View.countMap = scala.collection.mutable.Map(TrainStation.NORTH -> northCount, TrainStation.SOUTH -> southCount)
  Thread.sleep(2000)
  new Thread(){
    override def run(): Unit ={
      while(true){
        Thread.sleep(2000)
        val randomDirection = Random.shuffle(ArrayBuffer(TrainStation.NORTH, TrainStation.SOUTH)).head
        val newPassengers = for (i <- 1 to Random.nextInt() % 4) yield Seat(randomDirection, randomName)
        newPassengers.foreach(people.enqueue(_))
        master ! Batch(people)
      }
    }
  }.start()
}
object ConcurrentTrainsFXML extends JFXApp{
  val root = FXMLView(getClass.getResource("concurrentrains.fxml"),
    new DependenciesByType(Map(
      typeOf[UnitConverters] -> new UnitConverters(InchesToMM, MMtoInches))))

  stage = new PrimaryStage(){
    title = "Unit conversion"
    scene = new Scene(root)
  }
}