package sample.hello

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import _root_.util.RandomName._
import examples.ScalaFXHelloWorld._
import scala.collection.immutable.HashMap
import scala.concurrent.duration.Duration
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable
import scalafx._
import akka.actor.Actor.Receive
import akka.actor._

import scalafx.Includes._
import scalafx.application.{Platform, JFXApp}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.Platform.runLater
import scalafx.beans.property.StringProperty
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{HBox, BorderPane}

case class Number(number : Int)
case class Job()
case class Seat(direction : Int, name: String)
case class Close()
case class Seated(passenger : String)
case class Batch(batch : scala.collection.mutable.Queue[Seat])
case class Done(done : Int)
object Worker{
  def props(assignedDoor: String, maxOccupancy : Int, direction : Int) : Props = Props(new Worker(assignedDoor, maxOccupancy, direction))
  def SouthernWorker(context : ActorContext) : ActorRef = {
    context.actorOf(Worker.props("North", 8, TrainStation.NORTH), "northWorker")
  }
  def NorthernWorker(context: ActorContext) : ActorRef = {
    context.actorOf(Worker.props("South", 8, TrainStation.SOUTH), "southWorker")
  }
}


class Worker(val assignedDoor : String, val maxOccupancy : Int, assignedDirection : Int) extends Actor {
  val lineQueue = scala.collection.mutable.Queue[Seat]()
  val trainQueue = scala.collection.mutable.Queue[Seat]()
  var onHold : AtomicBoolean = new AtomicBoolean(false)
  //get count message and print it out
  def receive = {
      //Not sure if workers should handle a batch of people or a bunch of people all at once...hmmm
    case Batch(passengers) => {
      passengers.foreach((passenger) => {
        if(trainQueue.size < maxOccupancy){
          println("Sending " + passenger.name + " to " + assignedDoor)
          Platform.runLater{
            TrainTestOne.labelMap(assignedDirection).text.set("Sending " + passenger.name + " to " + assignedDoor)
          }
          trainQueue.enqueue(passenger)
        }else{
          println("Sending " + passenger.name + " to the waiting line on platform " + assignedDoor)
          Platform.runLater{
            TrainTestOne.labelMap(assignedDirection).text.set("Sending " + passenger.name + " to the waiting line on platform " + assignedDoor)
          }
          lineQueue.enqueue(passenger)
          onHold.set(true)
          sender ! Done(assignedDirection)
        }
        Thread.sleep(1000)
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
  val workerOne = context.actorOf(Worker.props("North", 8, TrainStation.NORTH), "northWorker")
  val workerTwo = context.actorOf(Worker.props("South", 8, TrainStation.SOUTH), "southWorker")
  override def receive = {
    case batch : Batch => {
      //Here the train station receives the people coming in and sorts them by desired travel direction
      val northernBatch = batch.batch.filter( _.direction == TrainStation.NORTH)
      val southernBatch = batch.batch.filter( _.direction == TrainStation.SOUTH)
      workerOne ! Batch(northernBatch)
      workerTwo ! Batch(southernBatch)
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
object TrainTestOne extends JFXApp {
  val system = ActorSystem("TrainTest")
  //The listener waits for the message to shut down the whole system
  val listener = system.actorOf(Props(new Listener), name = "Listener")
  //This queue holds the queue of random people
  val people = scala.collection.mutable.Queue[Seat]()
  val master = system.actorOf(Props(new TrainStation), name = "StationSystem")
  val testLabel = new Label{
    text = "Empty"
  }
  val northLabel = new Label(){
    text = "North"
  }
  val southLabel = new Label(){
    text = "South"
  }
  val centerButton = new Button(){
    text = "Click to Send People"
    onMouseClicked = (me: MouseEvent) => {
      runLater{
        (for(i <- 1 to 10 ) yield (Seat(Random.shuffle(ArrayBuffer(TrainStation.NORTH, TrainStation.SOUTH)).head, randomName))).foreach(people.enqueue(_))
        master ! Batch(people)
      }
    }

  }
  val labelMap = HashMap(TrainStation.NORTH -> northLabel, TrainStation.SOUTH -> southLabel)

  val hBox = new HBox(){
    spacing = 10
    content = List(
    northLabel,
    centerButton,
    southLabel
    )
  }
  stage = new PrimaryStage {
    title = "ScalaFX Hello World"
    scene = new Scene(){
      content = hBox
    }
  }

}