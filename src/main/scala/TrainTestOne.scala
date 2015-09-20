package sample.hello

import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.Actor.Receive
import akka.actor._
import scalafx._
import scala.collection.immutable

case class Number(number : Int)
case class Job()
case class Seat(direction : Int, name: String)
case class Close()
case class Seated(passenger : String)
case class Batch(batch : immutable.Queue[Seat])
case class Done(done : Int)
object Worker{
  def props(assignedDoor: String, maxOccupancy : Int, direction : Int) : Props = Props(new Worker(assignedDoor, maxOccupancy, direction))
}


class Worker(val assignedDoor : String, val maxOccupancy : Int, assignedDirection : Int) extends Actor {
  val lineQueue = scala.collection.mutable.Queue[Seat]()
  val trainQueue = immutable.Queue[Seat]()
  //get count message and print it out
  def receive = {
    case seat : Seat =>
      if(trainQueue.size < maxOccupancy) {
        println("Sending " + seat.name + " to " + assignedDoor)
        trainQueue.enqueue(seat)
        trainQueue.foreach(println(_))
      }
      else{
        println("Sending " + seat.name + " to " + assignedDoor)
        lineQueue.enqueue(seat)
        lineQueue.foreach(println(_))
        sender ! Close
      }
    case Batch(passengers) =>{
      passengers.foreach((passenger) => {
        println("Sending " + passenger.name + " to " + assignedDoor)
        lineQueue.enqueue(passenger)
        sender ! Done(assignedDirection)
      })
    }
  }
}



object TrainStation{
  val NORTH = 0
  val SOUTH = 1
}
class TrainStation extends Actor{
  val NORTH = 0
  val SOUTH = 1
  var number = 0

  var northDone = new AtomicBoolean(false)
  var southDone = new AtomicBoolean(false)

  //A router aka akka executor service maintains a thread/actor pool of
  //4 and assigns them jobs round robin style
  val workerOne = context.actorOf(Worker.props("North", 2, TrainStation.NORTH), "northWorker")
  val workerTwo = context.actorOf(Worker.props("South", 2, TrainStation.SOUTH), "southWorker")
  override def receive = {
    case  Seat(direction, name) => direction match {
      case NORTH => workerOne ! Seat(direction, name)
      case SOUTH => workerTwo ! Seat(direction, name)
    }
    case Close =>
      sender ! Close
    case batch : Batch => {
      println("received")
      val northernBatch = batch.batch.filter( _.direction == TrainStation.NORTH)
      val southernBatch = batch.batch.filter( _.direction == TrainStation.SOUTH)
      workerOne ! Batch(northernBatch)
      workerTwo ! Batch(southernBatch)
    }
    case Done(done) => {
      if(done == TrainStation.NORTH) northDone.set(true)
      else if(done == TrainStation.SOUTH) southDone.set(true)
      if(northDone.get() && southDone.get()){
        context.parent ! Close
      }
    }
  }
}

class Listener extends Actor {

  var systemOpen : AtomicBoolean = new AtomicBoolean(true)
  val master = context.actorOf(Props(new TrainStation), name = "StationSystem")

  //Create a router that will hold up to four worker threads
  //Create a worker actor that will print out number
  override def receive: Receive = {
    case Close => context.system.shutdown()
    case batch : Batch => {
      System.out.println("Received")
      master ! batch
    }
  }
}
object TrainTestOne extends App {
  val system = ActorSystem("TrainTest")

  val seats = immutable.Queue[Seat](Seat(TrainStation.SOUTH, "Ron"), Seat(TrainStation.NORTH, "Jon")
                                   ,Seat(TrainStation.SOUTH, "Gone"), Seat(TrainStation.NORTH, "Bomb"))

  seats.foreach((thing) => thing.name)
  val listener = system.actorOf(Props(new Listener), name = "Listener")
  listener ! Batch(seats)
}