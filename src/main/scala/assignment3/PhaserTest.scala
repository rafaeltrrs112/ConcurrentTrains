package assignment3

import scala.concurrent._
import ExecutionContext.Implicits.global
import java.util.concurrent.Phaser
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration

/**
  * Created by rtorres12 on 11/21/15.
  */
object PhaserTest extends App {
  //Create a list and fill it with two tasks that do something.
  val tasks : ListBuffer[() => Unit] = ListBuffer[() => Unit]()

  for(i <- Range(0, 10)){
    println('*')
    val runnable = () => {
      println("task running")
      var a : Int = 0
      var b : Int = 1
      for(i <- 1 to 200000000){
        a = a + b
        b = a - b
      }
    }
    tasks += runnable
  }

  def runTasks() : Unit = {
    /* Phaser argument is the number of parties required to advance the
     * phaser.
     * */
//    val phaser : Phaser = new Phaser(1){
//      override def onAdvance(phase : Int, registeredParties : Int) : Boolean = {
//        println("On advance!!!!!!!!!!!!!!!!!!!!!!" + " -> Registered: " + getRegisteredParties + " - Unarrived: "
//          + getUnarrivedParties + " - Arrived: " + getArrivedParties + " - Phase: " + getPhase)
//        phase >= 1 || registeredParties == 0
//      }
//    }
    //dumpPhaserState("After phaser init", phaser)
    val futuresList : ListBuffer[Future[Unit]] = tasks.map{ (task)  =>
      //Register the task with the phaser.
      //phaser.register()
      //dumpPhaserState("After register", phaser)
      val f  = Future[Unit] {
        {
          //do {
            //phaser.arriveAndAwaitAdvance()
            task()
          //} /*while(!phaser.isTerminated)*/
        }
      }
      //dumpPhaserState("After arrival", phaser)
      f
    }
    val taskFutures : Future[ListBuffer[Unit]] = Future sequence futuresList
    Await.result(taskFutures, Duration.Inf)
    /**
      * When the final party for a given phase arrives, onAdvance() is invoked and the phase advances. The
      * "face advances" means that all threads reached the barrier and therefore all threads are synchronized and can
      * continue processing.
      */
    //dumpPhaserState("Before main thread arrives and deregisters", phaser)

    /**
      * The arrival and deregistration of the main thread allows the other threads to start working. This is because
      * now the registered parties equal the arrived parties.
      */
    //phaser.arriveAndDeregister()
    //dumpPhaserState("After main thread arrived and deregistered", phaser)
    println("Main thread will terminate...")
    //println(phaser.getPhase)
  }

  def dumpPhaserState(when : String, phaser : Phaser) : Unit =  {
    println(when + " -> Registered: " + phaser.getRegisteredParties + " - Unarrived: "
      + phaser.getUnarrivedParties + " - Arrived: " + phaser.getArrivedParties + " - Phase: "
      + phaser.getPhase)
  }
  runTasks()
}
