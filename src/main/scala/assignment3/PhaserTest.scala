package assignment3

import scala.concurrent._
import ExecutionContext.Implicits.global
import java.util.concurrent.Phaser
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

/**
  * Created by rtorres12 on 11/21/15.
  */
object PhaserTest extends App {


  def dumpPhaserState(when : String, phaser : Phaser) : Unit =  {
    println(when + " -> Registered: " + phaser.getRegisteredParties + " - Unarrived: "
      + phaser.getUnarrivedParties + " - Arrived: " + phaser.getArrivedParties + " - Phase: "
      + phaser.getPhase)
  }
}
