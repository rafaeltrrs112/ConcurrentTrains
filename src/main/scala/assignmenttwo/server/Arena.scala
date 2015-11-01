package assignmenttwo.server

import javafx.beans.binding.DoubleBinding

import assignmenttwo.attacks.Attack
import assignmenttwo.pokemon._
import myactors.{Actor, ActorMessage}

import scala.util.Random
import scalafx.beans.property.{ReadOnlyDoubleProperty, DoubleProperty}
import scalafx.scene.control.ProgressBar

/**
 * An arena where pokemon fight.
 * @param name
 *             The  name of the Arena
 * @param one
 *             A contestant.
 * @param two
 *            Another contestant.
 */
class Arena(override val name : String, val one : Pokemon, val two : Pokemon, bars : (ProgressBar, ProgressBar)) extends Actor {
  def start() : Unit = {
    Seq(() => two.thisRef ! ActorMessage(one.attackBack, thisRef), () => one.thisRef ! ActorMessage(two.attackBack, thisRef))(Random.nextInt(2))()
  }
  override def onReceive(message: ActorMessage): Unit = {
    message match {
      case ActorMessage(attack : Attack, attacker) => {
        //Manage the damage between the two.
        if(one.amAlive & two.amAlive){ //if they both are alive then make them attack each other.
          attacker.name match {
            case one.name => {
              two.thisRef ! ActorMessage(attack, thisRef)
              Thread.sleep(1000)
            }
            case two.name => one.thisRef ! ActorMessage(attack, thisRef)
              Thread.sleep(1000)
            }
          println(s"${one.toString}\n${two.toString}\n")
        }
      }
    }
  }
}

object testArena extends App{
}
