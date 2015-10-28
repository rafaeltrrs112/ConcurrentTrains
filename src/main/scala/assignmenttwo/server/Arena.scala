package assignmenttwo.server

import assignmenttwo.attacks.Attack
import assignmenttwo.pokemon._
import myactors.{Actor, ActorMessage, ActorRef}

import scala.util.Random

/**
 *
 * @param name
 *             The  name of the Arena
 * @param one
 *             A contestant.
 * @param two
 *            Another contestant.
 */
class Arena(override val name : String, one : Pokemon, two : Pokemon) extends Actor {



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
//              println("One attacks two")
              Thread.sleep(1000)
            }
            case two.name => one.thisRef ! ActorMessage(attack, thisRef)
//              println("Two attacking attacking One")
              Thread.sleep(1000)
            }
          println(s"${one.toString}\n${two.toString}\n")
        }
      }
    }
  }
}

object testArena extends App{
  val a = new Arena("Test State", Flareon("Flareon", 10000, 500), Leafeon("Leafeon", 10000, 500))
  val a2 = new Arena("Test State", Zapdos("Zapdos", 10000, 500), Garydos("Garydos", 10000, 500))

  a.start()
  a2.start()

  Thread.sleep(100000)
}
