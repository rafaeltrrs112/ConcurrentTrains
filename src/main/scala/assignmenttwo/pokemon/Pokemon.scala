package assignmenttwo.pokemon

import javafx.beans.binding.DoubleBinding

import assignmenttwo.attacks._
import myactors.{Actor, ActorMessage}

import scalafx.beans.property.{ReadOnlyDoubleProperty, DoubleProperty}
import scalafx.scene.control.ProgressBar

abstract class Pokemon(override val name : String, override var healthPoints : Int, override val attack : Int, progressBar: ProgressBar) extends Actor with TypedPokemon {
  val startingHealth : Double = healthPoints
  override def onReceive(message: ActorMessage): Unit = {
     message match {
       case ActorMessage(incomingAttack : Attack, sender) => receiveDamage(incomingAttack)
         if(amAlive) {
           sender ! ActorMessage(attackBack, thisRef)
         }
         else{
           println(s"$name was dealt a critical blow...dying....!")
         }
         updateBar()
     }
   }
   override def receiveDamage(attack: Attack): Unit

   override def toString : String = {
     "Name: " + name + " HP: " + healthPoints
   }
  def updateBar() : Unit = {
    println(healthPoints.toDouble / startingHealth)
    progressBar.progress.set(healthPoints.toDouble / startingHealth.toDouble)
  }
}

case class Zapdos(override val name : String, var health : Int, override val attack : Int, progressBar: ProgressBar) extends Pokemon(name, health, attack, progressBar) with ElectricType {
  override def onReceive(message: ActorMessage): Unit = {
    message match {
      case ActorMessage(incomingAttack : Attack, sender) => receiveDamage(incomingAttack)
        if(amAlive) {
          sender ! ActorMessage(attackBack, thisRef)
        }
        else{
          println(s"$name was dealt a critical blow...dying....!")
        }
        updateBar()

    }
  }
}

case class Garydos(override val name : String, health : Int, override val attack : Int, progressBar: ProgressBar) extends Pokemon(name, health, attack, progressBar) with WaterType {
  override def onReceive(message: ActorMessage): Unit = {
    message match {
      case ActorMessage(incomingAttack : Attack, sender) => receiveDamage(incomingAttack)
        if(amAlive) {
          sender ! ActorMessage(attackBack, thisRef)
        }
        else{
          println(s"$name was dealt a critical blow...dying....!")
        }
        updateBar()

    }
  }
}

case class Flareon(override val name : String, health : Int, override val attack : Int, progressBar: ProgressBar) extends Pokemon(name, health, attack, progressBar) with FireType {
  override def onReceive(message: ActorMessage): Unit = {
    message match {
      case ActorMessage(incomingAttack : Attack, sender) => receiveDamage(incomingAttack)
        if(amAlive) {
          sender ! ActorMessage(attackBack, thisRef)
        }
        else{
          println(s"$name was dealt a critical blow...dying....!")
        }
        updateBar()

    }
  }
}

case class Leafeon(override val name : String, health : Int, override val attack : Int, progressBar: ProgressBar) extends Pokemon(name, health, attack, progressBar) with WoodType {
  override def onReceive(message: ActorMessage): Unit = {
    message match {
      case ActorMessage(incomingAttack : Attack, sender) => receiveDamage(incomingAttack)
        if(amAlive) {
          sender ! ActorMessage(attackBack, thisRef)
        }
        else{
          println(s"$name was dealt a critical blow...dying....!")
        }
        updateBar()

    }
  }
}