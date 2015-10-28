package assignmenttwo.pokemon

import assignmenttwo.attacks._
import myactors.{Actor, ActorMessage}

abstract class Pokemon(override val name : String, override var healthPoints : Int, override val attack : Int) extends Actor with TypedPokemon {
  override def onReceive(message: ActorMessage): Unit = {
     message match {
       case ActorMessage(incomingAttack : Attack, sender) => receiveDamage(incomingAttack)
         if(amAlive) {
           sender ! ActorMessage(attackBack, thisRef)
//           println(s"$name is attacking back!")
         }
         else{
           println(s"$name was dealt a critical blow...dying....!")

         }
     }
   }
   override def receiveDamage(attack: Attack): Unit

   override def toString : String = {
     "Name: " + name + " HP: " + healthPoints
   }
}

case class Zapdos(override val name : String, var health : Int, override val attack : Int) extends Pokemon(name, health, attack) with ElectricType {
  override def onReceive(message: ActorMessage): Unit = {
    message match {
      case ActorMessage(incomingAttack : Attack, sender) => receiveDamage(incomingAttack)
        if(amAlive) {
          sender ! ActorMessage(attackBack, thisRef)
//          println(s"$name is attacking back!")
        }
        else{
          println(s"$name was dealt a critical blow...dying....!")

        }
    }
  }
}

case class Garydos(override val name : String, health : Int, override val attack : Int) extends Pokemon(name, health, attack) with WaterType {
  override def onReceive(message: ActorMessage): Unit = {
    message match {
      case ActorMessage(incomingAttack : Attack, sender) => receiveDamage(incomingAttack)
        if(amAlive) {
          sender ! ActorMessage(attackBack, thisRef)
//          println(s"$name is attacking back!")
        }
        else{
          println(s"$name was dealt a critical blow...dying....!")

        }
    }
  }
}

case class Flareon(override val name : String, health : Int, override val attack : Int) extends Pokemon(name, health, attack) with FireType {
  override def onReceive(message: ActorMessage): Unit = {
    message match {
      case ActorMessage(incomingAttack : Attack, sender) => receiveDamage(incomingAttack)
        if(amAlive) {
          sender ! ActorMessage(attackBack, thisRef)
//          println(s"$name is attacking back!")
        }
        else{
          println(s"$name was dealt a critical blow...dying....!")

        }
    }
  }
}

case class Leafeon(override val name : String, health : Int, override val attack : Int) extends Pokemon(name, health, attack) with WoodType {
  override def onReceive(message: ActorMessage): Unit = {
    message match {
      case ActorMessage(incomingAttack : Attack, sender) => receiveDamage(incomingAttack)
        if(amAlive) {
          sender ! ActorMessage(attackBack, thisRef)
//          println(s"$name is attacking back!")
        }
        else{
          println(s"$name was dealt a critical blow...dying....!")
        }
    }
  }
}