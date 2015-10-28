package assignmenttwo.attacks

import myactors.utils

import scala.util
import scala.util.Random
/**
 *
 */
sealed trait Attack {
  val damage : Int
}

case class Water(override val damage : Int) extends Attack
case class Fire(override val damage : Int) extends Attack
case class Wood(override val damage : Int) extends Attack
case class Electricity(override val damage : Int) extends Attack

trait TypedPokemon{
  def amAlive : Boolean = healthPoints > 0
  def willSurvive(dmg : Int) : Boolean = healthPoints - dmg > 0
  val attack : Int
  var healthPoints : Int
  def attackBack : Attack
  def receiveDamage(attack : Attack) : Unit
}

trait ElectricType extends TypedPokemon {
  var healthPoints : Int
  val attack : Int
  def attackBack : Attack = Electricity(scala.util.Random.nextInt(attack))
  def receiveDamage(attack : Attack) : Unit = {
    if(amAlive){
      attack match {
        case Wood(_) => if (willSurvive(attack.damage)) healthPoints = healthPoints - (attack.damage * 2) else healthPoints = 0
        case Water(_) => if (willSurvive(attack.damage)) healthPoints = healthPoints - (attack.damage / 2) else healthPoints = 0
        case a : Attack => if (willSurvive(attack.damage)) healthPoints = healthPoints - attack.damage else healthPoints = 0
      }
    }
  }
}

trait FireType extends TypedPokemon{
  var healthPoints : Int
  def attackBack : Attack = Fire(scala.util.Random.nextInt(attack))
  def receiveDamage(attack : Attack) : Unit = {
    if(amAlive){
      attack match {
        case Water(_) => if (willSurvive(attack.damage)) healthPoints = healthPoints - (attack.damage * 2) else healthPoints = 0
        case Wood(_) => if (willSurvive(attack.damage)) healthPoints = healthPoints - (attack.damage / 2) else healthPoints = 0
        case a : Attack => if (willSurvive(attack.damage)) healthPoints = healthPoints - attack.damage else healthPoints = 0
      }
    }
  }
}

trait WoodType extends TypedPokemon{
  var healthPoints : Int
  def attackBack : Attack = Wood(scala.util.Random.nextInt(attack))
  def receiveDamage(attack : Attack) : Unit = {
    if(amAlive){
      attack match {
        case Fire(_) => if (willSurvive(attack.damage)) healthPoints = healthPoints - (attack.damage * 2) else healthPoints = 0
        case Electricity(_) => if (willSurvive(attack.damage)) healthPoints = healthPoints - (attack.damage / 2) else healthPoints = 0
        case a : Attack => if (willSurvive(attack.damage)) healthPoints = healthPoints - attack.damage else healthPoints = 0      }
    }
  }
}

trait WaterType extends TypedPokemon{
  var healthPoints : Int
  def attackBack : Attack = Water(scala.util.Random.nextInt(attack))
  def receiveDamage(attack : Attack) : Unit = {
    if(amAlive){
      attack match {
        case Electricity(_) => if (willSurvive(attack.damage)) healthPoints = healthPoints - (attack.damage * 2) else healthPoints = 0
        case Fire(_) => if (willSurvive(attack.damage)) healthPoints = healthPoints - (attack.damage / 2) else healthPoints = 0
        case a : Attack => if (willSurvive(attack.damage)) healthPoints = healthPoints - attack.damage else healthPoints = 0      }
    }
  }
}
