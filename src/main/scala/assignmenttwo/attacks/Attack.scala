package assignmenttwo.attacks

/**
 * A trait for an attack containing a damage property.
 */
sealed trait Attack {
  val damage : Int
}

/**
 * A water attack.
 * @param damage
 *               The amount of damage dealt by this attack.
 */
case class Water(override val damage : Int) extends Attack
/**
 * A fire attack.
 * @param damage
 *               The amount of damage dealt by this attack.
 */
case class Fire(override val damage : Int) extends Attack
/**
 * A wood attack.
 * @param damage
 *               The amount of damage dealt by this attack.
 */
case class Wood(override val damage : Int) extends Attack

/**
 * The electricity attack.
 * @param damage
 *             The amount of damage dealt by this attack.
 */
case class Electricity(override val damage : Int) extends Attack

trait TypedPokemon {
  def amAlive : Boolean = if(healthPoints > 0){
    true
  }
  else {
    healthPoints = 0
    false
  }
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

trait WoodType extends TypedPokemon {
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
