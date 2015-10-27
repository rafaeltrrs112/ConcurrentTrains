package myactors

/**
 * A wrapper of an Actor's reference.
 */
trait ActorRef {
  val name : String
  //Receive method
  def !(message : ActorMessage) : Unit
}
