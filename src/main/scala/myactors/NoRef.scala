package myactors

/**
 * Created by rtorres12 on 10/26/15.
 */
case class NoRef(actor : Actor) extends ActorRef {
  override val name = "NoRef"
  //Receive method does nothing
  override def !(message : ActorMessage) : Unit = Unit
}
