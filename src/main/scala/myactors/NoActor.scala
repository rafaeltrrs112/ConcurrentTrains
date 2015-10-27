package myactors

/**
 * Created by rtorres12 on 10/26/15.
 */
case class NoActor(override val name : String) extends Actor{
  override def onReceive(message : ActorMessage): Unit = {}
}
