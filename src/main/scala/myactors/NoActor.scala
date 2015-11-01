package myactors


case class NoActor(override val name : String) extends Actor{
  override def onReceive(message : ActorMessage): Unit = {}
}
