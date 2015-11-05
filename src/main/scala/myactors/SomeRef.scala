package myactors

/**
 */
case class SomeRef(actor : ActorLike) extends ActorRef {
  override val name = actor.name
  //Receive method actually calls the actors onReceive
  override def !(message : ActorMessage) : Unit = {
    utils.execute{
      this.actor.inbox.send(ActorMessage(message.message, message.sender))
    }
  }
}
