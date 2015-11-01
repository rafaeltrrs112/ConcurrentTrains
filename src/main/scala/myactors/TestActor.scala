package myactors

class TestActor(override val name : String) extends Actor {
  override def onReceive(message : ActorMessage): Unit = {
    message match {
      case ActorMessage("GoodBye!", sender) => utils.execute{
        println(this.name + " received good bye from " + sender.name)
        message.sender ! ActorMessage("Hello!", thisRef)
      }
      case ActorMessage("Hello!", sender) => utils.execute {
        println(this.name + " received hello from " + sender.name)
        message.sender ! ActorMessage("GoodBye!", thisRef)
      }
    }
  }
}
