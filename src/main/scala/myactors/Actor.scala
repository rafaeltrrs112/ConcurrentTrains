package myactors


trait ActorLike {
  val name : String
  val inbox : Inbox[Object]
  def onReceive(msg : ActorMessage) : Unit
  val thisRef = SomeRef(this)
}

trait Actor extends ActorLike {
  val name : String
  private val innerQueue = new ConcurrentQueue[ActorMessage]()

  val inbox : Inbox[Object] = new Inbox[Object](innerQueue)

  val logger = new Thread {
    setDaemon(true)
    override def run() = {
      while (true) {
        /*
         * Start a daemon that constantly calls dequeue from the inbox.
         * If the dequeue returns some value then call onReceive.
         */
        val msg = innerQueue.dequeue()
        if(msg.isDefined) onReceive(msg.get)
      }
    }
  }

  logger.start()

  def onReceive(message : ActorMessage) : Unit
}

trait BetterActor {
  val name : String
  private val innerQueue = new java.util.concurrent.ConcurrentLinkedQueue[ActorMessage]()

  val inbox : BetterInbox[Object] = new BetterInbox[Object](innerQueue)

  val logger = new Thread {
    setDaemon(true)
    override def run() = {
      while (true) {
        /*
         * Start a daemon that constantly calls dequeue from the inbox.
         * If the dequeue returns some value then call onReceive.
         */
        val msg = Option(innerQueue.poll())
        if(msg.isDefined) onReceive(msg.get)
      }
    }
  }

  logger.start()

  def onReceive(message : ActorMessage) : Unit
}