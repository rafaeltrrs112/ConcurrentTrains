package myactors

/**
 * An actor trait with only two abstract properties :
 * it's name, it's
 */
trait Actor {
  val name : String
  private val innerQueue = new ConcurrentQueue[ActorMessage]()

  val inbox : Inbox[Object] = new Inbox[Object](innerQueue)

  implicit val thisRef = SomeRef(this)

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
