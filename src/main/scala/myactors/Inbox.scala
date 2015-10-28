package myactors

/**
 */
class Inbox[T](private val q : ConcurrentQueue[ActorMessage]){
  def send(message : ActorMessage) : Unit = utils.execute{q.enqueue(message)}
}
