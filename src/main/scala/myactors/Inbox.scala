package myactors

/**
 */
class Inbox[T](val q : ConcurrentQueue[ActorMessage]){
  def send(message : ActorMessage) : Unit = utils.execute{q.enqueue(message)}
}
