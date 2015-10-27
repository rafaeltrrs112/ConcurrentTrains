package myactors

/**
 * Created by rtorres12 on 10/26/15.
 */
class Inbox[T](private val q : ConcurrentQueue[ActorMessage]){
  def send(message : ActorMessage) : Unit = utils.execute{q.enqueue(message)}
}
