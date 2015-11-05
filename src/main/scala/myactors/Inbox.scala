package myactors

trait InboxLike[T]{
  def send(message : ActorMessage) : Unit
}
/**
 */
class Inbox[T](val q : ConcurrentQueue[ActorMessage]) extends InboxLike[T] {
  def send(message : ActorMessage) : Unit = utils.execute{q.enqueue(message)}
}

class BetterInbox[T](val q : java.util.concurrent.ConcurrentLinkedQueue[ActorMessage]) extends InboxLike[T] {
  def send(message : ActorMessage) : Unit = utils.execute{q.offer(message)}
}