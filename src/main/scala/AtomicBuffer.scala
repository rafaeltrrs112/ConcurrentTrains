import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec

/**
 * One solution is to use synchronized on immutable data structures
 */
class AtomicBuffer[T] {
  private val buffer = new AtomicReference[List[T]](Nil)

  @tailrec
  final def +=(x: T): Unit = {
    val xs = buffer.get
    val nxs = x :: xs
    if (!buffer.compareAndSet(xs, nxs)) {
      this.+=(x)
    }
  }

  override def toString = buffer.get().toString()
}

object SyncOnMutable extends App {
  val buffer = new AtomicBuffer[Int]

  def asyncAdd(numbers: Int): Unit = {
    buffer.synchronized {
      buffer += numbers
      ConcurrentCollections.log(s"buffer = $buffer")
    }
  }

  asyncAdd(20)
  asyncAdd(30)
}
