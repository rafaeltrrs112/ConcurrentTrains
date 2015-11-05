import java.util.concurrent.LinkedBlockingDeque

/**
 * Iterators created for concurrent queues may or may not show all elements that currenlty
 * exist in a queue. One common occurrence with concurrent data structures are
 * weakly consistent iterators.
 */
object CollectionsIterators extends App {
  val queue = new LinkedBlockingDeque[String]()
  //Create a queue with 5500 elements
  for (i <- 1 to 5500) queue.offer(i.toString)
  //Concurrently start printing elements from the queue in the same order
  //the were entered. This is happening at the same time as when the main
  //thread is removing elements off the queue. The result of this is
  //a weakly consistent iteration
  ConcurrentCollections.execute {
    val it = queue.iterator()
    while (it.hasNext) println(Thread.currentThread().getName() + " " + it.next)
  }
  //Here the thread starts popping elements from the thread in the same order in
  //popped this in.
  for (i <- 1 to 5500) queue.poll()
  Thread.sleep(1000)
}
