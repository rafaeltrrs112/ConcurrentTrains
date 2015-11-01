package midterm

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import myactors.utils

import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext
class Worker[T](body : => T) extends Thread {
  var theValue : Option[T] = None
  override def run() = {
    chaptertwo.log(Thread.currentThread().getName + " executing")
    theValue = Some(body)
  }
}
object Worker{
  def apply[T](body : => T) : Worker[T] = new Worker[T](body)
}
/**
 */
object chaptertwo extends App {
  def thread[T](body: => T): Worker[T] = {
    val t = Worker[T](body)
    def result : Option[T] = t.theValue
    t.start()
    t
  }

  def execute[T](body: => T) : Option[T] = {
    val aWorker = Worker[T](body)
    ExecutionContext.global.execute(aWorker)
    aWorker.theValue
  }

  def log(s: String): Unit = {
    println(Thread.currentThread().getName + " " + s)
  }

  def one : Unit = {
    def parallel[A, B](a : => A, b : => B) : (A, B) = {
      val aResult = thread[A](a)
      val bResult = thread[B](b)
      aResult.join()
      bResult.join()
      (aResult.theValue.get, bResult.theValue.get)
    }
    println(parallel({1},{2}))
  }

  def two : Unit = {
    def periodically(duration: Long)(b: => Unit): Unit = {
      //Execute b every duration seconds
      while(true){
        thread(b)
        Thread.sleep(duration)
      }
    }
    periodically(1000)(println("time's up"))
  }

  def threeToSix = {
    class SyncVar[T] {
      val q : Queue[T] = Queue[T]()
      var _value : AtomicReference[Option[T]] = new AtomicReference[Option[T]](None)
      def value : Option[T] = if (q.isEmpty) None else Some[T](q.dequeue._1)
      //Throws NoSuchElementException
      def get(): T = value.get
      def put(x: T): Unit = _value.set(Some(x))
      def isEmpty : Boolean  = value.isEmpty
      def nonEmpty : Boolean = value.nonEmpty
    }

    val data : SyncVar[Int] = new SyncVar[Int]()

    def consumer() : Thread = thread {
      while(true){
        data.synchronized{
          if(data.nonEmpty) println(data.get())
          data.notify()
        }
      }
    }

    def producer() : Thread = thread {
      val value  = new AtomicInteger(0)
      while(value.incrementAndGet() != 15){
        data.synchronized{
          data.put(value.get())
          data.notify()
        }
      }
    }

    producer()
    consumer()
    consumer().join()
  }

  threeToSix

}
