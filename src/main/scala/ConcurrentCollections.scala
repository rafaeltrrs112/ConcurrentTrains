/**
 * Bad collection example
 */

import scala.collection._
import scala.concurrent.ExecutionContext

object ConcurrentCollections extends App {
  val buffer = mutable.ArrayBuffer[Int]()

  def asynAdd(numbers: Seq[Int]) = execute {
    buffer ++= numbers
    log(s"buffer = $buffer")
  }

  asynAdd(1 to 20)
  asynAdd(20 to 30)
  Thread.sleep(500)

  def execute(body: => Unit) = ExecutionContext.global.execute(
    new Runnable {
      def run() = body
    })

  def log(s: String): Unit = {
    println(Thread.currentThread().getName + " " + s)
  }
}
