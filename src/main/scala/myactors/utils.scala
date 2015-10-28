package myactors

import scala.concurrent.ExecutionContext
object utils{

  /**
   * Sends a body to be executed to the global executor.
   * @param body
   * The body to be executed.
   */
  def execute(body: => Unit) = ExecutionContext.global.execute(
    new Runnable {
      def run() = body
    })

  /**
   * A log method that log's strings and the calling thread's name.
   * @param s
   *          The string to log
   */
  def log(s: String): Unit = {
    println(Thread.currentThread().getName + " " + s)
  }

  def randomNum(num : Int) : Int = scala.util.Random.nextInt(num)
}
