import java.text.SimpleDateFormat
import java.util.concurrent.atomic.AtomicBoolean

/**
 */
object TimeCheck extends App {
  val dateFormat = new SimpleDateFormat("ss")
  val currentTime = System.currentTimeMillis() / 1000
  val trainGone = new AtomicBoolean(false)
//  println(dateFormat.format(new Date()))
  Thread.sleep(7000)
  val goneTime = System.currentTimeMillis() / 1000
  println(goneTime - currentTime)
  //Making a change now

}