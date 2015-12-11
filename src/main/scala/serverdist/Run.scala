package serverdist

/**
  * Created by rtorres12 on 12/9/15.
  */
object Run extends App {
  val heatNet = new HeatNet(8125, 250, 500, Bottom())
  heatNet.start()
}