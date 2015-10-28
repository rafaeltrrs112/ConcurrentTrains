package assignmenttwo.server

import myactors.{Actor, ActorMessage, ConcurrentQueue}

/**
 *
 */
object Server extends Actor {
  //TODO Implement the rematch algorithm for arenas only
  //TODO Port over to Play now and make the GUI with bindings then continue.
  //TODO Implement someone logging in from the outside with their pokemon if they can obtain a permission
  //TODO Implement the rematch algorithm for both arenas and pokemon now
  //TODO Change my concurrent Queue with the official one and test performance
  //TODO Test with UMH and log results on my desktop and only
  //TODO Create a GUI to display battle data on the screen as it happens
  //TODO Create a web page showing the results on my Oswego edu page

  //Finite number of permissions
  val permissions = ConcurrentQueue[Permission](Permission(999), Permission(998), Permission(978), Permission(932))

  override val name: String = "Server"

  override def onReceive(message: ActorMessage): Unit = {

  }
}
