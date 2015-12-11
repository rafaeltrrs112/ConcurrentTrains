package client

import java.net.URI
import io.backchat.hookup._
import org.gnome.gtk.DrawingArea
import scala.concurrent.Future
import scala.util.{Failure, Success}
import serverdist.{Bottom, Top, Config}
import cross.HeatConstants._

/**
  * A heat node for managing a portion of the heat net's work.
  *
  * @param port
  *             The port to connect to.
  * @param drawingArea
  *           The drawing area this heat node draws to on it's on draw methods.
  * @param config
  *               The config of the table of this node's associated table.
  */
class HeatNode(port : String, drawingArea : DrawingArea, config : Config) extends HookupClient {
  val uri = URI.create(s"ws://localhost:$port/")
  val settings : HookupClientConfig = HookupClientConfig(uri)
  var boundedNode : Option[HeatNode] = None

  implicit val clientPort = this

  val view = new HeatView(400, 100, drawingArea)
  var currentImg : Option[Array[Double]] = None


  override def receive = {
    case TextMessage(msg) =>
      val dataList = HeatNode.toSeq(msg)
      val tag = dataList.head.toInt
      val body = dataList.tail

      /**
        * Messages received from bounded server are updates and get relayed to the bounded client
        * where it is forwarded to the the bounded client's associated servers.
        *
        * case -> INIT_DATA : Initialization data received at connect time with the server.
        *
        * case -> IMG_UPDATE : The new image data, received from the associated server.
        */
      tag match {
        case INIT_DATA  => if(boundedNode.isDefined){
          boundedNode.get.receiveRelay(tag, body)
        }
        case IMG_UPDATE => if(boundedNode.isDefined) drawThenRelay(tag, body)
        case BORDER_UPDATE => if(boundedNode.isDefined){
          boundedNode.get.receiveRelay(tag, body)
        }
        case _ => throw new AssertionError("Invalid tag contained in message")
      }

      /* A INIT_REQUEST sent to the associated server. Should result in the subsequent arrival
       * of a INIT_DATA message.
       */
    case Connected => send(TextMessage(INIT_REQUEST.toString))
    case _ => println("Unhandled message....")
  }

  /**
    * Draws on the canvas.
    *
    * @param drawData
    *                Data to draw on the screen.
    */
  def draw(drawData : Array[Double]) : Unit = {
    view.painter.draw(drawData)
  }

  /**
    * Receives a relayed message from an associated node.
    *
    * @param tag
    *            The of the message being received.
    * @param body
    *             The body of the message being received.
    * @return
    *         A future object that handles the relay.
    */
  def receiveRelay(tag : Double, body : Array[Double]) = Future {
    tag match {
      case INIT_DATA  => send(buildData(INIT_DATA, body))
      case IMG_UPDATE => {
        config match {
          case Top() => send(buildData(BORDER_UPDATE, body.grouped(100).toList.head))
          case Bottom() => send(buildData(BORDER_UPDATE, body.grouped(100).toList.last))
        }
      }
      case BORDER_UPDATE => {
        config match {
          case Top() => send(buildData(BORDER_UPDATE, body))
          case Bottom() => send(buildData(BORDER_UPDATE, body))
        }
      }
    }
  }

  /**
    * Builds a data message from a tag and body..
    *
    * @param tag
    *            The of the message being received.
    * @param body
    *             The body of the message being received.
    * @return
    *         A message that can be sent out.
    */
  def buildData(tag : Double, body : Array[Double]) : TextMessage = {
    val msg = tag :: body.toList
    TextMessage(msg.mkString(","))
  }

  /**
    * Triggers a draw on the canvas and relay's the message to the associated
    * companion node.
    *
    * @param tag
    *            The tag of the message.
    * @param body
    *             The body of the message.
    * @return
    *         A future that handles the drawing task.
    */
  def drawThenRelay(tag : Double, body : Array[Double]) = {
    Future(view.painter.draw(body))
    boundedNode.get.receiveRelay(tag, body)
  }

  disconnect() onComplete  {
    case Success(value)=> println(s"Node at $port detached!")
    case Failure(e) => println("Disconnect Failure")
  }

  connect() onComplete  {
    case Success(value) => send(TextMessage(INIT_REQUEST.toString))
    case Failure(e) => println("Disconnect Failure")
  }

}

object HeatNode{
  def toSeq(msg : String) : Array[Double] = msg.split(",").map(_.toDouble)
}