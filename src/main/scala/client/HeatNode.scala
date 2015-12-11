package client

import java.net.URI
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
import akka.actor.ActorSystem
import io.backchat.hookup._
import org.gnome.gtk.DrawingArea
import org.json4s._
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}
import serverdist.{Bottom, Top, Config}
import cross.HeatConstants._
/**
  * Client node. In charge of one portion of the view
  */
class HeatNode(port : String, da : DrawingArea, config : Config) extends HookupClient {
  val uri = URI.create(s"ws://localhost:$port/")
  val settings : HookupClientConfig = HookupClientConfig(uri)
  var boundedNode : Option[HeatNode] = None

  implicit val clientPort = this

  val view = new HeatView(400, 100, da)
  var currentImg : Option[Array[Double]] = None

  override def receive = {
    case TextMessage(msg) =>
      val dataList = HeatNode.toSeq(msg)
      val tag = dataList.head.toInt
      val body = dataList.tail

      /**
        * Messages received from bounded server are updates and get relayed to friend.
        * INIT_DATA : Receive once, initialization data.
        * IMG_UPDATE : The new image data, received periodically.
        */
      tag match {
        case INIT_DATA  => if(boundedNode.isDefined){
          println("Relaying INIT!")
          boundedNode.get.receiveRelay(tag, body)
        }
        case IMG_UPDATE => if(boundedNode.isDefined) drawThenRelay(tag, body)
        case BORDER_UPDATE => if(boundedNode.isDefined){
          println("Border UPDATE")
          boundedNode.get.receiveRelay(tag, body)
        }
      }
    case Connected => send(TextMessage(INIT_REQUEST.toString))
    case _ => println("Unhandled message....")
  }

  def draw(d : Array[Double]) : Unit = {
    view.painter.draw(d)
  }

  //Relays are sent to the corresponding server.
  def receiveRelay(tag : Double, body : Array[Double]) = Future {
    tag match {
      case INIT_DATA  => send(buildData(INIT_DATA, body))
      case IMG_UPDATE => {
        println("Update Image received!")
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

  def buildData(tag : Double, body : Array[Double]) : TextMessage = {
    val msg = tag :: body.toList
    TextMessage(msg.mkString(","))
  }

  def drawThenRelay(tag : Double, body : Array[Double]) = {
    Future { view.painter.draw(body) }
    boundedNode.get.receiveRelay(tag, body)
  }

  disconnect() onComplete  {
    case Success(value)=>
      println("Disconnected!")
    case Failure(e) => println("Whoops...")
  }

  connect() onComplete  {
    case Success(value) => {
    println("Client connected!")
      send(TextMessage(INIT_REQUEST.toString))
    }
    case Failure(e) => println("Whoops...")
  }

}

object HeatNode{
  def toSeq(msg : String) : Array[Double] = msg.split(",").map(_.toDouble)
}