package serverdist
/**
  * Created by rtorres12 on 12/8/15.
  */

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import cross.{Adamantium, DummyCell}
import cross.cellutils.Composition
import io.backchat.hookup._
import io.backchat.hookup.HookupServer.HookupServerClient
import spray.json._
import scala.concurrent.Future
import cross.HeatConstants._
/**
  * A server class to run on RHO.
  * @param port
  *               The port to connect to.
  */
class HeatNet(port : Int, numberOfRows : Int, numberOfColumns : Int, config : Config) {
  implicit val serv : HeatNet = this
  val processor : CellProcessor = new CellProcessor(numberOfRows, numberOfColumns, config)

  val BEGIN = "BEGIN"
  val WAVE = "WAVE"

  val COMP_INIT = -99

  val serverModule = new HookupServerClient {
    override def receive = onReceiver
  }

  val internalHook = HookupServer(port)(serverModule)

  def send(msg : OutboundMessage) : Future[OperationResult] = {
    serverModule.send(msg)
  }

  def start(): Unit = {
    internalHook.start
  }

  def onReceiver : HookupClient.Receive = {
    case TextMessage(msg) => {
      val dataList = toSeq(msg)
      val tag = dataList.head.toInt
      val body = dataList.tail

      tag match {
        case INIT_REQUEST => send(buildMessage(INIT_DATA, processor.initMessage))
        case INIT_DATA => processor.updateWithInit(body)
        case BORDER_UPDATE => processor.normalUpdate(body)
      }

    }
    case Connected => {
      println(s"Server side connect sending initData now! $config")
    }
  }

  def buildMessage(tag : Double, body : Array[Double]) : TextMessage = {
    val msg = tag :: body.toList
    TextMessage(msg.mkString(","))
  }

  //Call when a calculation is complete
  def sendImg() : Unit = send(buildMessage(IMG_UPDATE, processor.updateMessage()))

  def sendBorders() : Unit = send(buildMessage(BORDER_UPDATE, processor.borderMessage))

  def toSeq(msg : String) : Array[Double] = msg.split(",").map(_.toDouble)
}