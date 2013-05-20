package quanto.core

import akka.actor._
import quanto.util.json._

case object StartCore
case object StopCore
case object CoreInitialized
case class CoreResponse(requestId: Int, resp: Json)
case class UnhandledRequest(requestId: Int, reason: String)
abstract class CoreRequest {
  def requestId: Int
  def json : Json
  def decode(resp: Json): Any
}
case class SimpleRequest(jsonString: String) extends CoreRequest {
  val json = Json.parse(jsonString)
  def requestId = (json / "request_id").intValue
  def decode(resp: Json): Json = resp
}

class CoreState(executable: String) extends Actor with ActorLogging {
  val coreProcess = new CoreProcess(parallel = true)
  var reader: ActorRef = _
  var writer: ActorRef = _
  val listeners = collection.mutable.Map[Int, (ActorRef,CoreRequest)]()

  coreProcess.startCore(executable)
  reader = context.actorOf(Props { new CoreReader(coreProcess) }, name = "core_reader")
  writer = context.actorOf(Props { new CoreWriter(coreProcess) }, name = "core_writer")

  log.info("fired up")

//  val coreDown: Receive = {
//    case StartCore =>
//      coreProcess.startCore(executable)
//      reader = context.actorOf(Props { new CoreReader(coreProcess) }, name = "core_reader")
//      writer = context.actorOf(Props { new CoreWriter(coreProcess) }, name = "core_writer")
//      context.become(coreRunning)
//    case req : CoreRequest =>
//      sender ! UnhandledRequest(req.requestId, "Core down")
//      log.warning("Unhandled request: " + req + " in state coreDown.")
//    case x => log.warning("Unexpected message: " + x + " in state coreDown.")
//  }

//  val coreWaitForInit: Receive = {
//    case CoreInitialized => context.become(coreRunning)
//    case req : CoreRequest =>
//      sender ! UnhandledRequest(req.requestId, "Core initializing")
//    case x => log.warning("Unexpected message: " + x + " in state coreWaitForInit.")
//  }

//  val coreRunning: Receive = {
//    case req : CoreRequest =>
//      log.info("Request: " + req)
//      listeners += req.requestId -> (sender, req)
//      writer ! req
//    case CoreResponse(rid, resp) =>
//      log.info("Response: " + resp)
//      listeners.get(rid) match {
//        case Some((listener, req)) => listener ! req.decode(resp)
//        case None => log.warning("Orphaned response for request_id: " + rid)
//      }
//    case x => log.warning("Unexpected message: " + x + " in state coreRunning.")
//  }

  def receive = {
    case req : CoreRequest =>
      //log.info("Request: " + req)
      listeners += req.requestId -> (sender, req)
      writer ! req
    case CoreResponse(rid, resp) =>
      //log.info("Response: " + resp)
      listeners.get(rid) match {
        case Some((listener, req)) => listener ! req.decode(resp)
        case None => log.warning("Orphaned response for request_id: " + rid)
      }
    case StopCore =>
      log.info("shutting down")
      coreProcess.killCore(false)
    case x => log.warning("Unexpected message: " + x)
  }
}

class CoreReader(process: CoreProcess) extends Actor {
  while (true) {
    val json = Json.parse(process.stdout)
    context.parent ! CoreResponse((json / "request_id").intValue, json)
  }

  def receive = PartialFunction.empty
}

class CoreWriter(process: CoreProcess) extends Actor {
  def receive = {
    case req : CoreRequest =>
      req.json.writeTo(process.stdin)
      process.stdin.flush()
  }
}
