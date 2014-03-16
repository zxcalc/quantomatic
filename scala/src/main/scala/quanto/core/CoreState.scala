package quanto.core

import akka.actor._
import quanto.util.json._

case object StartCore
case object StopCore
case object CoreInitialized
case class CoreResponse(requestId: Int, resp: Json)
case class UnhandledRequest(requestId: Int, reason: String)
abstract class CoreRequest {
  def json : Json
  def decode(resp: Json): Any
}
case class SimpleRequest(json: Json) extends CoreRequest {
  def decode(resp: Json): Json = resp
}

case class Call(controller: String, module: String, function: String, input: Json)
extends CoreRequest
{
  val json = JsonObject(
    "controller" -> controller,
    "module" -> module,
    "function" -> function,
    "input" -> input
  )

  def decode(resp: Json) = Call.decode(resp)
}

abstract class CallResponse
case class Success(output: Json) extends CallResponse
case class Error(code: Int, message: String) extends CallResponse

object Call {
  def decode(json: Json) =
    if ((json / "success").boolValue) Success(json / "output")
    else Error((json / "output" / "code").intValue, (json / "output" / "message").stringValue)
}

case class JsonRequest(json: Json) extends CoreRequest {
  def decode(resp: Json) = Call.decode(resp)
}

class CoreState(executable: String) extends Actor with ActorLogging {
  val coreProcess = new CoreProcess(parallel = true)
  var reader: ActorRef = _
  var writer: ActorRef = _
  val listeners = collection.mutable.Map[Int, (ActorRef,CoreRequest)]()
  private var requestId = 0

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
      val json = req.json.setPath("$.request_id", JsonInt(requestId))
      listeners += requestId -> (sender, req)
      writer ! json
      requestId += 1
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
    case reqJson : Json =>
      reqJson.writeTo(process.stdin)
      process.stdin.flush()
  }
}
