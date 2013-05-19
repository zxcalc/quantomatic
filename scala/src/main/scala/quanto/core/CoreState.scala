package quanto.core

import akka.actor._
import quanto.util.json.Json

abstract class CoreMessage
case object StartCore extends CoreMessage
case object CoreInitialized extends CoreMessage
case class CoreResponse(requestId: Int, resp: Json) extends CoreMessage
case class UnhandledRequest(requestId: Int, reason: String) extends CoreMessage
abstract class CoreRequest extends CoreMessage {
  def requestId: Int
  def json : Json
  def decode(resp: Json): Any
}

class CoreState(executable: String) extends Actor with ActorLogging {
  val coreProcess = new CoreProcess(parallel = true)
  var reader: ActorRef = _
  var writer: ActorRef = _
  val listeners = collection.mutable.Map[Int, (ActorRef,CoreRequest)]()

  val coreDown: Receive = {
    case StartCore =>
      coreProcess.startCore(executable)
      reader = context.actorOf(Props[CoreReader], name = "core_reader")
      writer = context.actorOf(Props[CoreWriter], name = "core_writer")
      context.become(coreWaitForInit)
    case req : CoreRequest =>
      sender ! UnhandledRequest(req.requestId, "Core down")
    case x => log.warning("Unexpected message: " + x + " in state coreDown.")
  }

  val coreWaitForInit: Receive = {
    case CoreInitialized => context.become(coreRunning)
    case req : CoreRequest =>
      sender ! UnhandledRequest(req.requestId, "Core initializing")
    case x => log.warning("Unexpected message: " + x + " in state coreWaitForInit.")
  }

  val coreRunning: Receive = {
    case req : CoreRequest =>
      listeners += req.requestId -> (sender, req)
      writer ! req
    case CoreResponse(rid, resp) =>
      listeners.get(rid) match {
        case Some((listener, req)) => listener ! req.decode(resp)
        case None => log.warning("Orphaned response for request_id: " + rid)
      }
    case x => log.warning("Unexpected message: " + x + " in state coreRunning.")
  }

  def receive = coreDown
}

class CoreReader(process: CoreProcess) extends Actor {
  def receive = ???
}

class CoreWriter(process: CoreProcess) extends Actor {
  def receive = ???
}
