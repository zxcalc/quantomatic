package quanto.core

import akka.actor._
import quanto.util.json._
import java.io.{FileWriter, BufferedWriter, File, OutputStream}
import quanto.util._

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

case class Call(controller: String, module: String, function: String, input: Json = JsonObject())
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

abstract class PolyConsoleMessage
case class AddConsoleOutput(out: OutputStream) extends PolyConsoleMessage
case class RemoveConsoleOutput(out: OutputStream) extends PolyConsoleMessage
case class CompileML(fileName: Option[String], code: String)(val onComplete : StreamMessage => Any) extends PolyConsoleMessage
case object InterruptML extends PolyConsoleMessage
case class SetMLWorkingDir(dir: String) extends PolyConsoleMessage


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

class Core extends Actor with ActorLogging {
  val coreProcess = new CoreProcess
  var reader: ActorRef = _
  var writer: ActorRef = _
  val listeners = collection.mutable.Map[Int, (ActorRef,CoreRequest)]()
  val activeRequests = collection.mutable.Set[Int]()
  private var requestId = 10
  private var mlCompileId = 10
  private var workingDir = "."

  coreProcess.startCore()
  reader = context.actorOf(Props { new CoreReader(coreProcess) }, name = "core_reader")
  writer = context.actorOf(Props { new CoreWriter(coreProcess) }, name = "core_writer")

  val codeWrapper =
    """PolyML.exception_trace (fn () => (OS.FileSys.chDir "%1s"; use "%2$s"; TextIO.print ("\n<Success>\n")))"""

//  +
//    """  handle SML90.Interrupt => TextIO.print ("\n<Interrupted>\n")"""+
//    """       | _               => TextIO.print ("\n<Finished with error>\n");"""

  log.info("fired up")

  def receive = {
    case req : CoreRequest =>
      //log.info("Request: " + req)
      try {
        val json = req.json.setPath("$.request_id", JsonInt(requestId))
        listeners += requestId -> (sender, req)
        writer ! json
      } catch {
        case e: JsonAccessException =>
          log.error(e, "JsonAccessException in Core request: " + req.json.toString)
      }

      requestId += 1
    case CoreResponse(rid, resp) =>
      //log.info("Response: " + resp)
      listeners.get(rid) match {
        case Some((listener, req)) => listener ! req.decode(resp)
        case None => log.warning("Orphaned response for request_id: " + rid)
      }
    case StopCore =>
      log.info("shutting down")
      coreProcess.killCore(waitForExit = false)
    case AddConsoleOutput(out) =>
      coreProcess.consoleOutput.addOutputStream(out)
    case RemoveConsoleOutput(out) =>
      coreProcess.consoleOutput.removeOutputStream(out)
    case msg: CompileML =>
      val file = File.createTempFile("ml-code", ".ML")
      val fw = new BufferedWriter(new FileWriter(file))
      fw.write(msg.code)
      fw.write("\n")
      fw.close()
      val code = codeWrapper.format(workingDir, file.getAbsolutePath, mlCompileId)

      activeRequests.synchronized(activeRequests += mlCompileId)
      coreProcess.consoleOutput.addListener(mlCompileId)(msg.onComplete)
      coreProcess.consoleOutput.addListener(mlCompileId) { _ =>
        activeRequests.synchronized(activeRequests -= mlCompileId)
        if (file.exists()) file.delete()
      }
      val sm = StreamMessage.compileMessage(mlCompileId, msg.fileName.getOrElse("untitled"), code)
      sm.writeTo(coreProcess.consoleInput)
//
//      val code = codeWrapper.format(file.getAbsolutePath, mlCompileId)
//      println(code)
//      coreProcess.consoleInput.println(code)
//      coreProcess.consoleInput.flush()
      mlCompileId += 1
    case InterruptML =>
      activeRequests.synchronized { activeRequests.foreach{ rid =>
        val sm = StreamMessage(CodePart('K'), IntPart(rid), CodePart('k'))
        sm.writeTo(coreProcess.consoleInput)
      }}
    case SetMLWorkingDir(dir) =>
      workingDir = dir
      //coreProcess.polyPid.map { p => Runtime.getRuntime.exec("kill -SIGINT " + p) }

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
