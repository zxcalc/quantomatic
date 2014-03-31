package quanto.core

import akka.actor._
import quanto.util.json._
import java.io.{FileWriter, BufferedWriter, File, OutputStream}
import quanto.util.Signal

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
case class CompileML(code: String)(val onComplete : Signal => Any) extends PolyConsoleMessage
case object InterruptML extends PolyConsoleMessage


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
  private var requestId = 10
  private var mlCompileId = 10

  coreProcess.startCore()
  reader = context.actorOf(Props { new CoreReader(coreProcess) }, name = "core_reader")
  writer = context.actorOf(Props { new CoreWriter(coreProcess) }, name = "core_writer")

  val codeWrapper =
    """PolyML.exception_trace (fn () => (use "%1$s"; TextIO.print ("\n<"^"<[S]%2$d>>\n")))"""+
    """  handle SML90.Interrupt => TextIO.print ("\n<"^"<[I]%2$d>>\n")"""+
    """       | _               => TextIO.print ("\n<"^"<[E]%2$d>>\n");"""

  log.info("fired up")

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
      coreProcess.killCore(waitForExit = false)
    case AddConsoleOutput(out) =>
      coreProcess.consoleOutput.addOutputStream(out)
    case RemoveConsoleOutput(out) =>
      coreProcess.consoleOutput.removeOutputStream(out)
    case compileMessage: CompileML =>
      val file = File.createTempFile("ml-code", ".ML")
      val fw = new BufferedWriter(new FileWriter(file))
      fw.write(compileMessage.code)
      fw.write("\n")
      fw.close()

      coreProcess.consoleOutput.addListener(mlCompileId) { _ => if (file.exists) file.delete() }
      coreProcess.consoleOutput.addListener(mlCompileId)(compileMessage.onComplete)

      val code = codeWrapper.format(file.getAbsolutePath, mlCompileId)
      println(code)
      coreProcess.consoleInput.println(code)
      coreProcess.consoleInput.flush()
      mlCompileId += 1
    case InterruptML =>
      coreProcess.polyPid.map { p => Runtime.getRuntime.exec("kill -SIGINT " + p) }

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
