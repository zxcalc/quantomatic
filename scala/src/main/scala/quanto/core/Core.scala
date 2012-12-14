package quanto.core
import quanto.util.json._
import JsonValues._

class Core(var controller: String, executable: String) {
  var rid = 0
  val process = new CoreProcess
  
  def start() { process.startCore(executable) }
  def stop() { process.killCore(true) }
  def kill() { process.killCore(false) }
  
  def request(module: String, function: String, input: Json): Json =
  {
    JsonObject(
      "request_id" -> rid,
      "controller" -> controller,
      "module"     -> module,
      "function"   -> function,
      "input"      -> input
    ).writeTo(process.stdin)

    process.stdin.flush()

    Json.parse(process.stdout) match {
      case JsonObject(map) =>
        try {
          if (map("success")) map("output")
          else throw (
            if (map("code") == -1) new CoreProtocolException(map("message"))
            else new CoreUserException(map("message"), map("code")))
        } catch {
          case e: NoSuchElementException =>
            throw new CoreProtocolException(e.toString + " for JSON: " + JsonObject(map).toString)
        }
      case _ => throw new CoreProtocolException("Expected JSON object as core response")
    }
  }
  
  // functions built in to the controller
  def help(module: String, function: String) : String = 
    this.request("!!", "help", JsonObject("module"->module,"function"->function))
    
  def help(module: String) : String = 
    this.request("!!", "help", JsonObject("module"->module))
  
  def version(): String = this.request("!!", "version", JsonNull())
}


