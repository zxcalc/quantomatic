package quanto.core
import org.codehaus.jackson.JsonNode
import com.codahale.jerkson.Json
import scala.collection.JavaConversions._

case class CoreRequest[A](
    request_id: Int,
    controller: String,
    module: String,
    function: String,
    input: A)

case class CoreResponse(
    request_id: Int,
    success: Boolean,
    output: JsonNode)
{
  // parsing as 'A' done after the fact, and only when success = true,
  // otherwise, output is parsed as CoreUserException
  def output[A](implicit mf: Manifest[A]): A = Json.parse[A](output)
}
case class CoreError(val message: String, val code: Int)

class Core(var controller: String, executable: String) {
  var rid = 0
  val process = new CoreProcess
  
  def start() { process.startCore(executable) }
  def stop() { process.killCore(true) }
  def kill() { process.killCore(false) }
  
  def request[S, T : Manifest](
      module: String,
      function: String,
      input: S): T =
  {
    val json = Json.generate(CoreRequest(rid, controller, module, function, input))
    process.stdin.write(json, 0, json.length)
    process.stdin.flush()
    val resp = Json.parse[CoreResponse](process.stdout)
    if (resp.success) resp.output[T]
    else {
      val err = resp.output[CoreError]
      if (err.code == -1)
        throw new CoreProtocolException(err.message)
      else throw new CoreUserException(err.message, err.code)
    }
  }
  
  // functions built in to the controller
  def help(module: String, function: String) : String = 
    this.request("!!", "help", Map("module"->module,"function"->function))
    
  def help(module: String) : String = 
    this.request("!!", "help", Map("module"->module))
  
  def version(): String = this.request("!!", "version", null)
}


