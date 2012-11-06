package quanto.core
import org.codehaus.jackson.JsonNode
import com.codahale.jerkson.Json

case class CoreResponse(val request_id: Int, val success: Boolean, val output: JsonNode) {
  def output[A](implicit mf: Manifest[A]): A = Json.parse[A](output)
}
case class CoreError(val message: String, val code: Int) extends Exception(message)

class Core(executable: String) {
  val process = new CoreProcess
  
  def start() { process.startCore(executable) }
  def stop() { process.killCore(true) }
  def kill() { process.killCore(false) }
  
  def request[A](s: String)(implicit mf: Manifest[A]) : A = {
    process.stdin.write(s, 0, s.length)
    process.stdin.flush()
    val resp = Json.parse[CoreResponse](process.stdout)
    if (resp.success) resp.output[A]
    else throw resp.output[CoreError]
  }
}