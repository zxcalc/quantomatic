import quanto.core._
import com.codahale.jerkson.Json
import java.io.ByteArrayInputStream

case class CoreResponse(val request_id: Int, val success: Boolean, val output: Any)

object Test {
  def main(args: Array[String]) = {
    val coreProc = new CoreProcess
    println("starting core...")
    coreProc.startCore("/Users/aleks/git/quanto/quantomatic/core/bin/quanto-core")
    println("done")
    val json = (id : Integer) => """
    {"request_id":"""+id+""",
     "controller":"red_green",
     "module":"Main",
     "function":"echo",
     "input":{"foo":1337}}"""
    
    for (i <- 0 to 3) {
	    println("writing to core...")
	    coreProc.stdin.write(json(i), 0, json(i).length) 
	    coreProc.stdin.flush()
	    println("done.\nreading from core...")
	    val resp = Json.parse[CoreResponse](coreProc.stdout)
	    println(resp)
	    println("done.")
    }
    
    
    coreProc.killCore(false)
  }
}