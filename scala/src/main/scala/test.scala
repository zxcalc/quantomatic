import quanto.core._
import com.codahale.jerkson.Json
import org.codehaus.jackson.JsonNode

object Test {
  def main(args: Array[String]) = {
    val core = new Core("/Users/aleks/git/quanto/quantomatic/core/bin/quanto-core")
    println("starting core...")
    core.start()
    println("done")
    val jsons = List(
    """
    {"request_id":0,
     "controller":"red_green",
     "module":"Main",
     "function":"echo",
     "input":{"foo":1337}}
    """,
    """
    {"request_id":1,
     "controller":"red_green",
     "module":"Main",
     "function":"concat",
     "input":{"arg1":"call me ", "arg2":"ishmael"}}
    """,
    """
    {"request_id":2,
     "controller":"red_green",
     "module":"Main",
     "function":"echoo",
     "input":{"foo":1337}}
    """)
    
    for (json <- jsons) {
      try {
        println("sending request to core...")
	    val resp = core.request[JsonNode](json)
	    println(resp)
	    println("done.")
      } catch {
        case e : CoreError => println("Error: " + e.message)
      }
    }
    
    
    
    core.kill()
  }
}