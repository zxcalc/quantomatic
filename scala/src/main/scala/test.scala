import quanto.core._
import com.codahale.jerkson.Json
import org.codehaus.jackson.JsonNode

object Test {
  def main(args: Array[String]) = {
    val core = new Core("../core/bin/quanto-core")
    println("starting core...")
    core.start()
    println("done")
    
    try {
      val resp1 : String = core.request(
          "red_green", "Main", "concat",
          Map("arg1" -> "foo ", "arg2" -> "bar")
        )
      println("req1: " + resp1)
      
      val resp2 : String = core.request(
          "red_green", "Main", "concatto",
          Map("arg1" -> "foo ", "arg2" -> "bar")
        )
      println("req2: " + resp2)
    } catch {
      case CoreUserException(msg, _) => println("User error: " + msg)
    }
    
    core.kill()
  }
}