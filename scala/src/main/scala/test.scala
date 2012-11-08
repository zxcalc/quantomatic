import quanto.core._
import com.codahale.jerkson.Json
import org.codehaus.jackson.JsonNode

object Test {
  def main(args: Array[String]) = {
    val core = new Core("red_green", "../core/bin/quanto-core")
    println("starting core...")
    core.start()
    println("done")
    
    try {
      println("version: " + core.version())
      val resp1 = core.request[Map[String,String],String](
          "Main", "concat",
          Map("arg1" -> "foo ", "arg2" -> "bar")
        )
      println("req1: " + resp1)
      
      val resp2 = core.request[Map[String,String],Map[String,String]](
          "Main", "echo",
          Map("arg1" -> "foo ", "arg2" -> "bar")
        )
      println("req2: " + resp2)
    } catch {
      case CoreUserException(msg, _) => println("User error: " + msg)
    }
    
    core.kill()
  }
}