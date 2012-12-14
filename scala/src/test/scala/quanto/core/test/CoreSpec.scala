package quanto.core.test

import org.scalatest._
import quanto.core._
import quanto.util.json._

class CoreSpec extends FlatSpec {
  var core : Core = _
  
  "Core" can "start" in {
    core = new Core("red_green", "../core/bin/quanto-core")
    core.start()
  }
  
  it should "return correct version" in {
    assert(core.version() === "2.01")
  }
  
  "Core.echo" should "correctly echo an Int" in {
    val req : Json = 12
    val resp = core.request("test", "echo", req)
    assert(req === resp)
  }
  
  it should "correctly echo a String" in {
    val req : Json = "foo"
    val resp = core.request("test", "echo", req)
    assert(req === resp)
  }
  
  it should "correctly echo a Map" in {
    val req = JsonObject("foo" -> 12, "bar" -> 4)
    val resp = core.request("test", "echo", req)
    assert(req === resp)
  }
  
  "Core" can "shutdown" in {
    core.kill()
  }
}