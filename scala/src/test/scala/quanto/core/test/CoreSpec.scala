package quanto.core.test

import org.scalatest._
import quanto.core._


class CoreSpec extends FlatSpec {
  var core : Core = _
  
  "Core" should "start" in {
    core = new Core("red_green", "../core/bin/quanto-core")
    core.start()
  }
  
  it should "return correct version" in {
    assert(core.version() === "2.01")
  }
  
  "Core.echo" should "correctly echo an Int" in {
    val req : Int = 12
    val resp = core.request[Int,Int]("Main", "echo", req)
    assert(req === resp)
  }
  
  it should "correctly echo a String" in {
    val req : String = "foo"
    val resp = core.request[String,String]("Main", "echo", req)
    assert(req === resp)
  }
  
  it should "correctly echo a Map" in {
    val req : Map[String,Int] = Map("foo" -> 12, "bar" -> 4)
    val resp = core.request[Map[String,Int],Map[String,Int]]("Main", "echo", req)
    assert(req === resp)
  }
  
  "Core" should "shutdown" in {
    core.kill()
  }
}