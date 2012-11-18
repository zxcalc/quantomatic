package quanto.data.test

import org.scalatest.FlatSpec
import quanto.data.PFun

class PFunSpec extends FlatSpec {
  var f : PFun[String,Int] = _
  
  behavior of "A Partial Function"
  
  it should "initialise" in {
    f = new PFun[String,Int]()
  }
  
  it should "add some elements" in {
    f += "a" -> 2
    f += "b" -> 1
    f += "c" -> 2
  }
  
  it should "evaluate inverse images" in {
    assert(f.inv(2).size === 2)
    assert(f.inv(2) === Set("a", "c"))
  }
  
}
