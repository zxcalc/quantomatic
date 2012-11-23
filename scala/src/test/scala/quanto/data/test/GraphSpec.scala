package quanto.data.test

import org.scalatest._
import quanto.data._


class GraphSpec extends FlatSpec with GivenWhenThen {
  behavior of "A graph"
  
  var g : Graph[Unit,Int,Int,Unit] = _
  
  it can "initialize" in {
    g = new Graph("g")
  }

  var v0 : VName = _
  var v1 : VName = _
  var a0 : VName = _

  it can "add some vertices" in {
    var (v0,_) = g.addVertex(1)
    assert(v0 === VName("v0"))

    v1 = g.addVertex(2)._1
    assert(v1 === VName("v1"))

    a0 = g.addVertex(Vertex(VName("a0"), 3))._1
    assert(a0 === VName("a0"))
  }
}
