package quanto.data.test

import org.scalatest._
import quanto.data._


class GraphSpec extends FlatSpec with GivenWhenThen {
  behavior of "A graph"
  
  var g : Graph[Unit,Int,Int,Unit] = _
  
  it can "initialize" in {
    g = new Graph("g")
  }

  it can "add some vertices" in {
    given("no parameters")
    g.addVertex()

    given("just data")
    g.addVertex(1)

    given("explicit Vertex instance")
    g.addVertex(Vertex("a0", 2))
  }
}
