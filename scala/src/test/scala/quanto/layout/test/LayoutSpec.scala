import org.scalatest._
import quanto.data._
import quanto.layout.DotLayout

import quanto.util.QuadTree

class LayoutSpec extends FlatSpec {

  behavior of "Dot layout"

  val dotLayout = new DotLayout

  var dotGraph: Graph = Graph.random(10,20)

  it should "lay out a graph" in {
    dotGraph = dotLayout.layout(dotGraph)
    println(dotLayout.dotString)
  }

  behavior of "Quad tree"

  var randomGraph: Graph = Graph.random(4,8)
  var qTree : QuadTree[VName] = _

  it should "initialise with a set of vertices" in {
    qTree = QuadTree(randomGraph.vdata.toSeq.map{case (v,d) => (d.coord, v)})
  }

  it should "visit every vertex" in {
    var vs = randomGraph.verts
    qTree.visit { tr => tr.value.map { v =>
      assert(vs contains v)
      vs -= v
    }; false }
    assert(vs.isEmpty)
  }
}