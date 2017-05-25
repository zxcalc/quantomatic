package quanto.cosy.test

import org.scalatest.FlatSpec
import quanto.cosy.{Graph,Tensor,AdjMat}

/**
  * Test specification for graphs created from adjacency matrices
  */
class GraphSpec extends FlatSpec{
  behavior of "Graphs"

  it should "accept an adjacency matrix" in {
    var amat = new AdjMat(numRedTypes = 2, numGreenTypes = 2)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(true, false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, true, false))
    amat = amat.addVertex(Vector(false, false, false, true))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, false, false, true, false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, false, false, true, false, true))
    var g = new Graph(amat)
    assert(g.vertices(5).angleType == 0)
    assert(g.vertices(5).connections == Set[Int](3,6))
  }


}
