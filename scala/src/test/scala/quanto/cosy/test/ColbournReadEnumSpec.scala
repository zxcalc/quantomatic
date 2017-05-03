package quanto.cosy.test

import org.scalatest._
import quanto.cosy._


class ColbournReadEnumSpec extends FlatSpec {
  behavior of "Adjacency matrices"

  it should "be constructable" in {
    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(true, false))

    println("Incomplete matrix:\n" + amat)
    assert(!amat.isComplete, "graph still has unconnected boundaries, so it should not yet be complete")

    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, true, false))

    println("Complete matrix:\n" + amat)
    assert(amat.isComplete, "graph should now be complete")
  }

  it should "allow multple red/green types" in {
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

    println("Multi-type matrix:\n" + amat)
  }

  it should "correctly identify a non-canonical matrix" in {

    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(true, false))
    amat = amat.addVertex(Vector(false, true, false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, false, true, true))

    println("Non-canonical matrix:\n" + amat)
    assert(!amat.isCanonical)
  }

  it should "correctly identify a canonical matrix" in {
    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, true))
    amat = amat.addVertex(Vector(true, false, false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, false, true, true))

    println("Canonical matrix:\n" + amat)
    assert(amat.isCanonical)
  }

  it should "give correct number of valid connections" in {
    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, true))
    amat = amat.addVertex(Vector(false, false, false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, false, true, true))

    // the next green node should be connectable to either red node, or the one remaining boundary
    assert(amat.validConnections(bipartite = true).length === 2*2*2)

    // the next green node should be connectable to any node, or the one remaining boundary
    assert(amat.validConnections(bipartite = false).length === 2*2*2*2)
  }

  behavior of "Graph enumerator"

  // numbers of graphs with 0-5 vertices: 1, 1, 2, 4, 11, 34
  // from: https://oeis.org/A000088
  it should "give correct number of untyped graphs" in {
    assert(ColbournReadEnum.enumerate(1,0,0,0, bipartite = false).size === 1)
    assert(ColbournReadEnum.enumerate(1,0,0,1, bipartite = false).size === 1+1)
    assert(ColbournReadEnum.enumerate(1,0,0,2, bipartite = false).size === 1+1+2)
    assert(ColbournReadEnum.enumerate(1,0,0,3, bipartite = false).size === 1+1+2+4)
    assert(ColbournReadEnum.enumerate(1,0,0,4, bipartite = false).size === 1+1+2+4+11)
    assert(ColbournReadEnum.enumerate(1,0,0,5, bipartite = false).size === 1+1+2+4+11+34)
  }
}
