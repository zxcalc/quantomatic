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
}
