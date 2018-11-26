package quanto.cosy.test

import org.scalatest._
import quanto.cosy._
import quanto.data.{Graph, NodeV, Theory}
import quanto.util.json.JsonObject


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
    assert(!amat.isCanonical())
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
    assert(amat.isCanonical())
  }

  it should "correctly identify a color-symmetric matrix" in {

    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(true, false))
    amat = amat.addVertex(Vector(false, false, false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, true, true, true))
    amat = amat.addVertex(Vector(false, false, true, true, false))

    println("Color-symmetric matrix:\n" + amat)
    assert(amat.isColorSymmetric)
  }

  it should "correctly identify a non-color-symmetric matrix" in {

    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(true, false))
    amat = amat.addVertex(Vector(false, false, false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, true, true, false))
    amat = amat.addVertex(Vector(false, false, true, true, false))

    println("Non-color-symmetric matrix:\n" + amat)
    assert(!amat.isColorSymmetric)
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
    assert(amat.validConnections(bipartite = true).length === 2 * 2 * 2)

    // the next green node should be connectable to any node, or the one remaining boundary
    assert(amat.validConnections(bipartite = false).length === 2 * 2 * 2 * 2)
  }

  it should "create a short, unique identifier" in {
    // Simple AdjMat first
    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, true))
    amat = amat.addVertex(Vector(false, false, false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, false, true, true))
    assert(amat.hash == "2.2.1.309y")

    // Now for multiple types
    amat = new AdjMat(numRedTypes = 2, numGreenTypes = 2)
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
    assert(amat.hash == "2.1-2.1-1.p1p5tukju")
  }

  it should "convert the hash back into an AdjMat" in {
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
    var adjFromHash = AdjMat.fromHash("2.1-2.1-1.p1p5tukju")
    assert(amat == adjFromHash)
  }

  behavior of "Graph enumerator"

  // numbers of graphs with 0-5 vertices: 1, 1, 2, 4, 11, 34
  // from: https://oeis.org/A000088
  it should "give correct number of untyped graphs" in {
    assert(ColbournReadEnum.enumerate(1, 0, 0, 0, bipartite = false).size === 1)
    assert(ColbournReadEnum.enumerate(1, 0, 0, 1, bipartite = false).size === 1 + 1)
    assert(ColbournReadEnum.enumerate(1, 0, 0, 2, bipartite = false).size === 1 + 1 + 2)
    assert(ColbournReadEnum.enumerate(1, 0, 0, 3, bipartite = false).size === 1 + 1 + 2 + 4)
    assert(ColbournReadEnum.enumerate(1, 0, 0, 4, bipartite = false).size === 1 + 1 + 2 + 4 + 11)
    assert(ColbournReadEnum.enumerate(1, 0, 0, 5, bipartite = false).size === 1 + 1 + 2 + 4 + 11 + 34)
  }

  // numbers of connected biparite (aka bi-colourable) graphs with 0-7 vertices: 1, 1, 1, 1, 3, 5, 17, 44
  // from: https://oeis.org/A005142
  it should "give correct number of bipartite graphs" in {
    // note that we double-count, one for each colouring, except when graphs are colour symmetric
    def numBi(sz: Int) = {
      ColbournReadEnum.enumerate(1, 1, 0, sz, bipartite = true).map { g =>
        if (g.isConnected) {
          if (g.isColorSymmetric) 2 else 1
        } else 0
      }.sum / 2
    }

    assert(numBi(0) === 1)
    assert(numBi(1) === 1 + 1)
    assert(numBi(2) === 1 + 1 + 1)
    assert(numBi(3) === 1 + 1 + 1 + 1)
    assert(numBi(4) === 1 + 1 + 1 + 1 + 3)
    assert(numBi(5) === 1 + 1 + 1 + 1 + 3 + 5)
    assert(numBi(6) === 1 + 1 + 1 + 1 + 3 + 5 + 17)
    assert(numBi(7) === 1 + 1 + 1 + 1 + 3 + 5 + 17 + 44)
  }

  behavior of "conversion to graph"

  it should "convert small amats into ZX graphs" in {
    val rg = Theory.fromFile("red_green")

    val pi = math.Pi
    val rdata = Vector(
      NodeV(data = JsonObject("type" -> "X", "value" -> "0"), theory = rg),
      NodeV(data = JsonObject("type" -> "X", "value" -> pi.toString), theory = rg),
      NodeV(data = JsonObject("type" -> "X", "value" -> (0.5 * pi).toString), theory = rg),
      NodeV(data = JsonObject("type" -> "X", "value" -> (-0.5 * pi).toString), theory = rg)
    )
    val gdata = Vector(
      NodeV(data = JsonObject("type" -> "Z", "value" -> "0"), theory = rg),
      NodeV(data = JsonObject("type" -> "Z", "value" -> pi.toString), theory = rg),
      NodeV(data = JsonObject("type" -> "Z", "value" -> (0.5 * pi).toString), theory = rg),
      NodeV(data = JsonObject("type" -> "Z", "value" -> (-0.5 * pi).toString), theory = rg)
    )

    var one = Complex.one
    def quickGraph(amat: AdjMat) : Graph = AdjMat.toZXGraph(amat, rdata, gdata)
    ColbournReadEnum.enumerate(2,2,2,2).map(quickGraph)
  }
}
