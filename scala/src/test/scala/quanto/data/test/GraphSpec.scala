package quanto.data.test

import org.scalatest._
import quanto.data._
import quanto.data.Names._


class GraphSpec extends FlatSpec with GivenWhenThen {
  behavior of "A graph"
  
  var g : Graph[Unit,VData,Boolean,Unit] = _
  
  it can "initialize" in {
    g = new Graph("g0", ())
  }

  var v0 : VName = _
  var v1 : VName = _
  var e0 : EName = _
  var e1 : EName = _
  var bb0 : BBName = _
  var bb1 : BBName = _

  it should "get fresh names for newVertex" in {
    g.newVertex(NodeV()) match {case (g1,v) => g = g1; v0 = v}
    assert(v0 === VName("v0"))

    g.newVertex(NodeV()) match {case (g1,v) => g = g1; v1 = v}
    assert(v1 === VName("v1"))
  }

  it should "get fresh names for newEdge" in {
    g.newEdge(true, v0 -> v1) match {case (g1,e) => g = g1; e0 = e}
    assert(e0 === EName("e0"))

    g.newEdge(false, v1 -> v1) match {case (g1,e) => g = g1; e1 = e}
    assert(e1 === EName("e1"))
  }

  it should "get fresh names for newBBox" in {
    g.newBBox(()) match {case (g1,b) => g = g1; bb0 = b}
    assert(bb0 === BBName("bb0"))

    g.newBBox((), Set(v0), Some(bb0)) match {case (g1,b) => g = g1; bb1 = b}
    assert(bb1 === BBName("bb1"))
  }

  it should "contain 2 vertices, edges, and bboxes" in {
    assert(g.verts.size === 2)
    assert(g.edges.size === 2)
    assert(g.bboxes.size === 2)
  }

  it should "throw dulicate name exceptions" in {
    intercept[DuplicateVertexNameException] {
      g.addVertex("v0", NodeV())
    }

    intercept[DuplicateEdgeNameException] {
      g.addEdge("e0", false, "v0" -> "v1")
    }

    intercept[DuplicateBBoxNameException] {
      g.addBBox("bb0", ())
    }
  }

  behavior of "Another graph"

  it can "be constructed in block form" in {
    // implicit conversions are used to make strings into names, where
    // necessary.
    val g1 = (new Graph[Unit,VData,Boolean,Unit]("g1",())
      addVertex ("v0", NodeV())
      addVertex ("v1", WireV())
      addVertex ("v2", NodeV())
      addEdge   ("e0", true, "v0" -> "v0")
      addEdge   ("e1", true, "v0" -> "v1")
      addEdge   ("e2", true, "v1" -> "v2")
      newEdge   (true, "v0" -> "v1")
      addBBox   ("bb0", (), Set("v0", "v1"))
      addBBox   ("bb1", (), Set("v2"), parent = Some("bb0"))
    )

    println(g1.toString)
  }

  it can "move some vertices" in {
    //..
  }
}
