package quanto.data.test

import org.scalatest._
import quanto.data._
import quanto.data.Names._
import quanto.util.json._



class GraphSpec extends FlatSpec with GivenWhenThen {
  behavior of "A graph"
  
  var g : Graph[Unit,VData,Boolean,Unit] = _
  
  it can "initialize" in {
    g = new Graph(())
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
    val g1 = (new Graph[Unit,VData,Boolean,Unit](())
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

  behavior of "A QGraph"

  var qg : QGraph = _
  it can "be constructed in block form" in {
    val g1 = (QGraph()
      addVertex ("v0", NodeV())
      addVertex ("v1", WireV())
      addVertex ("v2", NodeV())
      addEdge   ("e0", DirEdge(), "v0" -> "v0")
      addEdge   ("e1", DirEdge(), "v0" -> "v1")
      addEdge   ("e2", DirEdge(), "v1" -> "v2")
      newEdge   (DirEdge(), "v0" -> "v1")
      addBBox   ("bb0", BBData(), Set("v0", "v1"))
      addBBox   ("bb1", BBData(), Set("v2"), parent = Some("bb0"))
      )

    assert(g1.inBBox.codf("bb0") === Set(VName("v0"), VName("v1")))
  }

  var jsonGraph: QGraph = _

  it can "be constructed from JSON" in {
    jsonGraph = QGraph(Json.parse(
      """
        |{
        |  "wire_vertices": ["w0", "w1", "w2"],
        |  "node_vertices": {
        |    "n0":{"annotation": {"coord": [1,2]}},
        |    "n1":{}
        |  },
        |  "dir_edges": {
        |    "e0": {"src": "w0", "tgt": "w1"},
        |    "e1": {"src": "w1", "tgt": "w2"}
        |  },
        |  "undir_edges": {
        |    "e2": {"src": "n0", "tgt": "n1"}
        |  },
        |  "bang_boxes": {
        |    "bb0": {"contains": ["w0", "n0", "n1"]},
        |    "bb1": {"contains": ["n0", "n1"], "parent": "bb0"},
        |    "bb2": {}
        |  }
        |}
      """.stripMargin))
  }

  it should "be the expected graph" in {
    assert(jsonGraph.verts("w0").isInstanceOf[WireV])
    assert(jsonGraph.verts("w1").isInstanceOf[WireV])
    assert(jsonGraph.verts("w2").isInstanceOf[WireV])
    assert(jsonGraph.verts("n0").isInstanceOf[NodeV])
    assert(jsonGraph.verts("n1").isInstanceOf[NodeV])
    assert(jsonGraph.verts("n0").coord === (1.0, 2.0))
    assert(jsonGraph.verts("n1").coord === (0.0, 0.0))
    assert(jsonGraph.edges("e0").directed === true)
    assert(jsonGraph.edges("e1").directed === true)
    assert(jsonGraph.edges("e2").directed === false)
    assert(jsonGraph.source("e0") === VName("w0"))
    assert(jsonGraph.target("e0") === VName("w1"))
    assert(jsonGraph.source("e1") === VName("w1"))
    assert(jsonGraph.target("e1") === VName("w2"))
    assert(jsonGraph.source("e2") === VName("n0"))
    assert(jsonGraph.target("e2") === VName("n1"))
    assert(jsonGraph.inBBox.codf("bb0") === Set(VName("n0"), VName("n1"), VName("w0")))
  }

  behavior of "Depth-first traversal"

  val dftGraph = QGraph(Json.parse(
    """
      |{
      |  "node_vertices": ["n0", "n1", "n2", "n3", "n4", "n5"],
      |  "dir_edges": {
      |    "e0": {"src": "n0", "tgt": "n1"},
      |    "e1": {"src": "n2", "tgt": "n0"},
      |    "e2": {"src": "n1", "tgt": "n2"},
      |    "e3": {"src": "n0", "tgt": "n3"},
      |    "e4": {"src": "n4", "tgt": "n5"},
      |    "e5": {"src": "n5", "tgt": "n5"}
      |  }
      |}
    """.stripMargin))

  it should "traverse all edges" in {
    val eSet = dftGraph.dft(Set[EName]()) { (es, e, _) => es + e }
    assert(eSet === Set[EName]("e0", "e1", "e2", "e3", "e4", "e5"))
  }

  it should "traverse edges in the correct order" in {
    val eList = dftGraph.dft(List[EName]()) { (es, e, _) => e :: es }.reverse
    assert(eList === List[EName]("e0", "e2", "e1", "e3", "e4", "e5"))
  }
}
