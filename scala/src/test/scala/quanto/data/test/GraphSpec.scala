package quanto.data.test

import org.scalatest._
import quanto.data._
import quanto.data.Names._
import quanto.data.Theory.ValueType
import quanto.util.json._

import scala.collection.immutable.TreeSet


class GraphSpec extends FlatSpec with GivenWhenThen {
  val rg : Theory = Theory.fromFile("red_green")
  val composite_thy : Theory = Theory.fromFile("composite")
  behavior of "A graph"
  
  var g : Graph = _
  
  it can "initialize" in {
    g = new Graph()
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
    g.newEdge(DirEdge(), v0 -> v1) match {case (g1,e) => g = g1; e0 = e}
    assert(e0 === EName("e0"))

    g.newEdge(DirEdge(), v1 -> v1) match {case (g1,e) => g = g1; e1 = e}
    assert(e1 === EName("e1"))
  }

  it should "get fresh names for newBBox" in {
    g.newBBox(BBData()) match {case (g1,b) => g = g1; bb0 = b}
    assert(bb0 === BBName("bx0"))

    g.newBBox(BBData(), Set(v0), Some(bb0)) match {case (g1,b) => g = g1; bb1 = b}
    assert(bb1 === BBName("bx1"))
  }

  it should "contain 2 vertices, edges, and bboxes" in {
    assert(g.vdata.size === 2)
    assert(g.edata.size === 2)
    assert(g.bbdata.size === 2)
  }

  it should "throw dulicate name exceptions" in {
    intercept[DuplicateVertexNameException] {
      g.addVertex("v0", NodeV())
    }

    intercept[DuplicateEdgeNameException] {
      g.addEdge("e0", DirEdge(), "v0" -> "v1")
    }

    intercept[DuplicateBBoxNameException] {
      g.addBBox("bx0", BBData())
    }
  }

  it should "be equal to its copy" in {
    val g1 = g.copy()
    assert(g1 != null)
    assert(g1 === g)
  }

  behavior of "Another graph"

  var otherG : Graph = _

  it can "be constructed in block form" in {
    // implicit conversions are used to make strings into names, where
    // necessary.
    otherG = (new Graph()
      addVertex ("v0", NodeV())
      addVertex ("v1", WireV())
      addVertex ("v2", NodeV())
      addEdge   ("e0", DirEdge(), "v0" -> "v0")
      addEdge   ("e1", UndirEdge(), "v0" -> "v1")
      addEdge   ("e2", DirEdge(), "v1" -> "v2")
      newEdge   (DirEdge(), "v0" -> "v1") // fresh name is returned, but dropped by implicit conversion
      addBBox   ("bx0", BBData(), Set("v0", "v1"))
      addBBox   ("bx1", BBData(), Set("v2"), parent = Some("bx0"))
    )

    println(otherG.toString)
  }

  it should "not be equal to the first graph" in {
    assert(g != otherG)
  }

  val jsonString =
    """
      |{
      |  "wire_vertices": ["w0", "w1", "w2"],
      |  "node_vertices": {
      |    "n0":{"annotation": {"coord": [1.0,2.0]}},
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
      |    "bx0": {"contents": ["n0", "n1", "w0"]},
      |    "bx1": {"contents": ["n0", "n1"], "parent": "bx0"},
      |    "bx2": {}
      |  }
      |}
    """.stripMargin

  val jsonGraphShouldBe = (Graph()
    addVertex("w0", WireV()) addVertex("w1", WireV()) addVertex("w2", WireV())
    addVertex("n0", NodeV(coord=(1.0,2.0)))
    addVertex("n1", NodeV())
    addEdge("e0", DirEdge(), "w0" -> "w1")
    addEdge("e1", DirEdge(), "w1" -> "w2")
    addEdge("e2", UndirEdge(), "n0" -> "n1")
    addBBox("bx0", BBData(), Set[VName]("w0", "n0", "n1"))
    addBBox("bx1", BBData(), Set[VName]("n0", "n1"), parent=Some(BBName("bx0")))
    addBBox("bx2", BBData())
  )

  var jsonGraph: Graph = _

  it can "be constructed from JSON" in {
    jsonGraph = Graph.fromJson(jsonString)
  }

  it should "be equal to a graph from the same JSON" in {
    val jsonGraph1 = Graph.fromJson(jsonString)
    assert(jsonGraph === jsonGraph1)
  }

  it should "be the expected graph" in {
    assert(jsonGraphShouldBe === jsonGraph)
  }

  // note this test is sensitive to having "normalised" JSON input, e.g. arrays that represent name sets are
  // alphabetised.
  it should "save to the correct json" in {
    val json = Graph.toJson(jsonGraph)
    assert(json === Json.parse(jsonString))
  }

  it should "save then load to the same graph" in {
    assert(jsonGraphShouldBe === Graph.fromJson(Graph.toJson(jsonGraphShouldBe)))
  }

  behavior of "Normalisation"

  it should "Normalise a single wire vertex to a single wire vertex" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["w0"],
        |  "undir_edges": {
        |  }
        |}
      """.stripMargin))
    assert(g1.normalise === g1)
  }


  it should "Normalise K2 to itself" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["w0", "w1"],
        |  "undir_edges": {
        |    "e0": {"src": "w0", "tgt": "w1"}
        |  }
        |}
      """.stripMargin))
    assert(g1.normalise === g1)
  }

  it should "Normalise P3 to itself" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["w0", "w1", "w2"],
        |  "undir_edges": {
        |    "e0": {"src": "w0", "tgt": "w1"},
        |    "e1": {"src": "w1", "tgt": "w2"}
        |  }
        |}
      """.stripMargin))
    assert(g1.normalise === g1)
  }


  it should "Normalise P4 to P3" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["w0", "w1", "w2","w3"],
        |  "undir_edges": {
        |    "e0": {"src": "w0", "tgt": "w1"},
        |    "e1": {"src": "w1", "tgt": "w2"},
        |    "e2": {"src": "w2", "tgt": "w3"}
        |  }
        |}
      """.stripMargin))
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["w0", "w1", "w3"],
        |  "undir_edges": {
        |    "e0": {"src": "w0", "tgt": "w1"},
        |    "e2": {"src": "w1", "tgt": "w3"}
        |  }
        |}
      """.stripMargin))
    assert(g1.normalise === g2)
  }


  it should "Not normalise self loop" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["w0"],
        |  "undir_edges": {
        |    "e0": {"src": "w0", "tgt": "w0"}
        |  }
        |}
      """.stripMargin))
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["w0"],
        |  "undir_edges": {
        |  }
        |}
      """.stripMargin))
    assert(g1.normalise !== g2)
  }

  behavior of "Some more graphs"

  it should "normalise" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["w0", "w1"],
        |  "node_vertices": ["v0", "v1"],
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "w0"},
        |    "e1": {"src": "w0", "tgt": "w1"},
        |    "e2": {"src": "w1", "tgt": "v1"}
        |  }
        |}
      """.stripMargin))
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["w0"],
        |  "node_vertices": ["v0", "v1"],
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "w0"},
        |    "e2": {"src": "w0", "tgt": "v1"}
        |  }
        |}
      """.stripMargin))

    assert(g1.normalise === g2)
  }

  it should "minimise" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["w0", "w1"],
        |  "node_vertices": ["v0", "v1"],
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "w0"},
        |    "e1": {"src": "w0", "tgt": "w1"},
        |    "e2": {"src": "w1", "tgt": "v1"}
        |  }
        |}
      """.stripMargin))
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": ["v0", "v1"],
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "v1"}
        |  }
        |}
      """.stripMargin))

    assert(g1.minimise === g2)
  }

  behavior of "Depth-first traversal"

  val dftGraph = Graph.fromJson(
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
    """.stripMargin)

  it should "traverse all edges" in {
    val eSet = dftGraph.dft(Set[EName]()) { (es, e, _) => es + e }
    assert(eSet === Set[EName]("e0", "e1", "e2", "e3", "e4", "e5"))
  }

//  it should "traverse edges in the correct order" in {
//    val eList = dftGraph.dft(List[EName]()) { (es, e, _) => e :: es }.reverse
//    assert(eList === List[EName]("e0", "e2", "e1", "e3", "e4", "e5"))
//  }

  behavior of "Dag copy"

  val dagGraph = Graph.fromJson(
    """
      |{
      |  "node_vertices": ["n0", "n1", "n2", "n3", "n4", "n5"],
      |  "dir_edges": {
      |    "e0": {"src": "n1", "tgt": "n0"},
      |    "e1": {"src": "n2", "tgt": "n0"},
      |    "e2": {"src": "n1", "tgt": "n2"},
      |    "e3": {"src": "n0", "tgt": "n3"},
      |    "e4": {"src": "n4", "tgt": "n5"}
      |  }
      |}
    """.stripMargin)

  it should "translate into a dag correctly" in {
    assert(dftGraph.dagCopy === dagGraph)
  }

  it should "leave dags unchanged" in {
    val dag = Graph.randomDag(50,50)
    assert(dag.dagCopy === dag)
  }



  def traverseFrom(graph: Graph, v: VName, seen: Set[VName]) {
    if (seen contains v) fail("directed cycle detected")
    for (e <- graph.outEdges(v)) traverseFrom(graph, graph.target(e), seen + v)
  }

  it should "contain no directed cycles" in {
    val graph = Graph.random(100,100)
    val dag = graph.dagCopy

    for ((v,_) <- graph.vdata) traverseFrom(dag, v, Set[VName]())
  }

  val twobb = (new Graph()
    addVertex ("v0", NodeV())
    addBBox   ("bx0", BBData(), Set("v0"))
    addBBox   ("bx1", BBData(), Set("v0"))
    )

  "vertex in two bboxes" should "retain bbox membership for fullSubgraph" in {
    val twobb1 = twobb.fullSubgraph(Set("v0"), Set("bx0", "bx1"))
    assert(twobb1.inBBox.domf("v0") === Set[BBName]("bx0","bx1"))
  }

  it should "copy correctly" in {
    val twobb1 = twobb.fullSubgraph(Set("v0"), Set("bx0", "bx1")).renameAvoiding(twobb)
    val v1 = twobb1.verts.head
    val bs = twobb1.bboxes
    assert(bs.size === 2)
    assert(twobb1.inBBox.domf(v1) === bs)
  }

  behavior of "A graph with angles"

  it should "return the free variables" in {
    val g = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z", "value": "x + y"}},
        |    "v1": {"data": {"type": "X", "value": "z + pi"}}
        |  }
        |}
      """.stripMargin), thy = rg)

    assert(g.freeVars === Set(
      (ValueType.AngleExpr, "x"),
      (ValueType.AngleExpr, "y"),
      (ValueType.AngleExpr, "z")))
  }

  behavior of "A graph with composite values"

  it should "accept a composite-valued graph" in {
    val g = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z", "value": "x + y, true"}},
        |    "v1": {"data": {"type": "X", "value": "z + pi, false"}}
        |  }
        |}
      """.stripMargin), thy = composite_thy)
    var pi_commute = Graph.fromJson(Json.parse(
      """
        |{"wire_vertices":{
        | "b1":{"annotation":{"boundary":true,"coord":[-0.75,2.0]}},
        | "b0":{"annotation":{"boundary":true,"coord":[-11.5,1.5]}}
        | },
        |"node_vertices":{
        |   "v1":{"data":{"type":"Z","value":"\\pi/4,false"},"annotation":{"coord":[-3.0,0.5]}},
        |   "v0":{"data":{"type":"Z","value":"\\pi/2,false"},"annotation":{"coord":[-8.5,1.75]}},
        |   "v2":{"data":{"type":"X","value":"\\pi, \\true"},"annotation":{"coord":[-5.5,2.5]}}},
        |  "undir_edges":{
        |   "e0":{"src":"b0","tgt":"v0"},
        |   "e1":{"src":"v0","tgt":"v2"},
        |   "e2":{"src":"v2","tgt":"v1"},
        |   "e3":{"src":"v1","tgt":"b1"}
        | }
        |}""".stripMargin
    ), thy = composite_thy)

  assert(pi_commute.freeVars === Set())
    assert(pi_commute.vdata("v2").asInstanceOf[NodeV].phaseData ===
      new CompositeExpression(
        Vector(ValueType.AngleExpr, ValueType.Boolean),
        Vector(PhaseExpression(1, Map(), ValueType.AngleExpr), PhaseExpression(1, Map(), ValueType.Boolean)))
    )
  }

//  it should "support Graph.Flavor clipboard flavor" in {
//    assert(Graph().isDataFlavorSupported(Graph.Flavor))
//  }

  behavior of "vertex cutting"
  // Create a graph with left, middle and right vertices
  // Join middle and right,
  // Experiment with cutting out the left vertex
  // Note that we are normalising the graphs, which can cause unintuitive behaviour
  var cut_g = new Graph()
  cut_g = cut_g.addVertex("v0", NodeV()).
    addVertex("v1", NodeV()).
    addVertex("v2", NodeV()).
    addEdge("e", DirEdge(), "v1" -> "v2")

  it should "count arities" in {
    val g0 = cut_g.normalise
    assert(g0.arity("v0") == 0)
    assert(g0.arity("v1") == 1)
    assert(g0.arity("v2") == 1)
  }


  it should "find end-points of wires" in {
    val g0 = cut_g.normalise
    assert(g0.edgeEndPoints("e0")._1 == Set("v1", "v2").map(VName))
  }

  it should "find no end-points in a loop" in {
    val g0 = new Graph().
      addVertex("v0", WireV()).
      addVertex("v1", WireV()).
      addEdge("e0", DirEdge(), "v0"->"v1").
      addEdge("e1", DirEdge(), "v1"->"v0")
    assert(g0.edgeEndPoints("e0") == (Set(), Set("e0","e1").map(EName), Set("v0","v1").map(VName)))
  }

  it should "cut out a lone vertex" in {
    val (g0,b) = cut_g.
      normalise.cutVertex("v0")
    assert(b.isEmpty)
    assert(g0.nodesThatAreNotWires.size == 2)
    assert(g0.edges.size == 2) //double expected because normalisation
  }

  it should "cut out a vertex attached to another vertex" in {
    val (g0,b) = cut_g.
      normalise.addEdge("ee0", DirEdge(), "v0" -> "v1").cutVertex("v0")
    assert(b.size == 1)
    assert(g0.nodesThatAreNotWires.size == 2)
    assert(g0.edges.size == 3) // One going left to boundary, two between v1 and v2
  }


  it should "cut out a vertex attached to another vertex and a boundary" in {
    val (g0,b) = cut_g.
      normalise.
      addEdge("ee0", DirEdge(), "v0" -> "v1").
      addVertex("b0", WireV()).
      addEdge("ee1", DirEdge(), "b0" -> "v0").
      cutVertex("v0")
    assert(b.size == 1)
    assert(g0.nodesThatAreNotWires.size == 2)
    assert(g0.edges.size == 3) // One going left to boundary, two between v1 and v2
  }
}
