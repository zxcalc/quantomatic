import org.scalatest._
import quanto.data.{DirEdge, NodeV, QGraph}
import quanto.layout.Dynadag
import quanto.util.json.Json

class LayoutSpec extends FlatSpec {
  behavior of "Dynadag layout"

  var dagLayout: Dynadag = _

  it should "initialise" in {
    dagLayout = new Dynadag
  }

  val nverts = 4
  val nedges = 4
  val rand = new util.Random
  var randomGraph = QGraph()
  for (i <- 1 to nverts) {
    val p = (rand.nextDouble * 6.0 - 3.0, rand.nextDouble * 6.0 - 3.0)
    randomGraph = randomGraph.newVertex(NodeV(p))
  }
  val varray = randomGraph.verts.keys.toArray
  for(j <- 1 to nedges) {
    val s = varray(rand.nextInt(varray.size))
    val t = varray(rand.nextInt(varray.size))
    randomGraph = randomGraph.newEdge(DirEdge(), (s,t))
  }

//  val randomGraph = QGraph(Json.parse(
//    """
//      |{
//      |  "node_vertices": ["n0", "n1", "n2", "n3", "n4", "n5"],
//      |  "dir_edges": {
//      |    "e0": {"src": "n0", "tgt": "n1"},
//      |    "e1": {"src": "n2", "tgt": "n0"},
//      |    "e2": {"src": "n1", "tgt": "n2"},
//      |    "e3": {"src": "n0", "tgt": "n3"},
//      |    "e4": {"src": "n4", "tgt": "n5"},
//      |    "e5": {"src": "n5", "tgt": "n5"}
//      |  }
//      |}
//    """.stripMargin))

  it should "layout a graph" in {
    dagLayout.layout(randomGraph)
  }

  it should "yield ranks for all verts" in {
    for ((v,_) <- randomGraph.verts) {
      println("rank(" + v + ") = " + dagLayout.rank(v))
    }
  }

  it should "provide a strict ranking for associated dag" in {
    val dag = randomGraph.dagCopy

    for ((e,_) <- dag.edges) {
      assert(dagLayout.rank(dag.source(e)) < dagLayout.rank(dag.target(e)))
    }
  }
}