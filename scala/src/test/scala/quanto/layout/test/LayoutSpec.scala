import org.scalatest._
import quanto.data.{DirEdge, NodeV, QGraph}
import quanto.layout.RankLayout
import quanto.util.json.Json

class LayoutSpec extends FlatSpec {
  behavior of "Dynadag layout"

  var dagLayout: RankLayout = _

  it should "initialise" in {
    dagLayout = new RankLayout
  }

  val randomGraph = QGraph.random(40,80)

  it should "layout a graph" in {
    dagLayout.layout(randomGraph)
  }

  it should "yield ranks for all verts" in {
    for ((v,_) <- randomGraph.vdata) {
      println("rank(" + v + ") = " + dagLayout.rank(v))
    }
  }

  it should "provide a strict ranking for associated dag" in {
    val dag = randomGraph.dagCopy

    for ((e,_) <- dag.edata) {
      assert(dagLayout.rank(dag.source(e)) < dagLayout.rank(dag.target(e)))
    }
  }
}