import org.scalatest._
import quanto.data.{DirEdge, NodeV, QGraph}
import quanto.layout.{DotLayout, RankLayout}
import quanto.util.json.Json

class LayoutSpec extends FlatSpec {
  behavior of "Rank layout"

  var rankLayout: RankLayout = _

  it should "initialise" in {
    rankLayout = new RankLayout
  }

  var randomGraph: QGraph = _

  it should "layout a graph" in {
    randomGraph = rankLayout.layout(QGraph.random(4,8))
  }

  it should "yield ranks for all verts" in {
    randomGraph.verts.foreach(v => rankLayout.rank(v))

    val totalDist = randomGraph.edges.foldLeft(0) { (x, e) =>
      x + (rankLayout.rank(randomGraph.target(e)) - rankLayout.rank(randomGraph.source(e)))
    }

    println("average distance: " + (totalDist.toDouble / randomGraph.edges.size))
  }

  it should "provide a strict ranking for associated dag" in {
    val dag = randomGraph.dagCopy

    for ((e,_) <- dag.edata) {
      assert(rankLayout.rank(dag.source(e)) < rankLayout.rank(dag.target(e)))
    }
  }

  behavior of "Dot layout"

  val dotLayout = new DotLayout

  var dotGraph: QGraph = QGraph.random(10,20)

  it should "lay out a graph" in {
    dotGraph = dotLayout.layout(dotGraph)
    println(dotLayout.dotString)
  }
}