package quanto.layout

import quanto.data._

class RankLayout extends GraphLayout with ConstraintSolver {
  var precision = 1000
  var minNodeSep = 100

  private var rankMap = Map[VName, Int]()

  def weight(graph: QGraph, e: EName) = 1
  def minLength(graph: QGraph, e: EName) = 1

  def rank(v: VName): Int = rankMap(v)

  def layout(graph: QGraph): QGraph = {
    var model = graph.dagCopy

    // assign ranks to minimise edge length
    val rankVars = graph.verts.foldLeft(Map[VName,IntVar]())
      { (m, v) => m + (v -> IntVar("r_" + v, 0, graph.vdata.size)) }

    val weights = for (e <- model.edges) yield {
      constraints += { rankVars(model.target(e)) #>= (rankVars(model.source(e)) + minLength(graph, e)) }
      weight(graph, e) match {
        case 1 => rankVars(model.target(e)) - rankVars(model.source(e))
        case w => (rankVars(model.target(e)) - rankVars(model.source(e))) * w
      }
    }

    minimize(rankVars.values, sum(weights))
    rankMap = rankVars.mapValues(_.value)


    // update model graph to include virtual vertices at intermediate ranks
    for (e <- model.edges) {
      val vChain = (for (i <- (rankMap(model.source(e)) + 1) to (rankMap(model.target(e)) - 1)) yield {
        val (g,v) = model.newVertex(WireV())
        model = g
        rankMap += v -> i
        v
      }) :+ model.target(e)

      if (vChain.size > 1) {
        var prev = model.source(e)
        model = model.deleteEdge(e)
        for (v <- vChain) {
          model = model.newEdge(DirEdge(), (prev, v))
          prev = v
        }
      }
    }


    // the position of the centers of each vertex
    val oldCoord = collection.mutable.Map[VName, (Int,Int)]()
    val coord = collection.mutable.Map[VName, (Int,Int)]()

    // the dimensions of the bounding box of each vertex
    val dim = collection.mutable.Map[VName, (Int,Int)]()

    val maxRank = rankMap.values.max

    graph.vdata.foldLeft(graph) { case (gr,(vn,_)) =>
      gr.updateVData(vn) { d => d withCoord (d.coord._2, -(0.5 * maxRank) + rank(vn).toDouble) }
    }
  }

}
