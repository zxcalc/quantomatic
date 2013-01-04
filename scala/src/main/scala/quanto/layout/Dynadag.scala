package quanto.layout

import quanto.data._

class Dynadag extends GraphLayout with ConstraintSolver {
  var precision = 1000
  var minNodeSep = 100

  private var rankVars: Map[VName, IntVar] = null

  def weight(graph: QGraph, e: EName) = 1
  def minLength(graph: QGraph, e: EName) = 1

  def rank(v: VName): Int = {
    if (rankVars == null) throw new LayoutUnitializedException
    else rankVars(v).value
  }

  def layout(graph: QGraph): QGraph = {
    val dag = graph.dagCopy

    rankVars = graph.verts.foldLeft(Map[VName,IntVar]())
      { case (m, (v,_)) => m + (v -> IntVar("r_" + v, 0, graph.verts.size)) }

    val weights = for ((e,_) <- dag.edges) yield {
      constraints += { rankVars(dag.target(e)) #>= (rankVars(dag.source(e)) + minLength(graph, e)) }
      (rankVars(dag.target(e)) - rankVars(dag.source(e))) * weight(graph, e)
    }

    minimize(rankVars.values, sum(weights))

    // the position of the centers of each vertex
    val oldCoord = collection.mutable.Map[VName, (Int,Int)]()
    val coord = collection.mutable.Map[VName, (Int,Int)]()

    // the dimensions of the bounding box of each vertex
    val dim = collection.mutable.Map[VName, (Int,Int)]()

    graph
  }

}
