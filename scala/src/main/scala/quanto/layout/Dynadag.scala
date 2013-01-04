package quanto.layout

import quanto.data._

class Dynadag extends GraphLayout with ConstraintSolver {
  var precision = 1000
  var minNodeSep = 100

  def weight(graph: QGraph, e: EName) = 1
  def minLength(graph: QGraph, e: EName) = 1

  def layout(graph: QGraph): QGraph = {
    val dag = graph.dagCopy

    val rank = graph.verts.foldLeft(Map[VName,IntVar]())
      { case (m, (v,_)) => m + (v -> IntVar("r_" + v, 0, MaxInt)) }

    for ((e,_) <- dag.edges) {
      constraints += { rank(dag.target(e)) #>= (rank(dag.source(e)) + minLength(graph, e)) }
    }

    dag.edges.foldLeft[IntVar](None) {
      (x,y) => None
    }

    //rankCost = edges.foldLeft()

    // the position of the centers of each vertex
    val oldCoord = collection.mutable.Map[VName, (Int,Int)]()
    val coord = collection.mutable.Map[VName, (Int,Int)]()

    // the dimensions of the bounding box of each vertex
    val dim = collection.mutable.Map[VName, (Int,Int)]()

    graph
  }

}
