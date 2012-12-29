package quanto.layout

import quanto.data._

class Dynadag extends GraphLayout {
  var precision = 1000
  var minNodeSep = 100

  def weight(graph: QGraph, e: EName) = 1
  def minLength(graph: QGraph, e: EName) = 100

  def layout(graph: QGraph): QGraph = {
    val rank = collection.mutable.Map[VName, Int]()

    // the position of the centers of each vertex
    val oldCoord = collection.mutable.Map[VName, (Int,Int)]()
    val coord = collection.mutable.Map[VName, (Int,Int)]()

    // the dimensions of the bounding box of each vertex
    val dim = collection.mutable.Map[VName, (Int,Int)]()

    graph
  }

}
