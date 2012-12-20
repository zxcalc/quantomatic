package quanto.layout

import quanto.data._

class Dynadag {
  var graph: QGraph = QGraph()
  val rank = collection.mutable.Map[VName, Int]

  var precision = 1000
  var minNodeSep = 100

  // the position of the centers of each vertex
  val oldCoord = collection.mutable.Map[VName, (Int,Int)]
  val coord = collection.mutable.Map[VName, (Int,Int)]

  // the dimensions of the bounding box of each vertex
  val dim = collection.mutable.Map[VName, (Int,Int)]

  def weight(e: EName) = 1
  def minLength(e: EName) = 100
}
