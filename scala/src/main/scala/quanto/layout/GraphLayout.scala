package quanto.layout

import quanto.data.{VName, Graph}
import scala.util.Random

class LayoutUninitializedException extends Exception("Layout data read before layout() called")

abstract class GraphLayout {
  private var _graph: Graph = null
  def graph = _graph

  protected val lockedVertices = collection.mutable.Set[VName]()

  def lockVertex(v: VName) { lockedVertices += v }
  def clearLockedVertices() { lockedVertices.clear() }

  private val _coords = collection.mutable.Map[VName,(Double,Double)]()
  def setCoord(v: VName, p:(Double,Double)) {
    if (!lockedVertices.contains(v)) _coords(v) = p
  }
  def coord(v: VName) = _coords(v)
  def coords = _coords.clone()

  // override to compute layout data
  protected def compute()

  def initialize(g: Graph, randomCoords: Boolean = true) {
    _graph = g
    _coords.clear()
    val r = new Random(0xdeadbeef)
    graph.vdata.foreach { case (v,d) =>
      _coords(v) = if (randomCoords) (0.5 - r.nextDouble(), 0.5 - r.nextDouble()) else d.coord }
  }

  def updateGraph() {
    _graph = _coords.foldLeft(graph) { case(g,(v,c)) => g.updateVData(v) { _.withCoord(c) } }
  }

  def layout(g: Graph, randomCoords: Boolean = true): Graph = {
    initialize(g, randomCoords)
    compute()
    updateGraph()

    graph
  }
}
