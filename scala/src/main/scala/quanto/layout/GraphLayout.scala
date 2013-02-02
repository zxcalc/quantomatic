package quanto.layout

import quanto.data.{VName, Graph}

class LayoutUninitializedException extends Exception("Layout data read before layout() called")

abstract class GraphLayout {
  private var _graph: Graph = null
  def graph = _graph

  private val _coords = collection.mutable.Map[VName,(Double,Double)]()
  def setCoord(v: VName, p:(Double,Double)) { _coords(v) = p }
  def coord(v: VName) = _coords(v)

  // override to compute layout data
  protected def compute()

  def layout(g: Graph): Graph = {
    _graph = g
    _coords.clear()
    graph.vdata.foreach { case (v,d) => _coords(v) = d.coord }

    compute()

    _coords.foldLeft(graph) { case(g,(v,c)) => g.updateVData(v) { _.withCoord(c) } }
  }
}
