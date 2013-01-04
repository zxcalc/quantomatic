package quanto.layout

import quanto.data.QGraph

class LayoutUnitializedException extends Exception("Layout data read before layout() called")

abstract class GraphLayout {
  def layout(graph: QGraph): QGraph
}
