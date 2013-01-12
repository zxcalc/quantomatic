package quanto.layout

import quanto.data.Graph

class LayoutUnitializedException extends Exception("Layout data read before layout() called")

abstract class GraphLayout {
  def layout(graph: Graph): Graph
}
