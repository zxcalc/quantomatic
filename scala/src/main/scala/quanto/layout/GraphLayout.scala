package quanto.layout

import quanto.data.QGraph

abstract class GraphLayout {
  def layout(graph: QGraph): QGraph
}
