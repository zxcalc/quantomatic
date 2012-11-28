package quanto.gui

import quanto.data._
import quanto.data.Names._
import swing._

class GraphView(var graph: Graph[Unit,VData,Unit,Unit] = Graph(defaultGName, ())) extends Panel {
  val selectedVerts = collection.mutable.Set[VName]()
  val selectedEdges = collection.mutable.Set[EName]()
  val selectedBBoxes = collection.mutable.Set[BBName]()

  override def paint(g: Graphics2D) {
    super.paint(g)
    g.drawString("Foo", 10,10)

  }
}
