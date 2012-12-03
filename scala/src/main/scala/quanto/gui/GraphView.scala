package quanto.gui

import quanto.data._
import quanto.data.Names._
import swing._
import java.awt.{Color, Shape, RenderingHints}
import java.awt.geom.Ellipse2D

class GraphView(var graph: Graph[Unit,VData,Unit,Unit] = Graph(defaultGName, ())) extends Panel {
  val selectedVerts = collection.mutable.Set[VName]()
  val selectedEdges = collection.mutable.Set[EName]()
  val selectedBBoxes = collection.mutable.Set[BBName]()


  override def paint(g: Graphics2D) {
    super.paint(g)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setBackground(Color.WHITE)

    val centerX, centerY = 50.0d
    val radius = 8.0d
    val circle = new Ellipse2D.Double(centerX - radius, centerY - radius, 2.0 * radius, 2.0 * radius)

    g.setColor(Color.GREEN)
    g.fill(circle)

    g.setColor(Color.BLACK)
    g.draw(circle)
  }
}
