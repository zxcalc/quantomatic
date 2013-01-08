package quanto.gui.graphview

import quanto.data._
import quanto.gui._
import java.awt.{Color, Shape}
import java.awt.geom.{Rectangle2D, Point2D}

case class BBDisplay(rect: Rectangle2D) {
  def pointHit(pt: Point2D) = rect.contains(pt)
}

trait BBoxDisplayData { self: VertexDisplayData =>
  def graph: QGraph
  def trans: Transformer
  protected val bboxDisplay = collection.mutable.Map[BBName,BBDisplay]()

  protected def computeBBoxDisplay() {
    var globalBounds = boundsForVertexSet(graph.verts)

    // used to compute relative padding sizes
    val em = trans.scaleToScreen(0.1)

    graph.bboxes.foreach { bbox =>
      val vset = graph.contents(bbox)

      val rect = if (vset.isEmpty) {
        globalBounds = new Rectangle2D.Double(
                         globalBounds.getX, globalBounds.getY,
                         globalBounds.getWidth + 6*em, globalBounds.getHeight)
        new Rectangle2D.Double(globalBounds.getMaxX - 4*em, globalBounds.getCenterY - 2*em, 4*em, 4*em)
      } else {
        val bounds = boundsForVertexSet(vset)

        new Rectangle2D.Double(
          bounds.getX - 1*em, bounds.getY - 1*em,
          bounds.getWidth + 2*em, bounds.getHeight + 2*em)
      }

      bboxDisplay += bbox -> BBDisplay(rect)
    }
  }
}