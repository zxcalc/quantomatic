package quanto.gui.graphview

import quanto.data._
import quanto.gui._
import java.awt.{Color, Shape}
import java.awt.geom.{Rectangle2D, Point2D}

case class BBDisplay(rect: Rectangle2D) {
  def pointHit(pt: Point2D) = rect.contains(pt)
}

trait BBoxDisplayData { self: VertexDisplayData =>
  def graph: Graph
  def trans: Transformer
  val bboxDisplay = collection.mutable.Map[BBName,BBDisplay]()

  protected def computeBBoxDisplay() {
    var offset = Math.max(boundsForVertexSet(graph.verts).getMaxX, trans.origin._1)

    // used to compute relative padding sizes
    val em = trans.scaleToScreen(0.1)

    graph.bboxes.foreach { bbox =>
      val vset = graph.contents(bbox)

      val rect = if (vset.isEmpty) {
        offset += 8*em
        new Rectangle2D.Double(offset, trans.origin._2 - 2*em, 4*em, 4*em)
      } else {
        val bounds = boundsForVertexSet(vset)

        new Rectangle2D.Double(
          bounds.getX - 2*em, bounds.getY - 2*em,
          bounds.getWidth + 4*em, bounds.getHeight + 4*em)
      }

      bboxDisplay += bbox -> BBDisplay(rect)
    }
  }

  def invalidateAllBBoxes() { bboxDisplay.clear() }
  def invalidateBBox(bbname: BBName) = bboxDisplay -= bbname
}
