package quanto.gui.graphview

import quanto.data._
import quanto.gui._
import java.awt.{Color, Shape}
import java.awt.geom.{Rectangle2D, Point2D}

case class BBDisplay(rect: Rectangle2D) {
  def corner =
    new Rectangle2D.Double(rect.getMinX - 5.0, rect.getMinY - 5.0, 10.0, 10.0)

  def pointHit(pt: Point2D) = rect.contains(pt)
  def cornerHit(pt : Point2D) = corner.contains(pt)
}

trait BBoxDisplayData { self: VertexDisplayData =>
  def graph: Graph
  def trans: Transformer
  val bboxDisplay = collection.mutable.Map[BBName,BBDisplay]()

  protected def computeBBoxDisplay() {
    var offset = Math.max(boundsForVertexSet(graph.verts).getMaxX, trans.origin._1)

    // used to compute relative padding sizes
    val em = trans.scaleToScreen(0.1)

    val positions = collection.mutable.Set[(Double,Double)]()

    graph.bboxes.foreach { bbox =>
      val vset = graph.contents(bbox)

      val rect = if (vset.isEmpty) {
        offset += 8*em
        new Rectangle2D.Double(offset, trans.origin._2 - 2*em, 4*em, 4*em)
      } else {
        val bounds = boundsForVertexSet(vset)
        var p = (bounds.getX - 2*em, bounds.getY - 2*em)
        var q = (bounds.getWidth + 4*em, bounds.getHeight + 4*em)
        while (positions.contains(p)) {
          p = (p._1 - 6*em, p._2 - 6*em)
          q = (q._1 + 8*em, q._2 + 8*em)
        }
        positions.add(p)

        new Rectangle2D.Double(p._1, p._2, q._1, q._2)
      }

      bboxDisplay += bbox -> BBDisplay(rect)
    }
  }

  def invalidateAllBBoxes() { bboxDisplay.clear() }
  def invalidateBBox(bbname: BBName) = bboxDisplay -= bbname
}
