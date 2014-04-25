package quanto.gui.graphview

import quanto.data._
import quanto.gui._
import java.awt.{Color, Shape}
import java.awt.geom.{Rectangle2D, Point2D}
import scala.collection.SortedSet
import scala.collection.immutable.TreeSet
import math.{min,max}

case class BBDisplay(rect: Rectangle2D) {
  def corner =
    new Rectangle2D.Double(rect.getMinX - 5.0, rect.getMinY - 5.0, 10.0, 10.0)

  def pointHit(pt: Point2D) = rect.contains(pt)
  def cornerHit(pt : Point2D) = corner.contains(pt)
  def insideRect(other_rect: Rectangle2D) : Boolean = {
    other_rect.getMinX <= rect.getMinX &&
    other_rect.getMinY <= rect.getMinY &&
    other_rect.getMaxX >= rect.getMaxX &&
    other_rect.getMaxY >= rect.getMaxY
  }
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


    graph.bboxesChildrenFirst.foreach { bbox =>
      val vset = graph.contents(bbox)

      val rect = if (vset.isEmpty) {
        offset += 8*em
        new Rectangle2D.Double(offset, trans.origin._2 - 2*em, 4*em, 4*em)
      } else {

        /* bounds determined by vertices of bbox */
        var bounds = boundsForVertexSet(vset)
        val bbox_children = graph.bboxChildren(bbox)
        /* for each bbox child increase borders if child bbox rectangle
         * is near the borders of the parent bbox
         */
        bbox_children.foreach { bb_child =>
          val child_rect = bboxDisplay(bb_child).rect

          val ulx = min(child_rect.getMinX - 5.0, bounds.getMinX)
          val uly = min(child_rect.getMinY - 5.0, bounds.getMinY)
          val lrx = max(child_rect.getMaxX + 5.0, bounds.getMaxX)
          val lry = max(child_rect.getMaxY + 5.0, bounds.getMaxY)

          bounds = new Rectangle2D.Double(ulx, uly, lrx - ulx, lry - uly)
        }

        var p = (bounds.getX - 3*em, bounds.getY - 3*em)
        var q = (bounds.getWidth + 6*em, bounds.getHeight + 6*em)
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
