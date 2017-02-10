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
  def insideRect(otherRect: Rectangle2D) : Boolean = {
    otherRect.getMinX <= rect.getMinX &&
    otherRect.getMinY <= rect.getMinY &&
    otherRect.getMaxX >= rect.getMaxX &&
    otherRect.getMaxY >= rect.getMaxY
  }
}

trait BBoxDisplayData { self: VertexDisplayData =>
  def graph: Graph
  def trans: Transformer
  val bboxDisplay = collection.mutable.Map[BBName,BBDisplay]()

  protected def computeBBoxDisplay() {
    bboxDisplay.clear()
    var offset = Math.max(boundsForVertexSet(graph.verts).getMaxX, trans.origin._1)

    // used to compute relative padding sizes
    val em = trans.scaleToScreen(0.1)

    graph.bboxesChildrenFirst.foreach { bbox =>
      val vset = graph.contents(bbox)

      val rect = if (vset.isEmpty) {
        offset += 8*em
        new Rectangle2D.Double(offset, trans.origin._2 - 2*em, 4*em, 4*em)
      } else {

        /* bounds determined by vertices of bbox */
        var bounds = boundsForVertexSet(vset)
        val computed_bbs = bboxDisplay.keySet
        /* for each bbox increase borders if that bbox rectangle
         * is near the borders of the current bbox
         */
        computed_bbs.foreach { bb =>
          val bbd = bboxDisplay(bb)
          val rect = bbd.rect

          if(bounds.contains(rect)) {
            val ulx = min(rect.getMinX - 5.0*em, bounds.getMinX)
            val uly = min(rect.getMinY - 5.0*em, bounds.getMinY)
            val lrx = max(rect.getMaxX + 5.0*em, bounds.getMaxX)
            val lry = max(rect.getMaxY + 5.0*em, bounds.getMaxY)

            bounds = new Rectangle2D.Double(ulx, uly, lrx - ulx, lry - uly)
          }
          else if(rect.contains(bounds)) {
            val ulx = min(rect.getMinX, bounds.getMinX - 5.0*em)
            val uly = min(rect.getMinY, bounds.getMinY - 5.0*em)
            val lrx = max(rect.getMaxX, bounds.getMaxX + 5.0*em)
            val lry = max(rect.getMaxY, bounds.getMaxY + 5.0*em)

            val new_bounds = new Rectangle2D.Double(ulx, uly, lrx - ulx, lry - uly)
            bboxDisplay += bb -> BBDisplay(new_bounds)
          }
        }
        bounds
      }

      bboxDisplay += bbox -> BBDisplay(rect)
    }
  }

  def invalidateAllBBoxes() { bboxDisplay.clear() }
  def invalidateBBox(bbname: BBName) = bboxDisplay -= bbname
}
