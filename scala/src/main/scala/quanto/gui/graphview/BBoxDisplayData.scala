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

  protected def computeBBoxDisplay() {

  }
}