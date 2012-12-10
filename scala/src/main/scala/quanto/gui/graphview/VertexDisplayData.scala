package quanto.gui.graphview

import java.awt.geom.{Rectangle2D, Ellipse2D, Point2D}
import java.awt.{Color, Shape}
import math._
import quanto.data._
import quanto.gui._

case class VDisplay(shape: Shape, color: Color) {
  def pointHit(pt: Point2D) = shape.contains(pt)
  def rectHit(r: Rectangle2D) = shape.intersects(r)
}

trait VertexDisplayData {
  def graph: Graph[Unit,VData,Unit,Unit]
  def trans: Transformer

  protected val vertexDisplay = collection.mutable.Map[VName,VDisplay]()

  // returns the contact point at the given angle, in graph coordinates
  protected def vertexContactPoint(vn: VName, angle: Double): (Double,Double) = {
    // TODO: replace this with proper boundary detection
    val c = graph.verts(vn).coord

    vertexDisplay(vn).shape match {
      case _: Ellipse2D => (c._1 + GraphView.NodeRadius * cos(angle), c._2 + GraphView.NodeRadius * sin(angle))
      case _: Rectangle2D => {
        val chop = 0.707 * GraphView.WireRadius
        var rad = 0d

        if (abs(GraphView.WireRadius * cos(angle)) > chop) {
          rad = chop / cos(angle)
        } else if (abs(GraphView.WireRadius * cos(angle)) > chop) {
          rad = chop / sin(angle)
        } else {
          rad = GraphView.WireRadius
        }

        (c._1 + rad * cos(angle), c._2 + rad * sin(angle))
      }
    }
  }

  protected def computeVertexDisplay() {
    val trNodeRadius = trans scaleToScreen GraphView.NodeRadius
    val trWireWidth = 0.707 * (trans scaleToScreen GraphView.WireRadius)

    for ((v,data) <- graph.verts if !vertexDisplay.contains(v)) {
      val (x,y) = trans toScreen data.coord

      vertexDisplay(v) = data match {
        case NodeV(_) =>
          VDisplay(
            new Ellipse2D.Double(
              x - trNodeRadius, y - trNodeRadius,
              2.0 * trNodeRadius, 2.0 * trNodeRadius),
            Color.GREEN)
        case WireV(_) =>
          VDisplay(
            new Rectangle2D.Double(
              x - trWireWidth, y - trWireWidth,
              2.0 * trWireWidth, 2.0 * trWireWidth),
            Color.GRAY)
      }
    }
  }

  def invalidateAllVerts() { vertexDisplay.clear() }
  def invalidateVertex(n: VName) = vertexDisplay -= n
}
