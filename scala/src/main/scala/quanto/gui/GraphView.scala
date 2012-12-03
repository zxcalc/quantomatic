package quanto.gui

import quanto.data._
import quanto.data.Names._
import swing._
import java.awt.{Color, RenderingHints}
import java.awt.geom.{Line2D, CubicCurve2D, Rectangle2D, Ellipse2D}
import math._

class GraphView(var graph: Graph[Unit,VData,Unit,Unit] = Graph(defaultGName, ())) extends Panel {
  final val nodeRadius = 0.16
  final val wireRadius = 0.1
  final val arrowheadLength = 0.1
  final val arrowheadAngle = 0.25 * Pi

  val selectedVerts = collection.mutable.Set[VName]()
  val selectedEdges = collection.mutable.Set[EName]()
  val selectedBBoxes = collection.mutable.Set[BBName]()
  var trans = new Transformer()


  private def contactPoint(vd: VData, angle: Double): (Double,Double) = {
    vd match {
      case NodeV(c) => (c._1 + nodeRadius * cos(angle), c._2 + nodeRadius * sin(angle))
      case WireV(c) => {
        val chop = 0.707 * wireRadius
        var rad = 0d

        if (abs(wireRadius * cos(angle)) > chop) {
          rad = chop / cos(angle)
        } else if (abs(wireRadius * cos(angle)) > chop) {
          rad = chop / sin(angle)
        } else {
          rad = wireRadius
        }

        (c._1 + rad * cos(angle), c._2 + rad * sin(angle))
      }
    }
  }

  override def paint(g: Graphics2D) {
    g.setBackground(Color.WHITE)
    super.paint(g)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    val trNodeRadius = trans scaleToScreen nodeRadius
    val trWireWidth = 0.707 * (trans scaleToScreen wireRadius)

    for ((_,data) <- graph.verts) {
      val (x,y) = trans toScreen data.coord

      val (shape,fill) = data match {
        case NodeV(_) => (new Ellipse2D.Double(
            x - trNodeRadius, y - trNodeRadius,
            2.0 * trNodeRadius, 2.0 * trNodeRadius), Color.GREEN)
        case WireV(_) => (new Rectangle2D.Double(
            x - trWireWidth, y - trWireWidth,
            2.0 * trWireWidth, 2.0 * trWireWidth), Color.GRAY)
      }

      g.setColor(fill)
      g.fill(shape)
      g.setColor(Color.BLACK)
      g.draw(shape)
    }

    g.setColor(Color.BLACK)
    for ((v1,sd) <- graph.verts; (v2,td) <- graph.verts if v2 > v1) {
      val edges = graph.source.inv(v1) intersect graph.target.inv(v2)
      val redges = graph.target.inv(v1) intersect graph.source.inv(v2)
      val numEdges = edges.size + redges.size

      if (numEdges != 0) {
        val inc = Pi * (0.666 / (numEdges + 1))
        val angle = atan2(td.coord._2 - sd.coord._2, td.coord._1 - sd.coord._1)
        var i = 1

        // first do reverse edges, then do edges


        for (e <- redges.iterator ++ edges.iterator) {
          val outAngle = angle - (0.333 * Pi) + (inc * i)
          val inAngle = angle + (1.333 * Pi) - (inc * i)
          val sp = contactPoint(sd, outAngle)
          val tp = contactPoint(td, inAngle)

          val (dx,dy) = (tp._1 - sp._1, tp._2 - sp._2)
          val handleRad = 0.333 * sqrt(dx*dx + dy*dy)

          val cp1 = (sp._1 + handleRad * cos(outAngle), sp._2 + handleRad * sin(outAngle))
          val cp2 = (tp._1 + handleRad * cos(inAngle), tp._2 + handleRad * sin(inAngle))

          val (p1,p2,p3,p4) = (
              trans toScreen sp,
              trans toScreen cp1,
              trans toScreen cp2,
              trans toScreen tp
            )

          val bc = new CubicCurve2D.Double(p1._1, p1._2, p2._1, p2._2, p3._1, p3._2, p4._1, p4._2)
          g.draw(bc)

          val (ah1, ah2, ah3) =
            (if (edges contains e) // arrow head
              ((tp._1 + arrowheadLength * cos(inAngle - arrowheadAngle),
                tp._2 + arrowheadLength * sin(inAngle - arrowheadAngle)),
               tp,
               (tp._1 + arrowheadLength * cos(inAngle + arrowheadAngle),
                tp._2 + arrowheadLength * sin(inAngle + arrowheadAngle)))
            else // arrow tail
              ((sp._1 + arrowheadLength * cos(outAngle - arrowheadAngle),
                sp._2 + arrowheadLength * sin(outAngle - arrowheadAngle)),
               sp,
               (sp._1 + arrowheadLength * cos(outAngle + arrowheadAngle),
                sp._2 + arrowheadLength * sin(outAngle + arrowheadAngle))))
            match {case(x,y,z) => (trans toScreen x, trans toScreen y, trans toScreen z)}

          g.draw(new Line2D.Double(ah1._1, ah1._2, ah2._1, ah2._2))
          g.draw(new Line2D.Double(ah2._1, ah2._2, ah3._1, ah3._2))
          i += 1
        }
      }
    }
  }
}
