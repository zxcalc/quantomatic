package quanto.gui

import quanto.data._
import quanto.data.Names._
import swing._
import event._
import event.MouseMoved
import event.MousePressed
import event.MouseReleased
import java.awt.{Color, RenderingHints}
import java.awt.geom._
import math._
import quanto.data.EName
import quanto.data.BBName
import quanto.data.NodeV
import quanto.data.WireV
import quanto.data.VName




class GraphView(var graph: Graph[Unit,VData,Unit,Unit] = Graph(defaultGName, ())) extends Panel {
  final val nodeRadius = 0.16
  final val wireRadius = 0.1
  final val arrowheadLength = 0.1
  final val arrowheadAngle = 0.25 * Pi

  private case class ECache(path: Path2D.Double, lines: List[Line2D.Double])
  private val edgeCache = collection.mutable.Map[EName, ECache]()

  listenTo(mouse.clicks, mouse.moves)

  reactions += {
    case MousePressed(_, pt, modifiers, _, _) => println("pressed at: " + pt)
    case MouseReleased(_, pt, modifiers, _, _) => println("released at: " + pt)
    case MouseMoved(_, pt, _) => //println("moved through: " + pt)
  }

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

  private def computeEdgeCache() {
    for ((v1,sd) <- graph.verts; (v2,td) <- graph.verts if v2 > v1) {
      val edges = graph.source.inv(v1) intersect graph.target.inv(v2)
      val redges = graph.target.inv(v1) intersect graph.source.inv(v2)
      val numEdges = edges.size + redges.size

      if (numEdges != 0) {
        val inc = Pi * (0.666 / (numEdges + 1))
        val angle = atan2(td.coord._2 - sd.coord._2, td.coord._1 - sd.coord._1)
        var i = 1

        // first do reverse edges, then do edges


        for (e <- redges.iterator ++ edges.iterator if edgeCache.get(e) == None) {
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



          var j = 0
          val p = new Path2D.Double()

          val bc = new CubicCurve2D.Double(p1._1, p1._2, p2._1, p2._2, p3._1, p3._2, p4._1, p4._2)
          p.append(bc, false)

          val iter = bc.getPathIterator(null, 0.2)
          val coords = Array.ofDim[Double](6)
          var prev = (0.0,0.0)
          var lines = List[Line2D.Double]()
          while (!iter.isDone) {
            lines = iter.currentSegment(coords) match {
              case PathIterator.SEG_LINETO =>
                new Line2D.Double(prev._1, prev._2, coords(0), coords(1)) :: lines
              case _ => lines
            }
            prev = (coords(0), coords(1))
            iter.next()
            j+=1
          }
          println("segments: " + j)

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

          p.moveTo(ah1._1, ah1._2)
          p.lineTo(ah2._1, ah2._2)
          p.lineTo(ah3._1, ah3._2)

          edgeCache(e) = ECache(p, lines)

          i += 1
        }
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

    computeEdgeCache()

    g.setColor(Color.BLACK)
    for ((_, ECache(p,_)) <- edgeCache) g.draw(p)

  }
}
