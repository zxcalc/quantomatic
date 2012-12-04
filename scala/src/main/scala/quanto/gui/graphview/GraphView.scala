package quanto.gui.graphview

import quanto.data._
import quanto.data.Names._
import swing._
import event.MouseMoved
import event.MousePressed
import event.MouseReleased
import java.awt.{BasicStroke, Color, RenderingHints}
import java.awt.geom._
import math._
import quanto.data.EName
import quanto.data.BBName
import quanto.data.NodeV
import quanto.data.WireV
import quanto.data.VName


class GraphView(var graph: Graph[Unit,VData,Unit,Unit] = Graph(defaultGName, ())) extends Panel {
  import GraphView._

  var drawGrid = true
  var snapToGrid = true
  var gridMajor = 1.0
  var gridSubs = 4

  private var p_editMode: Int = _
  def editMode = p_editMode
  def editMode_=(em: Int) {
    p_editMode = em
    em match {
      case ReadWrite => listenTo(mouse.clicks, mouse.moves)
      case CosmeticEdits => listenTo(mouse.clicks, mouse.moves)
      case ReadOnly => deafTo(mouse.clicks, mouse.moves)
    }
  }

  editMode = ReadOnly

  private case class ECache(path: Path2D.Double, lines: List[Line2D.Double])
  private val edgeCache = collection.mutable.Map[EName, ECache]()



  val selectedVerts = collection.mutable.Set[VName]()
  val selectedEdges = collection.mutable.Set[EName]()
  val selectedBBoxes = collection.mutable.Set[BBName]()
  var trans = new Transformer()

  private def contactPoint(vd: VData, angle: Double): (Double,Double) = {
    vd match {
      case NodeV(c) => (c._1 + NodeRadius * cos(angle), c._2 + NodeRadius * sin(angle))
      case WireV(c) => {
        val chop = 0.707 * WireRadius
        var rad = 0d

        if (abs(WireRadius * cos(angle)) > chop) {
          rad = chop / cos(angle)
        } else if (abs(WireRadius * cos(angle)) > chop) {
          rad = chop / sin(angle)
        } else {
          rad = WireRadius
        }

        (c._1 + rad * cos(angle), c._2 + rad * sin(angle))
      }
    }
  }

  private def computeEdgeCache() {
    for ((v1,sd) <- graph.verts; (v2,td) <- graph.verts if v2 > v1) {
      val edges = graph.source.inv(v1) intersect graph.target.inv(v2)
      val rEdges = graph.target.inv(v1) intersect graph.source.inv(v2)
      val numEdges = edges.size + rEdges.size

      if (numEdges != 0) {
        val inc = Pi * (0.666 / (numEdges + 1))
        val angle = atan2(td.coord._2 - sd.coord._2, td.coord._1 - sd.coord._1)
        var i = 1

        // first do reverse edges, then do edges


        for (e <- rEdges.iterator ++ edges.iterator if edgeCache.get(e) == None) {
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
          }

          val (ah1, ah2, ah3) =
            (if (edges contains e) (tp._1, tp._2, inAngle) else (sp._1, sp._2, outAngle)) match {
              case (x,y,a) => (
                trans toScreen (x + ArrowheadLength * cos(a - ArrowheadAngle),
                  y + ArrowheadLength * sin(a - ArrowheadAngle)),
                trans toScreen (x,y),
                trans toScreen (x + ArrowheadLength * cos(a + ArrowheadAngle),
                  y + ArrowheadLength * sin(a + ArrowheadAngle))
              )
            }

          p.moveTo(ah1._1, ah1._2)
          p.lineTo(ah2._1, ah2._2)
          p.lineTo(ah3._1, ah3._2)

          edgeCache(e) = ECache(p, lines)

          i += 1
        }
      }
    }
  }

  private def drawGrid(g: Graphics2D) {
    val origin = trans toScreen (0,0)
    val minor = (trans scaleToScreen gridMajor) / gridSubs.toDouble

    val iterations = List(
      ceil((origin._1)/minor),
      ceil((bounds.width - origin._1)/minor),
      ceil((origin._2)/minor),
      ceil((bounds.height - origin._2)/minor)
    ).max.toInt

    g.setColor(AxisColor)
    g.drawLine(origin._1.toInt, 0, origin._1.toInt, bounds.height)
    g.drawLine(0, origin._2.toInt, bounds.width, origin._1.toInt)

    for (j <- 1 to iterations) {
      g.setColor(if (j % gridSubs == 0) MajorColor else MinorColor)
      val ydown = (origin._2 + j * minor).toInt
      val yup = (origin._2 - j * minor).toInt
      val xleft = (origin._1 - j * minor).toInt
      val xright = (origin._1 + j * minor).toInt

      g.drawLine(xleft, 0, xleft, bounds.height)
      g.drawLine(xright, 0, xright, bounds.height)
      g.drawLine(0, yup, bounds.width, yup)
      g.drawLine(0, ydown, bounds.width, ydown)

    }
  }

  override def paint(g: Graphics2D) {
    super.paint(g)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    g.setColor(Color.WHITE)
    g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height)
    if (drawGrid) drawGrid(g)

    computeEdgeCache()

    for ((e, ECache(p,_)) <- edgeCache) {
      if (selectedEdges contains e) {
        g.setColor(Color.BLUE)
        g.setStroke(new BasicStroke(2))
      } else {
        g.setColor(Color.BLACK)
        g.setStroke(new BasicStroke(1))
      }

      g.draw(p)
    }

    g.setStroke(new BasicStroke(1))

    val trNodeRadius = trans scaleToScreen NodeRadius
    val trWireWidth = 0.707 * (trans scaleToScreen WireRadius)

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

  }

  reactions += {
    case MousePressed(_, pt, modifiers, _, _) => println("pressed at: " + pt)
    case MouseReleased(_, pt, modifiers, _, _) => {
      computeEdgeCache()
      for ((e, ECache(_,lines)) <- edgeCache)
        if (lines exists (_.ptLineDistSq(pt) < EdgeSelectionRadius*EdgeSelectionRadius))
          selectedEdges += e

      this.repaint()
    }
    case MouseMoved(_, pt, _) => //println("moved through: " + pt)
  }
}

object GraphView {
  final val NodeRadius = 0.16
  final val WireRadius = 0.1
  final val ArrowheadLength = 0.1
  final val ArrowheadAngle = 0.25 * Pi
  final val EdgeSelectionRadius = 3.0

  final val ReadOnly = 0
  final val CosmeticEdits = 1
  final val ReadWrite = 2

  final val AxisColor = new Color(0.8f,0.8f,0.9f)
  final val MajorColor = new Color(0.85f,0.85f,1.0f)
  final val MinorColor = new Color(0.9f,0.9f,1.0f)
}

