package quanto.gui.graphview

import quanto.data._
import quanto.gui._
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


class GraphView extends Panel {
  import GraphView._

  private var edgeCache: EdgeCache = _

  var drawGrid = true
  var snapToGrid = true
  var gridMajor = 1.0
  var gridSubs = 4

  private var _editMode: Int = _
  def editMode = _editMode
  def editMode_=(em: Int) {
    _editMode = em
    em match {
      case ReadWrite => listenTo(mouse.clicks, mouse.moves)
      case CosmeticEdits => listenTo(mouse.clicks, mouse.moves)
      case ReadOnly => deafTo(mouse.clicks, mouse.moves)
    }
  }

  editMode = ReadOnly

  private var _trans = new Transformer()
  def trans = _trans
  def trans_=(newTrans: Transformer) {
    _trans = newTrans
    edgeCache = new EdgeCache(graph, trans)
  }

  private var _graph: Graph[Unit,VData,Unit,Unit] = _
  def graph = _graph
  def graph_=(newGraph: Graph[Unit,VData,Unit,Unit]) {
    _graph = newGraph
    edgeCache = new EdgeCache(graph, trans)
  }

  graph = Graph(defaultGName, ())

  val selectedVerts = collection.mutable.Set[VName]()
  val selectedEdges = collection.mutable.Set[EName]()
  val selectedBBoxes = collection.mutable.Set[BBName]()

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

    edgeCache.compute()

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
      edgeCache.compute()
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

