package quanto.gui.graphview

import quanto.data._
import quanto.gui._
import quanto.data.Names._
import swing._
import event.Key.Modifier
import event.{MouseDragged, MouseMoved, MousePressed, MouseReleased}
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

  private var mouseState: MouseState = SelectTool()

  var drawGrid = false
  var snapToGrid = false
  var gridMajor = 1.0
  var gridSubs = 4
  var undoStack = null

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

  var graph: Graph[Unit,VData,Unit,Unit] = Graph(defaultGName, ())
  var trans = new Transformer

  private lazy val vertexDisplay: VertexDisplayData = new VertexDisplayData(graph, trans)
  private lazy val edgeDisplay: EdgeDisplayData = new EdgeDisplayData(graph, trans, vertexDisplay)

  def invalidate() {
    vertexDisplay.clear()
    edgeDisplay.clear()
  }

  val selectedVerts = collection.mutable.Set[VName]()
  val selectedEdges = collection.mutable.Set[EName]()
  val selectedBBoxes = collection.mutable.Set[BBName]()

  private def drawGridLines(g: Graphics2D) {
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
      val y1 = (origin._2 + j * minor).toInt
      val y2 = (origin._2 - j * minor).toInt
      val x1 = (origin._1 - j * minor).toInt
      val x2 = (origin._1 + j * minor).toInt

      g.drawLine(x1, 0, x1, bounds.height)
      g.drawLine(x2, 0, x2, bounds.height)
      g.drawLine(0, y2, bounds.width, y2)
      g.drawLine(0, y1, bounds.width, y1)

    }
  }

  override def paint(g: Graphics2D) {
    super.paint(g)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    g.setColor(Color.WHITE)
    g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height)
    if (drawGrid) drawGridLines(g)

    vertexDisplay.compute()
    edgeDisplay.compute()

    for ((e, EDisplay(p,_)) <- edgeDisplay) {
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

    for ((v, VDisplay(shape,color)) <- vertexDisplay) {
      g.setColor(color)
      g.fill(shape)

      if (selectedVerts contains v) {
        g.setColor(Color.BLUE)
        g.setStroke(new BasicStroke(2))
      } else {
        g.setColor(Color.BLACK)
        g.setStroke(new BasicStroke(1))
      }

      g.draw(shape)
    }

    g.setStroke(new BasicStroke(1))

    mouseState match {
      case box: SelectionBox =>
        g.setColor(new Color(0.5f,0.5f,1f,0.1f))
        g.fill(box.rect2d)
        g.setColor(new Color(0.5f,0.5f,1f,0.4f))
        g.draw(box.rect2d)
      case _=>
    }
  }

  reactions += {

    case MousePressed(_, pt, modifiers, _, _) =>
      mouseState match {
        case SelectTool() =>
          mouseState = SelectionBox(pt, pt)
        case state => throw new InvalidMouseStateException("MousePressed", state)
      }

    case MouseDragged(_, pt, _) =>
      mouseState match {
        case SelectTool() => // do nothing
        case SelectionBox(start,_) =>
          mouseState = SelectionBox(start, pt)
          repaint()
        case state => throw new InvalidMouseStateException("MouseMoved", state)
      }

    case MouseReleased(_, pt, modifiers, _, _) =>
      mouseState match {
        case SelectionBox(start,_) =>
          if ((modifiers & Modifier.Shift) != Modifier.Shift) {
            selectedVerts.clear()
            selectedEdges.clear()
            selectedBBoxes.clear()
          }

          vertexDisplay.compute()
          edgeDisplay.compute()

          if (pt.getX == start.getX && pt.getY == start.getY) {
            var selectionUpdated = false
            vertexDisplay find (_._2.pointHit(pt)) map { x => selectionUpdated = true; selectedVerts += x._1 }

            if (!selectionUpdated)
              edgeDisplay find (_._2.pointHit(pt)) map { x => selectionUpdated = true; selectedEdges += x._1 }
            // TODO: bbox selection
          }

          mouseState = SelectTool()
          repaint()

        case state => throw new InvalidMouseStateException("MouseReleased", state)
      }


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

