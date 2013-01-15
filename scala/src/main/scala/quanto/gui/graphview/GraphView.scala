package quanto.gui.graphview

import quanto.data._
import quanto.gui._
import quanto.data.Names._
import swing._
import java.awt.{Font => AWTFont, BasicStroke, RenderingHints, Color}
import math._
import java.awt.geom.Rectangle2D


class GraphView extends Panel
  with Scrollable
  with EdgeDisplayData
  with VertexDisplayData
  with BBoxDisplayData
{
  import GraphView._

  var drawGrid = false
  var snapToGrid = false
  var dynamicResize = false
  var gridMajor = 1.0
  var gridSubs = 4

  var selectionBox: Option[Rectangle2D] = None

  // gets called when the component is first painted
  lazy val init = {
    resizeViewToFit()
  }

  def computeDisplayData() {
    computeVertexDisplay()
    computeEdgeDisplay()
    computeBBoxDisplay()
  }

  var graph = new Graph()
  var trans = new Transformer

  var selectedVerts = Set[VName]()
  var selectedEdges = Set[EName]()
  var selectedBBoxes = Set[BBName]()

  def invalidateGraph() {
    invalidateAllVerts()
    invalidateAllEdges()
  }

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
    g.drawLine(0, origin._2.toInt, bounds.width, origin._2.toInt)

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

  def resizeViewToFit() {
    // top left and bottom right of bounds, in screen coordinates
    val graphTopLeft = graph.vdata.foldLeft(0.0,0.0) { (c,v) =>
      (min(c._1, v._2.coord._1), max(c._2, v._2.coord._2))
    } match {case (x,y) => trans toScreen (x - 1.0, y + 1.0)}

    val graphBottomRight = graph.vdata.foldLeft((0.0,0.0)) { (c,v) =>
      (max(c._1, v._2.coord._1), min(c._2, v._2.coord._2))
    } match {case (x,y) => trans toScreen (x + 1.0, y - 1.0)}

    // default bounds, based on the current position of the origin and the size of the visible region
    val vRect = peer.getVisibleRect
    val defaultTopLeft = (trans.origin._1 - (vRect.getWidth/2.0), trans.origin._2 - (vRect.getHeight/2.0))
    val defaultBottomRight = (trans.origin._1 + (vRect.getWidth/2.0), trans.origin._2 + (vRect.getHeight/2.0))

    val topLeft =     (min(graphTopLeft._1, defaultTopLeft._1),
                       min(graphTopLeft._2, defaultTopLeft._2))
    val bottomRight = (max(graphBottomRight._1, defaultBottomRight._1),
                       max(graphBottomRight._2, defaultBottomRight._2))

    val (w,h) = (bottomRight._1 - topLeft._1,
                 bottomRight._2 - topLeft._2)

    val changed = (topLeft._1 != 0) || (topLeft._2 != 0) ||
                  (preferredSize.width != w) || (preferredSize.height != h)

    if (changed) {
      trans.origin = (trans.origin._1 - topLeft._1, trans.origin._2 - topLeft._2)
      preferredSize = new Dimension(w.toInt, h.toInt)
      invalidateGraph()
      revalidate()
    }
  }

  override def repaint() {
    if (dynamicResize) resizeViewToFit()
    super.repaint()
  }

  override def paintComponent(g: Graphics2D) {
    super.paintComponent(g)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    init

    g.setColor(Color.WHITE)
    g.fillRect(0, 0, bounds.width, bounds.height)
    if (drawGrid) drawGridLines(g)

    computeDisplayData()

    g.setStroke(new BasicStroke(1))


    for ((bb, BBDisplay(rect)) <- bboxDisplay) {
      g.setColor(new Color(0.5f,0.5f,0.5f,0.2f))
      g.fill(rect)
      g.setColor(new Color(0.5f,0.5f,0.5f,0.7f))
      g.draw(rect)
    }

    for ((e, ed) <- edgeDisplay) {
      if (selectedEdges contains e) {
        g.setColor(Color.BLUE)
        g.setStroke(new BasicStroke(2))
      } else {
        g.setColor(Color.BLACK)
        g.setStroke(new BasicStroke(1))
      }

      g.draw(ed.path)

      ed.label map { ld =>
        ld.backgroundColor.map { bg =>
          g.setColor(bg)
          g.fill(new Rectangle2D.Double(
            ld.bounds.getMinX - 3.0, ld.bounds.getMinY - 3.0,
            ld.bounds.getWidth + 6.0, ld.bounds.getHeight + 6.0))
        }
        g.setColor(ld.foregroundColor)
        g.setFont(EdgeLabelFont)
        g.drawString(ld.text, ld.bounds.getMinX.toFloat, ld.baseline.toFloat)
      }
    }

    g.setStroke(new BasicStroke(1))

    for ((v, VDisplay(shape,color,label)) <- vertexDisplay) {
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

      label.map { ld =>
        ld.backgroundColor.map { bg =>
          g.setColor(bg)
          g.fill(new Rectangle2D.Double(
            ld.bounds.getMinX - 3.0, ld.bounds.getMinY - 3.0,
            ld.bounds.getWidth + 6.0, ld.bounds.getHeight + 6.0))
        }
        g.setColor(ld.foregroundColor)
        g.setFont(VertexLabelFont)
        g.drawString(ld.text, ld.bounds.getMinX.toFloat, ld.baseline.toFloat)
      }
    }

    g.setStroke(new BasicStroke(1))

    selectionBox.map { rect =>
      g.setColor(new Color(0.5f,0.5f,1f,0.1f))
      g.fill(rect)
      g.setColor(new Color(0.5f,0.5f,1f,0.4f))
      g.draw(rect)
    }
  }

  // scrollable trait data
  def preferredViewportSize: Dimension = preferredSize

  def tracksViewportHeight: Boolean = false
  def tracksViewportWidth: Boolean = false

  def blockIncrement(visibleRect: Rectangle, orientation: Orientation.Value, direction: Int): Int = 50
  def unitIncrement(visibleRect: Rectangle, orientation: Orientation.Value, direction: Int): Int = 50
}

object GraphView {
  final val NodeRadius = 0.16
  final val WireRadius = 0.1
  final val ArrowheadLength = 0.1
  final val ArrowheadAngle = 0.25 * Pi
  final val EdgeSelectionRadius = 3.0
  final val VertexLabelFont = new Font("Dialog", AWTFont.PLAIN, 12)
  final val EdgeLabelFont = new Font("Dialog", AWTFont.PLAIN, 10)

  final val AxisColor = new Color(0.8f,0.8f,0.9f)
  final val MajorColor = new Color(0.85f,0.85f,1.0f)
  final val MinorColor = new Color(0.9f,0.9f,1.0f)
}

