package quanto.gui.graphview

import java.awt.geom.{Ellipse2D, Point2D, Rectangle2D}
import java.awt.{Color, FontMetrics, Shape}

import math._
import quanto.data._
import quanto.gui._
import quanto.core.data.TexConstants
import quanto.util.UserOptions

case class VDisplay(shape: Shape, color: Color, label: Option[LabelDisplayData]) {
  def pointHit(pt: Point2D) = {
    val bnd = shape.getBounds2D
    pt.getX >= bnd.getMinX - GraphView.VertexSelectionTolerence &&
      pt.getX <= bnd.getMaxX + GraphView.VertexSelectionTolerence &&
      pt.getY >= bnd.getMinY - GraphView.VertexSelectionTolerence &&
      pt.getY <= bnd.getMaxY + GraphView.VertexSelectionTolerence
  }
  def rectHit(r: Rectangle2D) = shape.intersects(r)
}

trait VertexDisplayData { self: GraphView =>

  val vertexDisplay = collection.mutable.Map[VName,VDisplay]()

  // returns the contact point at the given angle, in graph coordinates
  protected def vertexContactPoint(vn: VName, angle: Double): (Double,Double) = {
    // TODO: replace this with proper boundary detection
    val c = graph.vdata(vn).coord

    vertexDisplay(vn).shape match {
      case e: Ellipse2D =>
        val radius = trans.scaleFromScreen(e.getWidth) / 2.0
        (c._1 + radius * cos(angle),
         c._2 + radius * sin(angle))
      case r: Rectangle2D =>
        val chopX = (trans scaleFromScreen r.getWidth) / 2 + 0.01
        val chopY = (trans scaleFromScreen r.getHeight) / 2 + 0.01
        val tryRad = max(chopX, chopY)

        val rad = if (abs(tryRad * cos(angle)) > chopX) {
          abs(chopX / cos(angle))
        } else if (abs(tryRad * sin(angle)) > chopY) {
          abs(chopY / sin(angle))
        } else {
          tryRad
        }

        (c._1 + rad * cos(angle), c._2 + rad * sin(angle))
    }
  }

  protected def computeVertexDisplay() {
    val trWireWidth = 0.707 * (trans scaleToScreen GraphView.WireRadius)

    // Go through every vertex in the graph
    for ((v,data) <- graph.vdata if !vertexDisplay.contains(v)) {
      val coord = if(data.coord == (0.0,0.0)){
        val s = 5
        (s*(Math.random()-0.5), s*(Math.random()-0.5))
      } else data.coord
      val (x,y) = trans toScreen coord

      vertexDisplay(v) = data match {
        case vertexData : NodeV =>
          val style = vertexData.typeInfo.style
          val label = if (vertexData.hasValue) vertexData.phaseData.toString else vertexData.value
          val text = if(zoom < GraphView.zoomCutOut && label != "") "~"
                     else TexConstants.translate(label)
            /*vertexData.typeInfo.value.typ match {
            case Theory.ValueType.String => vertexData.value
            case _ => ""
          }*/

          // Build the label
          val fm = peer.getGraphics.getFontMetrics(GraphView.VertexLabelFont)
          val labelDisplay = LabelDisplayData(
            text, (x, y), fm,
            vertexData.typeInfo.style.labelForegroundColor,
            vertexData.typeInfo.style.labelBackgroundColor)

          // Build the shape around the label
          val shape = style.shape match {
            case Theory.VertexShape.Rectangle =>
              val buffer = trans.scaleToScreen(GraphView.NodeTextPadding)
              val height = labelDisplay.bounds.getHeight + buffer
              val widthFromLabel = labelDisplay.bounds.getWidth + buffer
              // Default to square if no data, and stretch horizontally if needed
              val width = max(widthFromLabel, height)


              new Rectangle2D.Double(
                labelDisplay.bounds.getMinX - (width - labelDisplay.bounds.getWidth) / 2.0,
                labelDisplay.bounds.getMinY - (height - labelDisplay.bounds.getHeight) / 2.0,
                width, height)
            case Theory.VertexShape.Circle =>
              // radius should fit to label if required
              val r = max(
                (labelDisplay.bounds.getWidth / 2.0) + trans.scaleToScreen(GraphView.NodeTextPadding),
                trans.scaleToScreen(GraphView.NodeRadius)
              )

              new Ellipse2D.Double(
                labelDisplay.bounds.getCenterX - r,
                labelDisplay.bounds.getCenterY - r,
                2.0 * r, 2.0 * r)
            case _ => throw new Exception("Shape not supported yet")
          }

          VDisplay(shape, style.fillColor, Some(labelDisplay))
        case _: WireV =>
          VDisplay(
            new Rectangle2D.Double(
              x - trWireWidth, y - trWireWidth,
              2.0 * trWireWidth, 2.0 * trWireWidth),
            Color.GRAY,None)
      }
    }
  }

  protected def boundsForVertexSet(vset: Set[VName]) = {
    var init = false
    var ulx,uly,lrx,lry = 0.0

    vset.foreach { v =>
      val rect = vertexDisplay(v).shape.getBounds
      if (init) {
        ulx = min(ulx, rect.getX)
        uly = min(uly, rect.getY)
        lrx = max(lrx, rect.getMaxX)
        lry = max(lry, rect.getMaxY)
      } else {
        ulx = rect.getX
        uly = rect.getY
        lrx = rect.getMaxX
        lry = rect.getMaxY
        init = true
      }
    }
    
    val bounds = new Rectangle2D.Double(ulx, uly, lrx - ulx, lry - uly)
    val em = trans.scaleToScreen(0.1)
    val p = (bounds.getX - 3*em, bounds.getY - 3*em)
    val q = (bounds.getWidth + 6*em, bounds.getHeight + 6*em)

    new Rectangle2D.Double(p._1, p._2, q._1, q._2)
  }

  def invalidateAllVerts() { vertexDisplay.clear() }
  def invalidateVertex(n: VName) = vertexDisplay -= n
}
