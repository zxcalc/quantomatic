package quanto.gui.histview

import swing._
import java.awt.{Font => AWTFont, BasicStroke, Color, RenderingHints}
import quanto.util._
import java.awt.geom.{Ellipse2D, Path2D, Line2D, Rectangle2D}

class HistViewItem[A](decorate: Seq[TreeLink[A]], item: A, selected: Boolean,
                      fm: java.awt.FontMetrics) extends Component {

  val cellHeight = fm.getHeight + 15.0
  val baseLine = fm.getHeight + 5.0
  preferredSize = new Dimension(250,cellHeight.toInt)
  val xIncrement = 15.0
  val circleRadius = 5.0

  override def paintComponent(g: Graphics2D) {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    if (selected) {
      val rect = new Rectangle2D.Double(1.0,1.0,248.0,cellHeight-2.0)
      g.setColor(new Color(0.7f,0.7f,0.9f))
      g.fill(rect)
    }

    var topIndex = 1
    var bottomIndex = 1
    var textIndex = 1

    g.setColor(Color.BLACK)
    g.setStroke(new BasicStroke(3, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER))

    for (d <- decorate) d match {
      case WireLink(_) =>
        val topX = topIndex * xIncrement
        val bottomX = bottomIndex * xIncrement

        val p = new Path2D.Double()
        p.moveTo(topX,-1.0)
        p.lineTo(topX,4.0)
        p.lineTo(bottomX,cellHeight-4.0)
        p.lineTo(bottomX,cellHeight + 1.0)

        g.draw(p)

        topIndex += 1
        bottomIndex += 1
        textIndex = math.max(topIndex,bottomIndex)
      case NodeLink(inputOpt, outputs) =>
        val topX = topIndex * xIncrement
        val centerY = cellHeight/2.0

        inputOpt.foreach { _ =>
          g.draw(new Line2D.Double(topX,-1.0,topX,centerY))
          topIndex += 1
        }

        if (outputs.isEmpty) bottomIndex += 1
        else outputs.foreach { _ =>
          val bottomX = bottomIndex * xIncrement

          val p = new Path2D.Double()
          p.moveTo(topX,centerY)
          p.lineTo(bottomX,cellHeight-4.0)
          p.lineTo(bottomX,cellHeight + 1.0)

          g.draw(p)
          bottomIndex += 1
        }

        textIndex = math.max(topIndex,bottomIndex)

        // draw the node
        g.fill(new Ellipse2D.Double(topX - circleRadius, centerY - circleRadius, circleRadius * 2.0, circleRadius * 2.0))
      case SpaceLink(collapseBottom, collapseTop) =>
        if (!collapseBottom) bottomIndex += 1
        if (!collapseTop) topIndex += 1
    }

    val leftX = textIndex * xIncrement + 8.0

    g.setStroke(new BasicStroke(1))
    g.setFont(HistView.ItemFont)
    g.drawString(item.toString, leftX.toInt, baseLine.toInt)
  }
}
