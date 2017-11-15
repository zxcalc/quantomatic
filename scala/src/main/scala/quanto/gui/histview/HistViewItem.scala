package quanto.gui.histview

import swing._
import java.awt.{BasicStroke, Color, RenderingHints}

import quanto.util.TreeSeq._
import java.awt.geom.{Ellipse2D, Line2D, Path2D, Rectangle2D}

import quanto.gui.UserOptions

class HistViewItem[A <: HistNode](decorate: Seq[Decoration[A]], item: A, selected: Boolean,
                      sz: Dimension) extends Component {
  private def scale(d: Double) : Double = UserOptions.scale(d)
  private def scaleInt(n: Int) : Int = UserOptions.scaleInt(n)
  import HistView.xIncrement
  preferredSize = sz
  val cellHeight = preferredSize.getHeight
  val cellWidth = preferredSize.getWidth
  val baseLine = cellHeight - scale(10.0)
  val circleRadius = cellHeight / 4.0

  override def paintComponent(g: Graphics2D) {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    if (selected) {
      val rect = new Rectangle2D.Double(1.0,1.0,cellWidth-scale(2.0),cellHeight-scale(2.0))
      g.setColor(new Color(0.7f,0.8f,0.95f))
      g.fill(rect)
      g.setStroke(new BasicStroke(scaleInt(1)))
      g.setColor(new Color(0.4f,0.4f,0.6f))
      g.draw(rect)
    }

    var topIndex = 1
    var bottomIndex = 1
    var textIndex = 1

    g.setColor(Color.BLACK)
    g.setStroke(new BasicStroke(scaleInt(3), BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER))

    for (d <- decorate) d match {
      case WireLink(_) =>
        val topX = topIndex * xIncrement
        val bottomX = bottomIndex * xIncrement

        val p = new Path2D.Double()
        p.moveTo(topX,scale(-1.0))
        p.lineTo(topX,scale(4.0))
        p.curveTo(topX,scale(12.0),bottomX,cellHeight-scale(12.0),bottomX,cellHeight-scale(4.0))
        //p.lineTo(bottomX,cellHeight-4.0)
        p.lineTo(bottomX,cellHeight + scale(1.0))

        g.draw(p)

        topIndex += 1
        bottomIndex += 1
        textIndex = math.max(topIndex,bottomIndex)
      case NodeLink(inputOpt, outputs) =>
        val topX = topIndex * xIncrement
        val centerY = cellHeight/2.0

        inputOpt.foreach { _ =>
          g.draw(new Line2D.Double(topX,scale(-1.0),topX,centerY))
          topIndex += 1
        }

        if (outputs.isEmpty) bottomIndex += 1
        else outputs.foreach { _ =>
          val bottomX = bottomIndex * xIncrement

          val p = new Path2D.Double()
          p.moveTo(topX,centerY)
          //p.lineTo(bottomX,cellHeight-4.0)
          p.curveTo(topX,centerY,bottomX,cellHeight-scale(8.0),bottomX,cellHeight-scale(4.0))
          p.lineTo(bottomX,cellHeight + scale(1.0))

          g.draw(p)
          bottomIndex += 1
        }

        textIndex = math.max(topIndex,bottomIndex)

        // draw the node
        val circle = new Ellipse2D.Double(
          topX - circleRadius,
          centerY - circleRadius,
          circleRadius * 2.0,
          circleRadius * 2.0
        )

        g.setColor(item.color)
        g.fill(circle)
        g.setColor(Color.BLACK)
        g.draw(circle)
      case WhiteSpace(collapseBottom, collapseTop) =>
        if (!collapseBottom) bottomIndex += 1
        if (!collapseTop) topIndex += 1
    }

    val leftX = textIndex * xIncrement
    //val leftX = (decorationWidth(decorate) + 1) * xIncrement

    g.setStroke(new BasicStroke(scaleInt(1)))
    g.setFont(HistView.ItemFont)
    g.drawString(item.label, leftX.toInt, baseLine.toInt)
  }
}
