package quanto.gui.histview

import swing._
import java.awt.{Font => AWTFont, Color, RenderingHints}
import quanto.util._
import java.awt.geom.Rectangle2D

class HistViewItem[A](decorate: Seq[TreeLink[A]], item: A, selected: Boolean,
                      fm: java.awt.FontMetrics) extends Component {

  val cellHeight = fm.getHeight + 15
  val baseLine = fm.getHeight + 5
  preferredSize = new Dimension(250,cellHeight)

  override def paintComponent(g: Graphics2D) {
    if (selected) {
      val rect = new Rectangle2D.Double(1.0,1.0,248.0,cellHeight-2.0)
      g.setColor(new Color(0.7f,0.7f,0.9f))
      g.fill(rect)
    }
    
    g.setColor(Color.BLACK)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setFont(HistView.ItemFont)
    g.drawString(item.toString, 5, baseLine)
  }
}
