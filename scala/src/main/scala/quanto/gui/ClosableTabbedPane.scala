package quanto.gui

import scala.swing._
import scala.swing.TabbedPane.Page
import javax.swing.border.EmptyBorder
import javax.swing.Icon
import java.awt.{Color, BasicStroke, RenderingHints, Graphics}

class ClosablePage(title: String, component: Component, val closeAction: () => Boolean)
extends TabbedPane.Page(title, component)

object ClosablePage {
  def apply(title: String, component: Component)(closeAction: => Boolean) =
    new ClosablePage(title, component, () => closeAction)
}

class ClosableTabbedPane extends TabbedPane { tabbedPane =>
  def +=(p: ClosablePage) {
    pages += p
    println(peer.getTabCount)

    peer.setTabComponentAt(pages.length-1, new TabComponent(p).peer)
  }

  private class TabComponent(p: ClosablePage) extends BoxPanel(Orientation.Horizontal) {
    opaque = false
    val titleLabel = new Label(p.title)
    titleLabel.border = new EmptyBorder(new Insets(5,5,5,10))
    contents += titleLabel
    val closeButton = new Button(Action("") {
      if (p.closeAction()) {
        tabbedPane.pages -= p
        printf("got successful close")
      } else {
        printf("tried to close")
      }
    })
    closeButton.border = new EmptyBorder(new Insets(0,0,0,0))
    closeButton.contentAreaFilled = false
    closeButton.rolloverEnabled = true
    closeButton.icon = new CloseIcon(false)
    closeButton.rolloverIcon = new CloseIcon(true)
    contents += closeButton
  }
}

// draw a little X
private class CloseIcon(rollover : Boolean) extends Icon {
  def getIconWidth = 9
  def getIconHeight = 9
  def paintIcon(c : java.awt.Component, g : Graphics, x : Int, y : Int) : Unit = {
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    val savedStroke = g2.getStroke
    g2.setStroke(new BasicStroke(2))
    if (rollover) g2.setColor(Color.black)
    else g2.setColor(Color.gray)
    g2.drawLine(2, 2, getIconWidth - 3, getIconHeight - 3)
    g2.drawLine(getIconWidth - 3, 2, 2, getIconHeight - 3)
    g2.setStroke(savedStroke)
  }
}
