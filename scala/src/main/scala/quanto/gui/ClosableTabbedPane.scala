package quanto.gui

import scala.swing._
import javax.swing.border.EmptyBorder
import javax.swing.Icon
import java.awt.{Color, BasicStroke, RenderingHints, Graphics}

class ClosablePage(title0: String, component0: Component, val closeAction: () => Boolean)
extends TabbedPane.Page(title0, component0) {
  lazy val tabComponent : ClosablePage.TabComponent = { new ClosablePage.TabComponent(this) }

  override def title_=(t: String) {
    super.title_=(t)
    tabComponent.title = t
  }
}

object ClosablePage {
  def apply(title: String, component: Component)(closeAction: => Boolean) =
    new ClosablePage(title, component, () => closeAction)

  class TabComponent(p: ClosablePage) extends BoxPanel(Orientation.Horizontal) {
    opaque = false
    val titleLabel = new Label(p.title)
    titleLabel.border = new EmptyBorder(new Insets(5,5,5,10))
    contents += titleLabel

    def title = titleLabel.text
    def title_=(t: String) {
      titleLabel.text = t
    }

    val closeButton = new Button(Action("") {
      if (p.closeAction()) {
        if (p.parent != null) p.parent.pages -= p
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

class ClosableTabbedPane extends TabbedPane { tabbedPane =>
  def +=(p: ClosablePage) {
    pages += p
    peer.setTabComponentAt(pages.length-1, p.tabComponent.peer)
  }

  def currentContent: Option[Component] =
    if (selection.index == -1) None
    else Some(selection.page.content)
}


