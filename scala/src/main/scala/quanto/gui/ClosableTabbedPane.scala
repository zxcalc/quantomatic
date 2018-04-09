package quanto.gui

import java.awt.event.{MouseAdapter, MouseEvent}

import scala.swing._
import javax.swing.border.EmptyBorder
import javax.swing.{Icon, ImageIcon, SwingUtilities}
import java.awt.{BasicStroke, Color, Graphics, Point, RenderingHints}

import quanto.gui.QuantoDerive.popup
import quanto.util.{UserAlerts, UserOptions}

import scala.swing.event._

class ClosablePage(title0: String, component0: Component, val closeAction: () => Boolean)
  extends TabbedPane.Page(title0, component0) {
  lazy val tabComponent: ClosablePage.TabComponent = {
    new ClosablePage.TabComponent(this)
  }

  override def title_=(t: String) {
    super.title_=(t)
    tabComponent.title = t
  }
}

case class PageClosed(p: ClosablePage) extends DocumentEvent


object ClosablePage {
  def apply(title: String, component: Component)(closeAction: => Boolean) =
    new ClosablePage(title, component, () => closeAction)

  class TabComponent(p: ClosablePage) extends BoxPanel(Orientation.Horizontal) {
    opaque = false
    val icon = p match {
      case _: GraphDocumentPage => new ImageIcon(getClass.getResource("graph-icon.png"), "Graph")
      case _: RuleDocumentPage => new ImageIcon(getClass.getResource("rule-icon.png"), "Rule")
      case _: DerivationDocumentPage => new ImageIcon(getClass.getResource("derive-icon.png"), "Derivation")
      case _: PythonDocumentPage => new ImageIcon(getClass.getResource("text-x-script.png"), "Python Script")
      case _ => new ImageIcon(getClass.getResource("text-x-generic.png"), "Document")
    }
    val titleLabel = new Label(p.title, icon, Alignment.Left)
    val closeButton = new Button(Action("") {
closePage()
    })
    titleLabel.border = new EmptyBorder(new Insets(5, 5, 5, 10))
    contents += titleLabel

    def closePage(): Unit = {
      if (p.closeAction()) {
        publish(PageClosed(p))
      } else {
      }
    }

    def TabContextMenu(): PopupMenu = new PopupMenu {
      menu =>

      val CloseJustThis: Action = new Action("Close this tab") {

        menu.contents += new MenuItem(this)

        def apply(): Unit = closePage()
      }


      val CloseOtherTabs: Action = new Action("Close all other tabs") {
        menu.contents += new MenuItem(this)

        def apply(): Unit = {
          QuantoDerive.closeAllOrListOfDocuments(Some(QuantoDerive.MainDocumentTabs.documents.toList.filter(q => q != p)))
        }
      }
    }

    def title = titleLabel.text

    def title_=(t: String) {
      titleLabel.text = t
    }

    closeButton.border = new EmptyBorder(new Insets(0, 0, 0, 0))
    closeButton.contentAreaFilled = false
    closeButton.rolloverEnabled = true
    closeButton.icon = new CloseIcon(false)
    closeButton.rolloverIcon = new CloseIcon(true)
    contents += closeButton

    //listenTo(titleLabel.mouse.clicks)
    // Once we know how to dispatch all events further down the hierarchy we can use this
    // Until then, any attempt to pick up mouse events blocks native behaviour
    reactions += {
      case e : MouseEvent =>
        //QuantoDerive.MainDocumentTabs.focus(p.asInstanceOf[DocumentPage])
        UserAlerts.alert("Should have popped up")
        if(e.isPopupTrigger) popup(TabContextMenu(), Some(e))
        peer.dispatchEvent(SwingUtilities.convertMouseEvent(e.getComponent, e, peer))
    }
  }

}

// draw a little X
private class CloseIcon(rollover: Boolean) extends Icon {
  def paintIcon(c: java.awt.Component, g: Graphics, x: Int, y: Int): Unit = {
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

  def getIconWidth : Int = UserOptions.scaleInt(9)

  def getIconHeight : Int = UserOptions.scaleInt(9)
}

// Handler for the TabbedPane object, to distance the swing from the Java
// TabbedPanes hold Java Components, but we want to interact with Documents
class DocumentTabs {

  val tabbedPane = new TabbedPane()
  private var pageIndex: Map[DocumentPage, Int] = Map()

  def component: TabbedPane = tabbedPane

  def +=(p: DocumentPage) {
    if (!pageIndex.contains(p)) {
      pages += p
      tabbedPane.peer.setTabComponentAt(pages.length - 1, p.tabComponent.peer)
      pageIndex += (p -> (pages.length - 1))
    }
    reorderPages()
    focus(p)
  }

  def focus(index: Int): Unit = {
    selection.index = index
  }

  def remove(page: DocumentPage): Unit = {
    val removedIndex = pageIndex(page)
    val preFocus = currentFocus
    remove(removedIndex)
    pageIndex -= page
    reorderPages()
    if (preFocus.nonEmpty && preFocus.get != page) {
      focus(preFocus.get)
    } else {
      // Handled by peer
      //selection.index -=1
    }
    ensureFocusIsValid()
  }

  private def reorderPages(): Unit = {
    val indexPages = pageIndex.map(pi => (pi._2, pi._1)).toList.sortBy(_._1)
    var newPageIndex: Map[DocumentPage, Int] = Map()
    for (iip <- indexPages.zipWithIndex) {
      newPageIndex += (iip._1._2 -> iip._2)
    }
    pageIndex = newPageIndex

  }

  private def remove(index: Int): Unit = {
    pages.remove(index)
    for (page <- pageIndex.keys) {
      if (pageIndex(page) > index) {
        pageIndex += (page -> (pageIndex(page) - 1))
      }
    }
  }

  private def ensureFocusIsValid(): Unit = {
    if (pages.length > 0) {
      // There is something to focus on
      if (selection.index >= pages.length) {
        selection.index = pages.length - 1
      }
      if (selection.index < 0) {
        selection.index = 0
      }
    }
  }

  def selection = tabbedPane.selection

  private def pages = tabbedPane.pages

  def focus(p: DocumentPage): Unit = {
    try {
      selection.index = pageIndex(p)
    } catch {
      case e: Exception => selection.index = -1
    }
  }

  def currentFocus: Option[DocumentPage] = {
    pageIndex.find(kv => kv._2 == selection.index).map(_._1)
  }

  def currentContent: Option[Component] = {
    if (selection.index == -1 || selection.page == null || selection.page.content == null) None
    else Some(selection.page.content)
  }

  def publishChanged(): Unit = {
    tabbedPane.selection.publish(SelectionChanged(tabbedPane))
  }

  def documents: Iterable[DocumentPage] = pageIndex.keys

  def clear(): Unit = {
    pages.clear()
  }
}


