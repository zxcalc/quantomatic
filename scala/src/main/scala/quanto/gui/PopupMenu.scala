package quanto.gui

import swing._
import swing.event._
import javax.swing.JPopupMenu

class PopupMenu extends Component
{
  override lazy val peer : JPopupMenu = new JPopupMenu

  def add(item:MenuItem) : Unit = { peer.add(item.peer) }
  def setVisible(visible:Boolean) : Unit = { peer.setVisible(visible) }
  def show(invoker: Component, x: Int, y: Int): Unit = peer.show(invoker.peer, x, y)

  val itemUnfold = new MenuItem(new Action("Unfold") {
    def apply = println("Unfold is not implemented");
  })

  val itemOpen = new MenuItem(new Action("Open") {
    def apply = println("Open is not implemented");
  })

  val itemSeperate = new MenuItem(new Action("Seperate") {
    def apply = println("Seperate is not implemented");
  })

  val _ = List(itemUnfold, itemOpen, itemSeperate) map this.add  /* add items to popup menu */

  def itemsEnabled_= (unfold : Boolean , open: Boolean, sep: Boolean) = {
    itemUnfold.enabled_= (unfold)
    itemOpen.enabled_= (open)
    itemSeperate.enabled_= (sep)
  }

  def unfoldAction_= (act: Unit) = {
    itemUnfold.action_= (new Action ("Unfold"){
      def apply = act;
    })
  }

  def openAction_= (act: Unit) = {
    itemOpen.action_= (new Action ("Open"){
      def apply = act;
    })
  }

  def seperateAction_= (act: Unit) = {
    itemSeperate.action_= (new Action ("Seperate"){
      def apply = act;
    })
  }
}


