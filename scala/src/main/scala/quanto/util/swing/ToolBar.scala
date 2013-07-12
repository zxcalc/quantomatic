package quanto.util.swing

import swing._
import javax.swing.JToolBar

class ToolBar extends Component with SequentialContainer.Wrapper {
  override lazy val peer: JToolBar = new JToolBar
  def add( action: Action ) { peer.add( action.peer )}
  def add( component: Component ) { peer.add( component.peer )}

  def addSeparator () {peer.addSeparator()};
}
