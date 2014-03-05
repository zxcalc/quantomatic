package quanto.gui

import scala.swing._
import javax.swing.JTree

class FileTree extends BorderPanel {
  val tree = new JTree
  val scrollPane = new ScrollPane

  scrollPane.peer.add(tree)
  add(scrollPane, BorderPanel.Position.Center)
}
