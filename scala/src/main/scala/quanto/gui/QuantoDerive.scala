package quanto.gui

import scala.swing._
import java.awt.Dimension
import scala.swing.event.Key
import javax.swing.KeyStroke
import java.awt.event.KeyEvent


object QuantoDerive extends SimpleSwingApplication {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  object Split extends SplitPane {
    orientation = Orientation.Vertical
    contents_=(new BorderPanel(), new BorderPanel())
  }

  object NewDialog extends Dialog(owner = top) {
    modal = true
    contents = new Button("Foo!")
  }

  val FileMenu = new Menu("File") { menu =>
    mnemonic = Key.F

    val NewProjectAction = new Action("New Project...") {
      menu.contents += new MenuItem(this) { mnemonic = Key.N }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_N, CommandMask | Key.Modifier.Shift))
      def apply() { NewDialog.open() }
    }

    val OpenProjectAction = new Action("Open Project...") {
      menu.contents += new MenuItem(this) { mnemonic = Key.O }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_O, CommandMask | Key.Modifier.Shift))
      def apply() {  }
    }
  }

  def top = new MainFrame {
    title = "QuantoDerive"
    contents = Split

    size = new Dimension(1000,800)

    menuBar = new MenuBar {
      contents += (FileMenu)
    }
  }
}
