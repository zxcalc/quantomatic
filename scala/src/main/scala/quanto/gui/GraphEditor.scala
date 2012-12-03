package quanto.gui

import swing._

object GraphEditor extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Quanto Graph Editor"
    contents = new GraphView()
    size = new Dimension(500,500)
  }
}