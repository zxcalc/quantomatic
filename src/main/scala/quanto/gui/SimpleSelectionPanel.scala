package quanto.gui

import quanto.data.Project

import scala.swing.event.ButtonClicked
import scala.swing._

class SimpleSelectionPanel(project: Project, instruction: String , options: List[String]) extends Dialog {
  modal = true
  val AddButton = new Button("Accept")
  val CancelButton = new Button("Cancel")
  val MainPanel = new BoxPanel(Orientation.Vertical) {
    var OptionList : ListView[String] = new ListView[String](options)
    val SelectionPane = new ScrollPane(OptionList)
    SelectionPane.preferredSize = new Dimension(400,200)


    contents += Swing.VStrut(10)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(10), new Label(instruction), Swing.HStrut(5))
    }
    contents += Swing.VStrut(5)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(10), SelectionPane, Swing.HStrut(10))
    }
    contents += Swing.VStrut(5)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(50), AddButton, Swing.HStrut(10), CancelButton, Swing.HStrut(50))
    }

    contents += Swing.VStrut(10)
  }

  contents = MainPanel

  listenTo(AddButton, CancelButton)


  reactions += {
    case ButtonClicked(AddButton) =>
      close()
    case ButtonClicked(CancelButton) =>
      MainPanel.OptionList.selection.indices.clear()
      close()
  }
}
