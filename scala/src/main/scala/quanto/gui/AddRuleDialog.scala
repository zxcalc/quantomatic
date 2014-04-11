package quanto.gui

import scala.swing._
import scala.swing.event.ButtonClicked

case class RuleDesc(name: String, inverse: Boolean)

class AddRuleDialog extends Dialog {
  modal = true
  var result: Option[String] = None

  val AddButton = new Button("Add")
  val CancelButton = new Button("Cancel")
  defaultButton = Some(AddButton)



  val mainPanel = new BoxPanel(Orientation.Vertical) {
    val Search = new TextField
    val RuleList = new ListView
    val RulePane = new ScrollPane(RuleList)
    val InverseCheckbox = new CheckBox("Inverse")
    RulePane.preferredSize = new Dimension(400,200)

    contents += Swing.VStrut(10)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(10), new Label("Search:"), Swing.HStrut(5), Search, Swing.HStrut(10))
    }
    contents += Swing.VStrut(5)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(10), RulePane, Swing.HStrut(10))
    }
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(10), InverseCheckbox, Swing.HStrut(10))
    }
    contents += Swing.VStrut(5)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (AddButton, Swing.HStrut(5), CancelButton)
    }

    contents += Swing.VStrut(10)
  }

  contents = mainPanel

  listenTo(AddButton, CancelButton)

  reactions += {
    case ButtonClicked(AddButton) =>
      result = Some("foo")
      close()
    case ButtonClicked(CancelButton) =>
      close()
  }
}
