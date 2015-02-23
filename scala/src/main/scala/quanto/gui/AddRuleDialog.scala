package quanto.gui

import scala.swing._
import scala.swing.event.{Key, KeyPressed, ButtonClicked}
import quanto.data._
import quanto.util.Globals


class AddRuleDialog(project: Project) extends Dialog {
  modal = true

  def result: Seq[RuleDesc] =
    MainPanel.RuleList.selection.items.map { s =>
      RuleDesc(s, inverse = MainPanel.InverseCheckbox.selected)
    }

  val AddButton = new Button("Add")
  val CancelButton = new Button("Cancel")
  defaultButton = Some(AddButton)

//  val dir = Files.newDirectoryStream(Paths.get(rootDir), "**/*.qrule")
//  for (p <- dir.asScala) println(p)

  val MainPanel = new BoxPanel(Orientation.Vertical) {
    val Search = new TextField
    val RuleList = new ListView[String](project.rules)

    val RulePane = new ScrollPane(RuleList)
    val InverseCheckbox = new CheckBox("Inverse")
    RulePane.preferredSize = new Dimension(400,200)

    contents += Swing.VStrut(10)
//    TODO: add filtering
//    contents += new BoxPanel(Orientation.Horizontal) {
//      contents += (Swing.HStrut(10), new Label("Filter:"), Swing.HStrut(5), Search, Swing.HStrut(10))
//    }
//    contents += Swing.VStrut(5)
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

  contents = MainPanel

  listenTo(AddButton, CancelButton)

  reactions += {
    case ButtonClicked(AddButton) =>
      close()
    case ButtonClicked(CancelButton) =>
      MainPanel.RuleList.selection.indices.clear()
      close()
  }
}
