package quanto.gui

import scala.swing._
import scala.swing.event.{ButtonClicked, Key, KeyPressed, ValueChanged}
import quanto.data._


class SimulatedAnnealingDialog(project: Project) extends Dialog {
  modal = true

  implicit def buttonIsSelected(radButton: RadioButton): Boolean = radButton.selected

  val AnnealButton = new Button("Anneal")
  val CancelButton = new Button("Cancel")
  defaultButton = Some(AnnealButton)


  val MainPanel = new BoxPanel(Orientation.Vertical) {

    def vertexLimit(): Option[Int] = {
      try {
        if (radYesVertexLimit) {
          Some(VertexLimit.text.toInt)
        } else {
          None
        }
      } catch {
        case e: Error => None
      }
    }

    val TimeSteps = new TextField
    TimeSteps.text = "100"
    TimeSteps.preferredSize = new Dimension(50,20)

    val radNoVertexLimit = new RadioButton("None")
    val radYesVertexLimit = new RadioButton("")

    val VertexLimit = new TextField
    VertexLimit.text = "50"
    VertexLimit.preferredSize = new Dimension(50,20)
    VertexLimit.enabled = false
    VertexLimit.editable = false

    radNoVertexLimit.selected = true

    val radGroupIncludeInverse = new ButtonGroup(radNoVertexLimit, radYesVertexLimit)

    contents += Swing.VStrut(10)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(10), new Label("Time steps:"), Swing.HStrut(5), TimeSteps, Swing.HStrut(10))
    }
    contents += Swing.VStrut(5)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(10), new Label("Limit vertex count:"))
      contents += (Swing.HStrut(10), radNoVertexLimit, Swing.HStrut(10))
      contents += (Swing.HStrut(10), radYesVertexLimit, Swing.HStrut(10), VertexLimit, Swing.HStrut(10))
    }
    contents += Swing.VStrut(5)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (Swing.HStrut(20),AnnealButton, Swing.HStrut(10), CancelButton,Swing.HStrut(20))
    }

    contents += Swing.VStrut(10)
  }

  contents = MainPanel

  listenTo(AnnealButton, CancelButton, MainPanel.radYesVertexLimit, MainPanel.radNoVertexLimit)

  reactions += {
    case ButtonClicked(AnnealButton) =>
      close()
    case ButtonClicked(CancelButton) =>
      MainPanel.TimeSteps.text = "0"
      close()
    case ButtonClicked(MainPanel.radNoVertexLimit) | ButtonClicked(MainPanel.radYesVertexLimit) =>
      MainPanel.VertexLimit.enabled = MainPanel.radYesVertexLimit
      MainPanel.VertexLimit.editable = MainPanel.radYesVertexLimit
  }
}
