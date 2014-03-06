package quanto.gui

import scala.swing._
import scala.swing.event.ButtonClicked

class NewProjectDialog extends Dialog {
  modal = true
  val NameField = new TextField()
  val LocationField = new TextField(System.getProperty("user.home"))
  val BrowseButton = new Button("...")
  // TODO: make these not hard-coded
  val theoryNames = Vector("Red/Green", "Proof Strategy Graph", "String Vertex/Edge")
  val theoryFiles = Vector("red_green", "strategy_graph", "string_ve")

  val TheoryField = new ComboBox(theoryNames)
  val CreateButton = new Button("Create")
  val CancelButton = new Button("Cancel")
  defaultButton = Some(CreateButton)

  var result : Option[(String,String,String)] = None

  val mainPanel = new BoxPanel(Orientation.Vertical) {

    contents += Swing.VStrut(10)

    contents += new BoxPanel(Orientation.Horizontal) {
      val nameLabel = new Label("Name", null, Alignment.Right)
      nameLabel.preferredSize = new Dimension(80, 30)
      LocationField.preferredSize = new Dimension(235, 30)

      contents += (Swing.HStrut(10), nameLabel, Swing.HStrut(5), NameField, Swing.HStrut(10))
    }

    contents += Swing.VStrut(5)

    contents += new BoxPanel(Orientation.Horizontal) {
      val locationLabel = new Label("Location", null, Alignment.Right)
      locationLabel.preferredSize = new Dimension(80, 30)
      LocationField.preferredSize = new Dimension(200, 30)
      BrowseButton.preferredSize = new Dimension(30, 30)

      contents += (Swing.HStrut(10), locationLabel, Swing.HStrut(5), LocationField,
        Swing.HStrut(5), BrowseButton, Swing.HStrut(10))
    }

    contents += Swing.VStrut(5)

    contents += new BoxPanel(Orientation.Horizontal) {
      val theoryLabel = new Label("Theory ", null, Alignment.Right)
      theoryLabel.preferredSize = new Dimension(80, 30)
      TheoryField.preferredSize = new Dimension(235, 30)

      contents += (Swing.HStrut(10), theoryLabel, Swing.HStrut(5), TheoryField, Swing.HStrut(10))
    }

    contents += Swing.VStrut(5)

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (CreateButton, Swing.HStrut(5), CancelButton)
    }

    contents += Swing.VStrut(10)
  }

  contents = mainPanel

  listenTo(BrowseButton, CreateButton, CancelButton)

  reactions += {
    case ButtonClicked(CreateButton) =>
      result = Some((theoryFiles(TheoryField.peer.getSelectedIndex), NameField.text, LocationField.text))
      close()
    case ButtonClicked(CancelButton) =>
      close()
    case ButtonClicked(BrowseButton) =>
      val chooser = new FileChooser()
      chooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
      chooser.showOpenDialog(mainPanel) match {
        case FileChooser.Result.Approve =>
          LocationField.text = chooser.selectedFile.toString
        case _ =>
      }
  }
}
