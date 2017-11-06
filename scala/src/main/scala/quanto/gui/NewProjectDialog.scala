package quanto.gui

import javax.swing.filechooser.FileNameExtensionFilter

import quanto.data.Theory
import quanto.util.FileHelper

import scala.swing._
import scala.swing.event.ButtonClicked

class NewProjectDialog extends Dialog {
  modal = true
  val NameField = new TextField()
  val ProjectLocationField = new TextField(System.getProperty("user.home"))
  val BrowseProjectButton = new Button("...")
  val TheoryLocationField = new TextField("<Select a .qtheory file>")
  val BrowseTheoryButton = new Button("...")
  val theoryName = new TextField("")

  val CreateButton = new Button("Create")
  val CancelButton = new Button("Cancel")
  defaultButton = Some(CreateButton)

  var result : Option[(String,String,String)] = None

  val mainPanel = new BoxPanel(Orientation.Vertical) {

    contents += Swing.VStrut(10)

    contents += new BoxPanel(Orientation.Horizontal) {
      val nameLabel = new Label("Name", null, Alignment.Right)
      nameLabel.preferredSize = new Dimension(80, 30)
      ProjectLocationField.preferredSize = new Dimension(235, 30)

      contents += (Swing.HStrut(10), nameLabel, Swing.HStrut(5), NameField, Swing.HStrut(10))
    }

    contents += Swing.VStrut(5)

    contents += new BoxPanel(Orientation.Horizontal) {
      val locationLabel = new Label("Location", null, Alignment.Right)
      locationLabel.preferredSize = new Dimension(80, 30)
      ProjectLocationField.preferredSize = new Dimension(200, 30)
      BrowseProjectButton.preferredSize = new Dimension(30, 30)

      contents += (Swing.HStrut(10), locationLabel, Swing.HStrut(5), ProjectLocationField,
        Swing.HStrut(5), BrowseProjectButton, Swing.HStrut(10))
    }

    contents += Swing.VStrut(5)

    contents += new BoxPanel(Orientation.Horizontal) {
      val theoryLabel = new Label("Theory ", null, Alignment.Right)
      theoryLabel.preferredSize = new Dimension(80, 30)
      ProjectLocationField.preferredSize = new Dimension(200, 30)
      BrowseTheoryButton.preferredSize = new Dimension(30, 30)

      contents += (Swing.HStrut(10), theoryLabel, Swing.HStrut(5), TheoryLocationField,
        Swing.HStrut(5), BrowseTheoryButton, Swing.HStrut(10))
    }

    contents += Swing.VStrut(5)

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (CreateButton, Swing.HStrut(5), CancelButton)
    }

    contents += Swing.VStrut(10)
  }

  contents = mainPanel

  listenTo(BrowseProjectButton, CreateButton, CancelButton, BrowseTheoryButton)

  reactions += {
    case ButtonClicked(CreateButton) =>
      result = Some((TheoryLocationField.text, NameField.text, ProjectLocationField.text))
      close()
    case ButtonClicked(CancelButton) =>
      close()
    case ButtonClicked(BrowseProjectButton) =>
      val chooser = new FileChooser()
      chooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
      chooser.showOpenDialog(mainPanel) match {
        case FileChooser.Result.Approve =>
          ProjectLocationField.text = chooser.selectedFile.toString
        case _ =>
      }
    case ButtonClicked(BrowseTheoryButton) =>
      val chooser = new FileChooser()
      val qtheoryExtensionFilter = new FileNameExtensionFilter("Theory files", "qtheory")
      chooser.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
      chooser.fileFilter = qtheoryExtensionFilter
      chooser.showOpenDialog(mainPanel) match {
        case FileChooser.Result.Approve =>
          TheoryLocationField.text = chooser.selectedFile.toString
        case _ =>
      }
  }
}
