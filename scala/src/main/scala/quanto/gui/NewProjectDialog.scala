package quanto.gui

import java.io.File
import javax.swing.filechooser.FileNameExtensionFilter

import quanto.data.Theory
import quanto.util.{FileHelper, UserAlerts}

import scala.swing._
import scala.swing.event.{ButtonClicked, SelectionChanged}
import quanto.util.UserOptions.scaleInt

class NewProjectDialog extends Dialog {
  modal = true
  val NameField = new TextField()
  val ProjectLocationField = new TextField(System.getProperty("user.home"))
  val BrowseProjectButton = new Button("...")
  val TheoryChoiceDropdown = new ComboBox(Seq[String]("ZX", "ZW", "From existing project", "From .qtheory file"))
  val TheoryLocationField = new TextField("")
  val BrowseTheoryButton = new Button("...")
  val theoryName = new TextField("")


  val CreateButton = new Button("Create")
  val CancelButton = new Button("Cancel")
  defaultButton = Some(CreateButton)
  val mainPanel = new BoxPanel(Orientation.Vertical) {

    contents += Swing.VStrut(scaleInt(10))

    contents += new BoxPanel(Orientation.Horizontal) {
      val nameLabel = new Label("Name", null, Alignment.Right)
      nameLabel.preferredSize = new Dimension(scaleInt(80), scaleInt(30))
      ProjectLocationField.preferredSize = new Dimension(scaleInt(235), scaleInt(30))

      contents += (Swing.HStrut(scaleInt(10)),
        nameLabel,
        Swing.HStrut(scaleInt(5)),
        NameField,
        Swing.HStrut(scaleInt(10)))
    }

    contents += Swing.VStrut(scaleInt(5))

    contents += new BoxPanel(Orientation.Horizontal) {
      val locationLabel = new Label("Location", null, Alignment.Right)
      locationLabel.preferredSize = new Dimension(scaleInt(80), scaleInt(30))
      ProjectLocationField.preferredSize = new Dimension(scaleInt(200), scaleInt(30))
      BrowseProjectButton.preferredSize = new Dimension(scaleInt(30), scaleInt(30))

      contents += (Swing.HStrut(scaleInt(10)), locationLabel, Swing.HStrut(scaleInt(5)), ProjectLocationField,
        Swing.HStrut(scaleInt(5)), BrowseProjectButton, Swing.HStrut(scaleInt(10)))
    }

    contents += Swing.VStrut(scaleInt(5))

    contents += new BoxPanel(Orientation.Horizontal) {
      val theoryLabel = new Label("Theory ", null, Alignment.Right)
      theoryLabel.preferredSize = new Dimension(scaleInt(80), scaleInt(30))
      ProjectLocationField.preferredSize = new Dimension(scaleInt(200), scaleInt(30))
      BrowseTheoryButton.preferredSize = new Dimension(scaleInt(30), scaleInt(30))

      val locationAndButton : BoxPanel = new BoxPanel(Orientation.Horizontal) {
        contents += (TheoryLocationField, Swing.VStrut(scaleInt(5)), BrowseTheoryButton)
      }

      val theoryAndLocation: BoxPanel = new BoxPanel(Orientation.Vertical) {
        contents += (TheoryChoiceDropdown, Swing.VStrut(scaleInt(5)), locationAndButton)
      }

      contents += (Swing.HStrut(scaleInt(10)), theoryLabel, Swing.HStrut(scaleInt(5)),
        theoryAndLocation, Swing.HStrut(scaleInt(10)))
    }

    contents += Swing.VStrut(scaleInt(5))

    contents += new BoxPanel(Orientation.Horizontal) {
      contents += (CreateButton, Swing.HStrut(scaleInt(5)), CancelButton)
    }

    contents += Swing.VStrut(scaleInt(10))
  }
  var result: Option[(String, String, String)] = None

  contents = mainPanel

  listenTo(BrowseProjectButton, CreateButton, CancelButton, BrowseTheoryButton, TheoryChoiceDropdown.selection)
  var fileChoiceFilter = "*"
  var fileChoiceFilterDescription = ""

  def disableFileChoosers(replacementText: String): Unit = {
    BrowseTheoryButton.enabled = false
    TheoryLocationField.enabled = false
    TheoryLocationField.text = replacementText
  }

  def enableFileChoosers(filterDescription: String, filter: String): Unit = {
    BrowseTheoryButton.enabled = true
    TheoryLocationField.enabled = true
    TheoryLocationField.text = ""
    fileChoiceFilterDescription = filterDescription
    fileChoiceFilter = filter
  }

  disableFileChoosers("red_green")

  reactions += {
    case ButtonClicked(CreateButton) =>
      val theory = TheoryLocationField.text
      val name = NameField.text
      val path = ProjectLocationField.text
      val folder = new File(path + "/" + name)
      if (name.isEmpty) {
        UserAlerts.errorbox("Please enter a name for your project.")
      } else if (folder.exists()) {
        UserAlerts.errorbox("That folder is already in use.")
      } else if (theory.isEmpty) {
        UserAlerts.errorbox("Please choose a theory.")
      } else {
        result = Some((theory, name, path))
        close()
      }
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
      val qtheoryExtensionFilter = new FileNameExtensionFilter(fileChoiceFilterDescription, fileChoiceFilter)
      chooser.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
      chooser.fileFilter = qtheoryExtensionFilter
      chooser.showOpenDialog(mainPanel) match {
        case FileChooser.Result.Approve =>
          TheoryLocationField.text = chooser.selectedFile.toString
        case _ =>
      }
    case SelectionChanged(TheoryChoiceDropdown) =>
      TheoryChoiceDropdown.selection.item match {
        case "ZX" =>
          disableFileChoosers("red_green")
        case "ZW" =>
          disableFileChoosers("black_white")
        case "From existing project" =>
          enableFileChoosers("Project files", "qproject")
        case "From .qtheory file" =>
          enableFileChoosers("Theory files", "qtheory")
      }
  }
}
