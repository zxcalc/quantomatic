package quanto.gui

import scala.swing._
import java.awt.Dimension
import scala.swing.event.{ButtonClicked, Key}
import javax.swing.KeyStroke
import java.awt.event.KeyEvent
import quanto.util.json.Json
import quanto.data._
import java.io.File
import java.nio.file.{Files, Paths}


object QuantoDerive extends SimpleSwingApplication {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  var CurrentProject : Option[Project] = None

  val ProjectFileTree = new FileTree
  ProjectFileTree.preferredSize = new Dimension(200,800)

  object Split extends SplitPane {
    orientation = Orientation.Vertical
    contents_=(ProjectFileTree, new BorderPanel())
  }

  class NewDialog extends Dialog {
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

  val FileMenu = new Menu("File") { menu =>
    mnemonic = Key.F

    val NewProjectAction = new Action("New Project...") {
      menu.contents += new MenuItem(this) { mnemonic = Key.N }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_N, CommandMask | Key.Modifier.Shift))
      def apply() {
        val d = new NewDialog()
        d.centerOnScreen()
        d.open()
        d.result.map {
          case (thy,name,path) =>
            println("got: " + (thy, name, path))
            val folder = Paths.get(path, name)
            if (new File(folder.toString).exists()) {
              Dialog.showMessage(
                title = "Error",
                message = "A file or folder already exists with that name.",
                messageType = Dialog.Message.Error)
            } else {
              Files.createDirectories(folder)
              Files.createDirectory(folder.resolve("graphs"))
              Files.createDirectory(folder.resolve("axioms"))
              Files.createDirectory(folder.resolve("theorems"))
              Files.createDirectory(folder.resolve("derivations"))
              val proj = Project(theoryFile = thy, rootFolder = folder.toString)
              Project.toJson(proj).writeTo(new File(folder.resolve("main.qproject").toString))
              CurrentProject = Some(proj)
              ProjectFileTree.root = Some(folder.toString)
            }
        }
      }
    }

    val OpenProjectAction = new Action("Open Project...") {
      menu.contents += new MenuItem(this) { mnemonic = Key.O }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_O, CommandMask | Key.Modifier.Shift))
      def apply() {
        val chooser = new FileChooser()
        chooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
        chooser.showOpenDialog(Split) match {
          case FileChooser.Result.Approve =>
            val folder = chooser.selectedFile.toString
            val projectFile = new File(folder + "/main.qproject")
            if (projectFile.exists) {
              try {
                val proj = Project.fromJson(Json.parse(projectFile), folder)
                CurrentProject = Some(proj)
                ProjectFileTree.root = Some(folder)
              } catch {
                case _: ProjectLoadException =>
                  Dialog.showMessage(
                    title = "Error",
                    message = "Error loading project file",
                    messageType = Dialog.Message.Error)
                case e : Exception =>
                  Dialog.showMessage(
                    title = "Error",
                    message = "Unexpected error when opening project",
                    messageType = Dialog.Message.Error)
                  e.printStackTrace()
              }
            } else {
              Dialog.showMessage(
                title = "Error",
                message = "Folder does not contain a QuantoDerive project",
                messageType = Dialog.Message.Error)
            }
          case _ =>
        }
      }
    }

    val QuitAction = new Action("Quit") {
      menu.contents += new MenuItem(this) { mnemonic = Key.Q }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Q, CommandMask))
      def apply() {
        //if (ruleDocument.promptUnsaved())
          sys.exit(0)
      }
    }

  }

  def top = new MainFrame {
    title = "QuantoDerive"
    contents = Split

    size = new Dimension(1280,800)

    menuBar = new MenuBar {
      contents += FileMenu
    }
  }
}
