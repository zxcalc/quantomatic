package quanto.gui

import scala.swing._
import java.awt._
import scala.swing.event.{ButtonClicked, Key}
import javax.swing.{Icon, KeyStroke}
import java.awt.event.KeyEvent
import quanto.util.json.Json
import quanto.data._
import java.io.File
import java.nio.file.{Files, Paths}
import scala.swing.TabbedPane.Page
import javax.swing.border.EmptyBorder
import scala.swing.Color
import scala.swing.Dialog
import scala.swing.Insets
import java.awt.Dimension
import scala.swing.Button
import scala.Some
import scala.swing.Menu
import scala.swing.Label
import scala.swing.Graphics2D
import scala.swing.MenuBar
import scala.swing.Color
import java.awt.Color
import scala.swing.MenuItem


object QuantoDerive extends SimpleSwingApplication {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  var CurrentProject : Option[Project] = None

  val ProjectFileTree = new FileTree
  ProjectFileTree.preferredSize = new Dimension(250,360)



  val MainTabbedPane = new ClosableTabbedPane
  MainTabbedPane += ClosablePage("Test1", new BorderPanel) { true }
  MainTabbedPane += ClosablePage("Test2", new BorderPanel) { true }
  MainTabbedPane += ClosablePage("Test3", new BorderPanel) { true }



  object LeftSplit extends SplitPane {
    orientation = Orientation.Horizontal
    contents_=(ProjectFileTree, new BorderPanel)
  }

  object Split extends SplitPane {
    orientation = Orientation.Vertical
    contents_=(LeftSplit, MainTabbedPane)
  }



  val FileMenu = new Menu("File") { menu =>
    mnemonic = Key.F

    val NewProjectAction = new Action("New Project...") {
      menu.contents += new MenuItem(this) { mnemonic = Key.N }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_N, CommandMask | Key.Modifier.Shift))
      def apply() {
        val d = new NewProjectDialog()
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

    size = new Dimension(1280,720)

    menuBar = new MenuBar {
      contents += FileMenu
    }
  }
}
