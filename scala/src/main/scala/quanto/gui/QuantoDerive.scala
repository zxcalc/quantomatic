package quanto.gui


import scala.swing._
import scala.swing.event.{SelectionChanged, Key}
import javax.swing.{UIManager, KeyStroke}
import java.awt.event.KeyEvent
import quanto.util.json.Json
import quanto.data._
import java.io.{FilenameFilter, IOException, File}
import java.nio.file.{Files, Paths}
import javax.swing.plaf.metal.MetalLookAndFeel


object QuantoDerive extends SimpleSwingApplication {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  def error(msg: String) = Dialog.showMessage(
    title = "Error", message = msg, messageType = Dialog.Message.Error)

  try {
    UIManager.setLookAndFeel(new MetalLookAndFeel) // tabs in OSX PLAF look bad
  } catch {
    case e: Exception => e.printStackTrace()
  }

  var CurrentProject : Option[Project] = None

  val ProjectFileTree = new FileTree
  ProjectFileTree.preferredSize = new Dimension(250,360)
  ProjectFileTree.filenameFilter = Some(new FilenameFilter {
    val extns = Set("qgraph", "qrule", "qderive")
    def accept(parent: File, name: String) = {
      val extn = name.lastIndexOf('.') match {
        case i if i > 0 => name.substring(i+1) ; case _ => ""}
      if (extns.contains(extn)) true
      else new File(parent, name).isDirectory
    }
  })

  val MainTabbedPane = new ClosableTabbedPane

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

    val NewGraphAction = new Action("New Graph..") {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_N, CommandMask))
      menu.contents += new MenuItem(this) { mnemonic = Key.G }
      def apply() {
        CurrentProject.map{ project =>
          val page = new GraphDocumentPage(project.theory)
          MainTabbedPane += page
          MainTabbedPane.selection.index = page.index
        }
      }
    }

    val NewAxiomAction = new Action("New Axiom...") {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_N, CommandMask | Key.Modifier.Shift))
      menu.contents += new MenuItem(this) { mnemonic = Key.X }
      def apply() {
        CurrentProject.map{ project =>
          val page = new RuleDocumentPage(project.theory)
          MainTabbedPane += page
          MainTabbedPane.selection.index = page.index
        }
      }
    }

    val SaveAction = new Action("Save") {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_S, CommandMask))
      enabled = false
      menu.contents += new MenuItem(this) { mnemonic = Key.S }
      def apply() {
        MainTabbedPane.currentContent match {
          case Some(doc: HasDocument) =>
            doc.document.file match {
              case Some(_) => doc.document.save()
              case None    => doc.document.showSaveAsDialog(CurrentProject.map(_.rootFolder))
            }
          case _ =>
        }
      }
    }

    val SaveAsAction = new Action("Save As...") {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_S, CommandMask | Key.Modifier.Shift))
      enabled = false
      menu.contents += new MenuItem(this) { mnemonic = Key.A }
      def apply() {
        MainTabbedPane.currentContent match {
          case Some(doc: HasDocument) =>
            doc.document.showSaveAsDialog(CurrentProject.map(_.rootFolder))
          case _ =>
        }
      }
    }

    menu.contents += new Separator()

    val NewProjectAction = new Action("New Project...") {
      menu.contents += new MenuItem(this) { mnemonic = Key.N }

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
                  error("Error loading project file")
                case e : Exception =>
                  error("Unexpected error when opening project")
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

    menu.contents += new Separator()

    val QuitAction = new Action("Quit") {
      menu.contents += new MenuItem(this) { mnemonic = Key.Q }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Q, CommandMask))
      def apply() {
        //if (ruleDocument.promptUnsaved())
          sys.exit(0)
      }
    }
  }

  listenTo(ProjectFileTree, MainTabbedPane.selection)

  reactions += {
    case FileOpened(file) =>
      CurrentProject match {
        case Some(project) =>
          val existingPage = MainTabbedPane.pages.find { p =>
            p.content match {
              case doc : HasDocument => doc.document.file.exists(_.getPath == file.getPath)
              case _ => false
            }
          }

          existingPage match {
            case Some(p) =>
              MainTabbedPane.selection.index = p.index
            case None =>
              val extn = file.getName.lastIndexOf('.') match {
                case i if i > 0 => file.getName.substring(i+1) ; case _ => ""}

              val pageOpt = extn match {
                case "qgraph" => Some(new GraphDocumentPage(project.theory))
                case "qrule"  => Some(new RuleDocumentPage(project.theory))
                case _ => None
              }

              pageOpt.map{ page =>
                if (page.document.load(file)) {
                  MainTabbedPane += page
                  MainTabbedPane.selection.index = page.index
                }
              }
          }
        case None => error("No project open.")
      }

    case SelectionChanged(_) =>
      MainTabbedPane.currentContent match {
        case Some(doc: HasDocument) =>
          FileMenu.SaveAction.enabled = true
          FileMenu.SaveAction.title = "Save " + doc.document.description
          FileMenu.SaveAsAction.enabled = true
          FileMenu.SaveAsAction.title = "Save " + doc.document.description + " As..."
        case _ =>
          FileMenu.SaveAction.enabled = false
          FileMenu.SaveAction.title = "Save"
          FileMenu.SaveAsAction.enabled = false
          FileMenu.SaveAsAction.title = "Save As..."

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
