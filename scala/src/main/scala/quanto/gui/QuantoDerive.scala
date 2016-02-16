package quanto.gui


import scala.swing._
import scala.swing.event.{SelectionChanged, Key}
import javax.swing.{UIManager, KeyStroke}
import java.awt.event.KeyEvent
import quanto.util.json.{JsonString, Json}
import quanto.data._
import java.io.{FilenameFilter, IOException, File}
import javax.swing.plaf.metal.MetalLookAndFeel
import java.util.prefs.Preferences
import quanto.gui.histview.HistView
import akka.actor.{Props, ActorSystem}
import quanto.core._
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.PoisonPill
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import java.awt.Color
import quanto.util.Globals


object QuantoDerive extends SimpleSwingApplication {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
  val actorSystem = ActorSystem("QuantoDerive")
  val core = actorSystem.actorOf(Props { new Core }, "core")
  implicit val timeout = Timeout(1.day)


  println(new File(".").getAbsolutePath)

  def error(msg: String) = Dialog.showMessage(
    title = "Error", message = msg, messageType = Dialog.Message.Error)

  try {
    UIManager.setLookAndFeel(new MetalLookAndFeel) // tabs in OSX PLAF look bad
  } catch {
    case e: Exception => e.printStackTrace()
  }

  val prefs = Preferences.userRoot().node(this.getClass.getName)

  var CurrentProject : Option[Project] = prefs.get("lastProjectFolder", null) match {
    case path : String =>
      try {
        println("project path: " + path)
        val projectFile = new File(path + "/main.qproject")
        if (projectFile.exists) Some(Project.fromJson(Json.parse(projectFile), path))
        else None
      } catch {
        case e: Exception =>
          e.printStackTrace()
          None
      }
    case _ => None
  }

  CurrentProject.map { pr => core ! SetMLWorkingDir(pr.rootFolder) }

  val ProjectFileTree = new FileTree
  ProjectFileTree.preferredSize = new Dimension(250,360)
  ProjectFileTree.filenameFilter = Some(new FilenameFilter {
    val extns = Set("qgraph", "qrule", "qderive", "ML")
    def accept(parent: File, name: String) = {
      val extn = name.lastIndexOf('.') match {
        case i if i > 0 => name.substring(i+1) ; case _ => ""}
      if (extns.contains(extn)) true
      else {
        val f = new File(parent, name)
        f.isDirectory && !(f.isHidden || f.getName.startsWith(".")) // don't show hidden (dot) directories
      }
    }
  })

  ProjectFileTree.root = CurrentProject.map { _.rootFolder }

  val MainTabbedPane = new ClosableTabbedPane

  def currentDocument: Option[HasDocument] =
    MainTabbedPane.currentContent match {
      case Some(doc: HasDocument) => Some(doc)
      case _ => None
    }

  def currentGraphController: Option[GraphEditController] =
    MainTabbedPane.currentContent match {
      case Some(p: GraphEditPanel) => Some(p.graphEditController)
      case Some(p: RuleEditPanel) => Some(p.focusedController)
      case _ => None
    }

  object HistViewSlot extends BorderPanel {
    def setHistView(hv: HistView[DeriveState]) {
      add(new ScrollPane(hv), BorderPanel.Position.Center)
      revalidate()
      repaint()
    }

    def clearHistView() {
      add(new BorderPanel(), BorderPanel.Position.Center)
      revalidate()
      repaint()
    }
  }

  private var _histView: Option[HistView[DeriveState]] = None
  def histView_=(hvOpt: Option[HistView[DeriveState]]) {
    _histView = hvOpt
    hvOpt match {
      case Some(hv) => HistViewSlot.setHistView(hv)
      case None => HistViewSlot.clearHistView()
    }
  }

  def histView = _histView

  object LeftSplit extends SplitPane {
    orientation = Orientation.Horizontal
    contents_=(ProjectFileTree, HistViewSlot)
  }

  object Split extends SplitPane {
    orientation = Orientation.Vertical
    contents_=(LeftSplit, MainTabbedPane)
  }

  def hasUnsaved =
    MainTabbedPane.pages.exists { p => p.content match {
      case c : HasDocument => c.document.unsavedChanges
      case _ => false
    }}

  /**
   * Try to save all documents in the project by calling their
   * associated trySave() method.
   * @return true if all documents were saved, false otherwise
   * (depends on user choice)
   */
  def trySaveAll() = {
    MainTabbedPane.pages.forall { p =>
      MainTabbedPane.selection.index = p.index // focus a pane before saving
      p.content match {
        case c : HasDocument => c.document.trySave()
        case _ => false
      }
    }
  }

  /**
   * Show a dialog (when necessary) asking the user if the program should quit
   * @return true if the program should quit, false otherwise
   */
  def closeAllDocuments() = {
    if (hasUnsaved) {
      val choice = Dialog.showOptions(
        title = "Confirm quit",
        message = "Some documents have unsaved changes.\nDo you want to save your changes or discard them?",
        entries = "Save" :: "Discard" :: "Cancel" :: Nil,
        initial = 0
      )
      // scala swing dialogs implementation is dumb, here's what I found :
      // Result(0) = Save, Result(1) = Discard, Result(2) = Cancel
      if (choice == Dialog.Result(2)) false
      else if (choice == Dialog.Result(1)) {
        MainTabbedPane.pages.clear()
        true
      }
      else {
        val b = trySaveAll()
        if (b) MainTabbedPane.pages.clear()
        b
      }
    }
    else {
      MainTabbedPane.pages.clear()
      true
    }
  }

  def quitQuanto() = {
    if (closeAllDocuments()) {
      try {
        core ! StopCore
        core ! PoisonPill
      } catch {
        case e : Exception => e.printStackTrace()
      }
      true
    } else {
      false
    }
  }

// TODO: get this code compiling cross-platform
//  if (Globals.isMacBundle) {
//    import com.apple.eawt._
//    Application.getApplication.setQuitHandler(
//      new QuitHandler {
//        def handleQuitRequestWith(e: AppEvent.QuitEvent, response: QuitResponse) {
//          if (quitQuanto()) response.performQuit()
//          else response.cancelQuit()
//        }
//      })
//  }


  object FileMenu extends Menu("File") { menu =>
    mnemonic = Key.F

    val NewGraphAction = new Action("New Graph") {
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

    val NewAxiomAction = new Action("New Axiom") {
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

    val NewMLAction = new Action("New ML Document") {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_M, CommandMask | Key.Modifier.Shift))
      menu.contents += new MenuItem(this) { mnemonic = Key.M }
      def apply() {
        CurrentProject.map{ project =>
          val page = new MLDocumentPage
          MainTabbedPane += page
          MainTabbedPane.selection.index = page.index
        }
      }
    }

    def updateNewEnabled() {
      CurrentProject match {
        case Some(_) =>
          NewGraphAction.enabled = true
          NewAxiomAction.enabled = true
          NewMLAction.enabled = true
        case None =>
          NewGraphAction.enabled = false
          NewAxiomAction.enabled = false
          NewMLAction.enabled = false
      }
    }

    updateNewEnabled()

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

    val SaveAllAction = new Action("Save All") {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_A, CommandMask | Key.Modifier.Shift))
      enabled = false
      menu.contents += new MenuItem(this) { mnemonic = Key.V }
      def apply() {
        val selection = MainTabbedPane.selection.index
        trySaveAll()
        MainTabbedPane.selection.index = selection
      }
    }

    menu.contents += new Separator()

    val NewProjectAction = new Action("New Project...") {
      menu.contents += new MenuItem(this) { mnemonic = Key.N }

      def apply() {
        if (closeAllDocuments()) {
          val d = new NewProjectDialog()
          d.centerOnScreen()
          d.open()
          d.result.map {
            case (thy,name,path) =>
              println("got: " + (thy, name, path))
              val folder = new File(path + "/" + name)
              if (folder.exists()) {
                Dialog.showMessage(
                  title = "Error",
                  message = "A file or folder already exists with that name.",
                  messageType = Dialog.Message.Error)
              } else {
                folder.mkdirs()
                new File(folder.getPath + "/graphs").mkdir()
                new File(folder.getPath + "/axioms").mkdir()
                new File(folder.getPath + "/theorems").mkdir()
                new File(folder.getPath + "/derivations").mkdir()
                new File(folder.getPath + "/simprocs").mkdir()
                val rootFolder = folder.getAbsolutePath
                val proj = Project(theoryFile = thy, rootFolder = rootFolder)
                Project.toJson(proj).writeTo(new File(folder.getPath + "/main.qproject"))
                CurrentProject = Some(proj)
                ProjectFileTree.root = Some(rootFolder)
                prefs.put("lastProjectFolder", rootFolder)
                core ! SetMLWorkingDir(rootFolder)
                updateNewEnabled()
              }
          }
        }
      }
    }

    val OpenProjectAction = new Action("Open Project...") {
      menu.contents += new MenuItem(this) { mnemonic = Key.O }
      def apply() {
        if (closeAllDocuments()) {
          val chooser = new FileChooser()
          chooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
          chooser.showOpenDialog(Split) match {
            case FileChooser.Result.Approve =>
              val rootFolder = chooser.selectedFile.toString
              val projectFile = new File(rootFolder + "/main.qproject")
              if (projectFile.exists) {
                try {
                  val proj = Project.fromJson(Json.parse(projectFile), rootFolder)
                  CurrentProject = Some(proj)
                  ProjectFileTree.root = Some(rootFolder)
                  prefs.put("lastProjectFolder", rootFolder.toString)
                  core ! SetMLWorkingDir(rootFolder)
                } catch {
                  case _: ProjectLoadException =>
                    error("Error loading project file")
                  case e : Exception =>
                    error("Unexpected error when opening project")
                    e.printStackTrace()
                } finally {
                  updateNewEnabled()
                }
              } else {
                error("Folder does not contain a QuantoDerive project")
              }
            case _ =>
          }
        }
      }
    }

    val CloseProjectAction = new Action("Close Project") {
      menu.contents += new MenuItem(this) { mnemonic = Key.C }
      def apply() {
        if (closeAllDocuments()) {
          ProjectFileTree.root = None
          CurrentProject = None
          updateNewEnabled()
        }
      }
    }

    menu.contents += new Separator()

    val QuitAction = new Action("Quit") {
      menu.contents += new MenuItem(this) { mnemonic = Key.Q }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Q, CommandMask))
      def apply() { if (quitQuanto()) scala.sys.exit(0) }
    }
  }

  val EditMenu = new Menu("Edit") { menu =>
    mnemonic = Key.E

    val UndoAction = new Action("Undo") with Reactor {
      menu.contents += new MenuItem(this) { mnemonic = Key.U }

      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, CommandMask))
      enabled = false
      def apply() = currentDocument.map { doc =>
        doc.document.undoStack.undo()
      }

      def updateUndoCommand() =
        currentDocument match {
          case Some(doc) =>
            enabled = doc.document.undoStack.canUndo
            title = "Undo " + doc.document.undoStack.undoActionName.getOrElse("")
          case None =>
            enabled = false
            title = "Undo"
        }

      listenTo(MainTabbedPane.selection)

      reactions += {
        case DocumentChanged(_) => updateUndoCommand()
        case SelectionChanged(_) =>
          currentDocument.map { doc => listenTo(doc.document) }
          updateUndoCommand()
      }
    }

    val RedoAction = new Action("Redo") with Reactor {
      menu.contents += new MenuItem(this) { mnemonic = Key.R }

      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, CommandMask | Key.Modifier.Shift))
      enabled = false

      def apply() = currentDocument.map { doc =>
        doc.document.undoStack.redo()
      }

      def updateRedoCommand() =
        currentDocument match {
          case Some(doc) =>
            enabled = doc.document.undoStack.canRedo
            title = "Redo " + doc.document.undoStack.redoActionName.getOrElse("")
          case None =>
            enabled = false
            title = "Redo"
        }

      listenTo(MainTabbedPane.selection)

      reactions += {
        case DocumentChanged(_) => updateRedoCommand()
        case SelectionChanged(_) =>
          currentDocument.map { doc => listenTo(doc.document) }
          updateRedoCommand()
      }
    }

    contents += new Separator

    val CutAction = new Action("Cut") {
      menu.contents += new MenuItem(this) { mnemonic = Key.U }
      //accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_X, CommandMask))
      def apply() { currentGraphController.map(_.cutSubgraph()) }
    }

    val CopyAction = new Action("Copy") {
      menu.contents += new MenuItem(this) { mnemonic = Key.C }
      //accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_C, CommandMask))
      def apply() { currentGraphController.map(_.copySubgraph()) }
    }

    val PasteAction = new Action("Paste") {
      menu.contents += new MenuItem(this) { mnemonic = Key.P }
      //accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_V, CommandMask))
      def apply() { currentGraphController.map(_.pasteSubgraph()) }
    }

    contents += new Separator

    val SnapToGridAction = new Action("Snap to grid") {
      menu.contents += new MenuItem(this) { mnemonic = Key.S }
      //accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_G, CommandMask))
      def apply() { currentGraphController.map(_.snapToGrid()) }
    }

//    val LayoutAction = new Action("Layout Graph") with Reactor {
//      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_L, CommandMask))
//      def apply() {
//        ruleEditPanel.lhsController.layoutGraph()
//        ruleEditPanel.rhsController.layoutGraph()
//      }
//    }



//    contents += new MenuItem(LayoutAction) { mnemonic = Key.L }
  }

  val DeriveMenu = new Menu("Derive") { menu =>
    val StartDerivation = new Action("Start derivation") {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_D, CommandMask))
      enabled = false
      menu.contents += new MenuItem(this) { mnemonic = Key.D }
      def apply() = (CurrentProject, MainTabbedPane.currentContent) match {
          case (Some(project), Some(doc: HasDocument)) =>
            doc.document match {
              case (graphDoc: GraphDocument) =>
                val page = new DerivationDocumentPage(project)
                MainTabbedPane += page
                MainTabbedPane.selection.index = page.index
                page.document.asInstanceOf[DerivationDocument].root = graphDoc.graph

              case _ =>
                System.err.println("WARNING: Start derivation called with no graph active")
            }
          case _ => // no project and/or document open, do nothing
      }
    }

    val LayoutDerivation = new Action("Layout derivation") {
//      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_L, CommandMask))
      enabled = false
      menu.contents += new MenuItem(this) { mnemonic = Key.L }
      def apply() = (CurrentProject, MainTabbedPane.currentContent) match {
        case (Some(project), Some(derivePanel: DerivationPanel)) =>
          derivePanel.controller.layoutDerivation()
        case _ => // no project and/or derivation open, do nothing
      }
    }
  }

  val WindowMenu = new Menu("Window") { menu =>
    val CloseAction = new Action("Close tab") {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_W, CommandMask))
      enabled = false
      menu.contents += new MenuItem(this) { mnemonic = Key.C }
      def apply() {
        MainTabbedPane.currentContent match {
          case Some(doc: HasDocument) =>
            if (doc.document.promptUnsaved()) MainTabbedPane.pages.remove(MainTabbedPane.selection.index)
          case _ =>
        }
      }
    }
  }

  val ExportMenu = new Menu("Export") { menu =>
    val ExportAction = new Action("Export to LaTeX") {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_E, CommandMask))
      enabled = false
      menu.contents += new MenuItem(this) { mnemonic = Key.E }
      def apply() {
        MainTabbedPane.currentContent match {
          case Some(doc: HasDocument) =>
            if (doc.document.unsavedChanges) {
              Dialog.showMessage(title = "Unsaved Changes",
                message = "You need to save the document before exporting",
                messageType = Dialog.Message.Info
              )
            }
            else doc.document.export()
          case _ =>
        }
      }
    }
  }

  val CoreStatus = new Label("Waiting for connection...")
  CoreStatus.foreground = Color.BLUE
  val ConsoleProgress = new ProgressBar

  val StatusBar = new GridPanel(1,2) {
    contents += new FlowPanel(FlowPanel.Alignment.Left) ( new Label("Core status:"), CoreStatus )
    contents += new FlowPanel(FlowPanel.Alignment.Right) ( ConsoleProgress )
  }

  val Main = new BorderPanel {
    add(Split, BorderPanel.Position.Center)
    add(StatusBar, BorderPanel.Position.South)
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
                case "qgraph"  => Some(new GraphDocumentPage(project.theory))
                case "qrule"   => Some(new RuleDocumentPage(project.theory))
                case "qderive" => Some(new DerivationDocumentPage(project))
                case "ML"      => Some(new MLDocumentPage)
                case _         => None
              }

              pageOpt.map{ page =>
                MainTabbedPane += page
                MainTabbedPane.selection.index = page.index

                if (!page.document.load(file)) {
                  MainTabbedPane.pages -= page
                }
              }
          }
        case None => error("No project open.")
      }

    case SelectionChanged(_) =>
      FileMenu.SaveAction.enabled = false
      FileMenu.SaveAsAction.enabled = false
      FileMenu.SaveAllAction.enabled = false
      EditMenu.CutAction.enabled = false
      EditMenu.CopyAction.enabled = false
      EditMenu.PasteAction.enabled = false
      EditMenu.SnapToGridAction.enabled = false
      DeriveMenu.StartDerivation.enabled = false
      DeriveMenu.LayoutDerivation.enabled = false
      WindowMenu.CloseAction.enabled = false
      ExportMenu.ExportAction.enabled = false

      histView = None
      FileMenu.SaveAction.title = "Save"
      FileMenu.SaveAsAction.title = "Save As..."

      MainTabbedPane.currentContent match {
        case Some(content: HasDocument) =>
          WindowMenu.CloseAction.enabled = true
          FileMenu.SaveAction.enabled = true
          FileMenu.SaveAsAction.enabled = true
          FileMenu.SaveAllAction.enabled = true
          FileMenu.SaveAction.title = "Save " + content.document.description
          FileMenu.SaveAsAction.title = "Save " + content.document.description + " As..."
          ExportMenu.ExportAction.title = "Export " + content.document.description + " to LaTeX"


          content match {
            case panel: GraphEditPanel =>
              EditMenu.CutAction.enabled = true
              EditMenu.CopyAction.enabled = true
              EditMenu.PasteAction.enabled = true
              EditMenu.SnapToGridAction.enabled = true
              DeriveMenu.StartDerivation.enabled = true
              ExportMenu.ExportAction.enabled = true
            case panel: RuleEditPanel =>
              EditMenu.CutAction.enabled = true
              EditMenu.CopyAction.enabled = true
              EditMenu.PasteAction.enabled = true
              EditMenu.SnapToGridAction.enabled = true
              ExportMenu.ExportAction.enabled = true
            case panel: DerivationPanel =>
              DeriveMenu.LayoutDerivation.enabled = true
              ExportMenu.ExportAction.enabled = true
              histView = Some(panel.histView)
            case _ => // nothing else enabled for ML
          }

        case _ => // leave everything disabled
      }
  }

  val versionResp = core ? Call("!!", "system", "version")
  versionResp.onSuccess { case Success(JsonString(version)) =>
    Swing.onEDT { CoreStatus.text = "OK"; CoreStatus.foreground = new Color(0,150,0) }
  }

  def top = new MainFrame {
    title = "QuantoDerive"
    contents = Main

    size = new Dimension(1280,720)

    menuBar = new MenuBar {
      contents += (FileMenu, EditMenu, DeriveMenu, WindowMenu, ExportMenu)
    }

    import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
    peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)

    override def closeOperation() {
      if (quitQuanto()) scala.sys.exit(0)
    }
  }
}
