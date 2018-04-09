package quanto.gui


import org.python.util.PythonInterpreter

import scala.io.Source
import scala.swing._
import scala.swing.event.{Key, SelectionChanged}
import javax.swing.{JOptionPane, KeyStroke, SwingUtilities, UIManager}
import java.awt.event.KeyEvent
import java.awt.Frame
import java.awt.event.{KeyEvent, MouseAdapter, MouseEvent}

import quanto.util.json.{Json, JsonString}
import quanto.data._
import java.io.{File, FilenameFilter, IOException, PrintWriter}
import javax.swing.plaf.metal.MetalLookAndFeel
import java.util.prefs.Preferences

import quanto.gui.histview.HistView
import akka.actor.{ActorSystem, Props}
import quanto.core._
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.PoisonPill

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import java.awt.{Color, Desktop, Window}
import javax.swing.filechooser.FileNameExtensionFilter

import quanto.util.{Globals, UserAlerts, UserOptions, WebHelper}


object QuantoDerive extends SimpleSwingApplication {
  System.setProperty("apple.eawt.quitStrategy", "CLOSE_ALL_WINDOWS")
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
  val actorSystem = ActorSystem("QuantoDerive")
  //val core = actorSystem.actorOf(Props { new Core }, "core")
  implicit val timeout = Timeout(1.day)

  // pre-initialise jython, so its zippy when the user clicks "run" in a script
  new Thread(new Runnable { def run() { new PythonInterpreter() }}).start()

  UserAlerts.alert("Working directory: " + new File(".").getAbsolutePath)

  // Dialogs in in scala.swing seem to be broken since updated scala to 2.12, so
  // we're using the javax.swing versions instead
  def error(msg: String) =
    UserAlerts.errorbox(msg)

  def uiScale(i : Int) : Int = UserOptions.scaleInt(i)

  //Dialog.showMessage(title = "Error", message = msg, messageType = Dialog.Message.Error)

  val prefs = Preferences.userRoot().node(this.getClass.getName)

  try {
    UIManager.setLookAndFeel(new MetalLookAndFeel) // tabs in OSX PLAF look bad
    UserOptions.uiScale = prefs.getDouble("uiScale", 1.0)
  } catch {
    case e: Exception => {
      UserAlerts.alert("Could no load UI preferences on startup.")
      e.printStackTrace()
    }
  }

  def unloadProject() {
    CurrentProject = None
    ProjectFileTree.root = None
  }

  def updateProjectFile(projectFile: File): Unit = {
    if (CurrentProject.nonEmpty) {
      val project = CurrentProject.get
      try {
        if (projectFile.exists) {
          val parsedInput = Json.parse(projectFile)
          if (Project.toJson(project).toString != parsedInput.toString) {
            Project.toJson(project).writeTo(project.projectFile)
            UserAlerts.alert(s"Updated project file", UserAlerts.Elevation.DEBUG)
          }
        }
      } catch {
        case e: Exception =>
          throw new ProjectLoadException("Error loading project", e)
      }
    }
  }

  def loadProject(projectFileLocation: String) : Option[Project] = {
    UserAlerts.alert(s"Opening project: $projectFileLocation")

    val projectFile = if(new File(projectFileLocation).isDirectory){
      new File(projectFileLocation + "/main.qproject")
    } else {
      new File(projectFileLocation)
    }
    try {
      if (projectFile.exists) {
        val parsedInput = Json.parse(projectFile)
        val project = Project.fromJson(parsedInput, new File(projectFileLocation))
        // Old .qproject files had links rather than embedded theories
        // So update when loading in
        CurrentProject = Some(project)
        updateProjectFile(projectFile)
        ProjectFileTree.root = Some(project.rootFolder)
        prefs.put("lastProjectFile", projectFileLocation)
        UserAlerts.alert(s"Successfully loaded project: $projectFileLocation")
        Some(project)
      } else {
        UserAlerts.alert("Selected project file does not exist", UserAlerts.Elevation.ERROR)
        unloadProject()
        None
      }
    } catch {
      case e: Exception =>
        throw new ProjectLoadException("Error loading project",e)
        unloadProject()
        None
    } finally {
      FileMenu.updateNewEnabled()
    }
  }

  //CurrentProject.map { pr => core ! SetMLWorkingDir(pr.rootFolder) }

  val ProjectFileTree = new FileTree
  ProjectFileTree.preferredSize = new Dimension(uiScale(250), uiScale(360))
  ProjectFileTree.filenameFilter = Some(new FilenameFilter {
    val extns = Set("qgraph", "qrule", "qderive", "ML", "py", "qsbr")
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


  var CurrentProject : Option[Project] = prefs.get("lastProjectFile", null) match {
    case path : String =>
      try {
        loadProject(path)
      } catch {
        case e: Exception =>
          e.printStackTrace()
          None
      }
    case _ => None
  }

  // Access via even is preferred, as then we can pinpoint where to put the popup
  def popup(menu: PopupMenu, e: Option[MouseEvent]) : Unit = {
    if (e.nonEmpty){
      val componentBounds = e.get.getComponent.getBounds
      val shift : Int = UserOptions.scaleInt(5)
      popup(menu, e.get.getX + componentBounds.x + shift, e.get.getY + componentBounds.y+ shift)
    } else {
      popup(menu, 0, 0)
    }
  }

  def popup(menu: PopupMenu, x: Int, y: Int) : Unit = {
    menu.show(Main, x, y)
  }

  def addAndFocusPage(d : DocumentPage): Unit = {
    MainDocumentTabs += d
    listenTo(d.tabComponent)
  }

  listenTo(quanto.util.UserOptions.OptionsChanged)
  reactions += {
    case quanto.util.UserOptions.UIRedrawRequest() =>
      requestUIRefresh()
  }
  def requestUIRefresh(): Unit = {
    for (w <-  Window.getWindows) {
      SwingUtilities.updateComponentTreeUI(w)
    }
  }

  val MainDocumentTabs = new DocumentTabs

  def currentDocument: Option[DocumentPage] = MainDocumentTabs.currentFocus

  def currentGraphController: Option[GraphEditController] =
    MainDocumentTabs.currentContent match {
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
    contents_=(LeftSplit, MainDocumentTabs.component)
  }

  def hasUnsaved =
    MainDocumentTabs.documents.exists { p => p.content match {
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
    MainDocumentTabs.documents.forall { p =>
      MainDocumentTabs.focus(p) // focus a pane before saving
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
//      val choice = Dialog.showOptions(
//        title = "Confirm quit",
//        message = "Some documents have unsaved changes.\nDo you want to save your changes or discard them?",
//        entries = "Save" :: "Discard" :: "Cancel" :: Nil,
//        initial = 0
//      )

      val choice = JOptionPane.showOptionDialog(null,
        "Do you want to save your changes or discard them?",
        "Unsaved changes",
        JOptionPane.DEFAULT_OPTION,
        JOptionPane.WARNING_MESSAGE, null,
        List("Save", "Discard", "Cancel").toArray,
        "Save")

      // scala swing dialogs implementation is dumb, here's what I found :
      // Result(0) = Save, Result(1) = Discard, Result(2) = Cancel
      if (choice == 2) false
      else if (choice == 1) {
        MainDocumentTabs.clear()
        true
      }
      else {
        val b = trySaveAll()
        if (b) MainDocumentTabs.clear()
        b
      }
    }
    else {
      MainDocumentTabs.clear()
      true
    }
  }

  def quitQuanto(): Boolean = {
    val close = closeAllDocuments()
    if (close) {
      try {
        //core ! StopCore
        //core ! PoisonPill
      } catch {
        case e : Exception => e.printStackTrace()
      }
      val rect = _mainframe.peer.getBounds()
      val isFullScreen : Boolean = _mainframe.peer.getExtendedState() == Frame.MAXIMIZED_BOTH
      prefs.putBoolean("fullscreen", isFullScreen)
      if (!isFullScreen) {
        prefs.putInt("locationx",rect.x)
        prefs.putInt("locationy",rect.y)
        prefs.putInt("screenwidth",rect.width)
        prefs.putInt("screenheight",rect.height)
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


  def FolderContextMenu(folder: File) : PopupMenu = new PopupMenu { //Context menu for project folders
    menu =>

    val OpenLocationAction: Action = new Action("Open Folder") {
      menu.contents += new MenuItem(this) {
        mnemonic = Key.L
      }

      def apply() {
        Desktop.getDesktop.browse(folder.toURI)
      }
    }

  }

  def FileContextMenu(file: File): PopupMenu = new PopupMenu { //Context menu for project files
    menu =>

    val OpenLocationAction: Action = new Action("Open File Location") {
      menu.contents += new MenuItem(this) {
        mnemonic = Key.L
      }

      def apply() {
        Desktop.getDesktop.browse(file.getParentFile.toURI)
      }
    }


    val DeleteFile: Action = new Action("Delete File") {
      menu.contents += new MenuItem(this) {
        mnemonic = Key.D
      }

      def apply() {
        file.delete()
      }
    }
  }


  object FileMenu extends Menu("File") { menu =>
    mnemonic = Key.F

    val NewGraphAction = new Action("New Graph") {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_N, CommandMask))
      menu.contents += new MenuItem(this) { mnemonic = Key.G }
      def apply() {
        CurrentProject.foreach{ project =>
          val page = new GraphDocumentPage(project.theory)
          addAndFocusPage(page)
        }
      }
    }

    val NewAxiomAction = new Action("New Axiom") {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_N, CommandMask | Key.Modifier.Shift))
      menu.contents += new MenuItem(this) { mnemonic = Key.X }
      def apply() {
        CurrentProject.foreach{ project =>
          val page = new RuleDocumentPage(project.theory)
          addAndFocusPage(page)
        }
      }
    }

//    val NewMLAction = new Action("New ML Document") {
//      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_M, CommandMask | Key.Modifier.Shift))
//      menu.contents += new MenuItem(this) { mnemonic = Key.M }
//      def apply() {
//        CurrentProject.map{ project =>
//          val page = new MLDocumentPage
//          MainTabbedPane += page
//          MainTabbedPane.selection.index = page.index
//        }
//      }
//    }

    val NewPythonAction = new Action("New Python Script") {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Y, CommandMask | Key.Modifier.Shift))
      menu.contents += new MenuItem(this) { mnemonic = Key.Y }
      def apply() {
        CurrentProject.foreach{ project =>
          val page = new PythonDocumentPage
          addAndFocusPage(page)
        }
      }
    }

    def updateNewEnabled() {
      CurrentProject match {
        case Some(_) =>
          NewGraphAction.enabled = true
          NewAxiomAction.enabled = true
          //NewMLAction.enabled = true
        case None =>
          NewGraphAction.enabled = false
          NewAxiomAction.enabled = false
          //NewMLAction.enabled = false
      }
    }

    updateNewEnabled()

    val SaveAction = new Action("Save") {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_S, CommandMask))
      enabled = false
      menu.contents += new MenuItem(this) { mnemonic = Key.S }
      def apply() {
        MainDocumentTabs.currentContent match {
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
        MainDocumentTabs.currentContent match {
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
        val selection = MainDocumentTabs.selection.index
        trySaveAll()
        MainDocumentTabs.selection.index = selection
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
          d.result match {
            case Some((theoryFile, name, path)) =>
              println("got: " + (theoryFile, name, path))
              val folder = new File(path + "/" + name)
              if (name.isEmpty) {
                UserAlerts.errorbox("Please enter a name for your project.")
              } else if (folder.exists()) {
                UserAlerts.errorbox("That folder is already in use.")
              } else if (theoryFile.isEmpty) {
                UserAlerts.errorbox("Please enter a theory file.")
              } else {
                folder.mkdirs()
                new File(folder.getPath + "/graphs").mkdir()
                new File(folder.getPath + "/axioms").mkdir()
                new File(folder.getPath + "/theorems").mkdir()
                new File(folder.getPath + "/derivations").mkdir()
                new File(folder.getPath + "/simprocs").mkdir()
                val projectFile = new File(folder.getPath + "/" + name + ".qproject")
                val rootFolder = folder.getAbsolutePath
                val proj = Project.fromTheoryOrProjectFile(new File(theoryFile), new File(rootFolder), name)
                Project.toJson(proj).writeTo(projectFile)
                loadProject(projectFile.getAbsolutePath)
                //core ! SetMLWorkingDir(rootFolder)
                updateNewEnabled()
              }
            case None =>
          }
        }
      }
    }

    val OpenProjectAction = new Action("Open Project...") {
      menu.contents += new MenuItem(this) { mnemonic = Key.O }
      def apply() {
        if (closeAllDocuments()) {
          val chooser = new FileChooser()
          chooser.fileFilter = new FileNameExtensionFilter("Quantomatic Project File (*.qproject)", "qproject")
          chooser.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
          chooser.showOpenDialog(Split) match {
            case FileChooser.Result.Approve =>
              val projectFile = new File(chooser.selectedFile.toString)
              if (projectFile.exists) {
                try {
                  loadProject(chooser.selectedFile.toString)
                  //core ! SetMLWorkingDir(rootFolder)
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
                error(s"Folder does not contain a Quantomatic project: $projectFile")
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

    val EditTheoryAction = new Action("Edit Theory") {
      menu.contents += new MenuItem(this) {
        mnemonic = Key.T
      }

      def apply() {
        CurrentProject.foreach { project =>
          val page = MainDocumentTabs.documents.find(tp => tp.title == "Theory Editor") match {
            case Some(p) => p
            case None =>
              val p = new TheoryPage()
              listenTo(p.document)
              p.title = "Theory Editor"
              addAndFocusPage(p)
              p
          }
          MainDocumentTabs.focus(page)
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
      def apply() = currentDocument.foreach { doc =>
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

      listenTo(MainDocumentTabs.selection)

      reactions += {
        case DocumentChanged(_) => updateUndoCommand()
        case SelectionChanged(_) =>
          currentDocument.foreach { doc => listenTo(doc.document) }
          updateUndoCommand()
      }
    }

    val RedoAction = new Action("Redo") with Reactor {
      menu.contents += new MenuItem(this) { mnemonic = Key.R }

      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, CommandMask | Key.Modifier.Shift))
      enabled = false

      def apply() = currentDocument.foreach { doc =>
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

      listenTo(MainDocumentTabs.selection)

      reactions += {
        case DocumentChanged(_) => updateRedoCommand()
        case SelectionChanged(_) =>
          currentDocument.foreach { doc => listenTo(doc.document) }
          updateRedoCommand()
      }
    }

    contents += new Separator

    val CutAction = new Action("Cut") {
      menu.contents += new MenuItem(this) { mnemonic = Key.U }
      //accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_X, CommandMask))
      def apply() { currentGraphController.foreach(_.cutSubgraph()) }
    }

    val CopyAction = new Action("Copy") {
      menu.contents += new MenuItem(this) { mnemonic = Key.C }
      //accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_C, CommandMask))
      def apply() { currentGraphController.foreach(_.copySubgraph()) }
    }

    val PasteAction = new Action("Paste") {
      menu.contents += new MenuItem(this) { mnemonic = Key.P }
      //accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_V, CommandMask))
      def apply() { currentGraphController.foreach(_.pasteSubgraph()) }
    }

    contents += new Separator

    val SnapToGridAction = new Action("Snap to grid") {
      menu.contents += new MenuItem(this) { mnemonic = Key.S }
      //accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_G, CommandMask))
      def apply() { currentGraphController.foreach(_.snapToGrid()) }
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

  val DeriveMenu = new Menu("Derive") {
    menu =>
    val StartDerivation = new Action("Start derivation") {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_D, CommandMask))
      enabled = false
      menu.contents += new MenuItem(this) {
        mnemonic = Key.D
      }

      def apply() = (CurrentProject, MainDocumentTabs.currentContent) match {
        case (Some(project), Some(doc: HasDocument)) =>
          doc.document match {
            case (graphDoc: GraphDocument) =>
              val page = new DerivationDocumentPage(project)
              addAndFocusPage(page)
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
      def apply() = (CurrentProject, MainDocumentTabs.currentContent) match {
        case (Some(project), Some(derivePanel: DerivationPanel)) =>
          derivePanel.controller.layoutDerivation()
        case _ => // no project and/or derivation open, do nothing
      }
    }


    val BatchDerivationAction = new Action("Batch Derivation") {
      menu.contents += new MenuItem(this) {
        mnemonic = Key.B
      }

      def apply() {
        CurrentProject.foreach { project =>
          val page = MainDocumentTabs.documents.find(tp => tp.title == "Batch Derivation") match {
            case Some(p) => p
            case None =>
              val p = new BatchDerivationPage()
              listenTo(p.document)
              p.title = "Batch Derivation"
              addAndFocusPage(p)
              p
          }
          MainDocumentTabs.focus(page)
        }
      }
    }
  }

  val WindowMenu = new Menu("Window") { menu =>
    val CloseAction = new Action("Close tab") {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_W, CommandMask))
      enabled = false
      menu.contents += new MenuItem(this) { mnemonic = Key.C }
      def apply() {
        MainDocumentTabs.currentFocus match {
          case Some(page: DocumentPage) =>
            if (page.document.promptUnsaved()) MainDocumentTabs.remove(MainDocumentTabs.currentFocus.get)
          case _ =>
        }
      }
    }

    contents += new Separator

    val IncreaseUIScaling = new Action("Increase UI scaling") {
      menu.contents += new MenuItem(this) { mnemonic = Key.I}
      def apply(){UserOptions.uiScale *= 1.2}
    }
    val DecreaseUIScaling = new Action("Decrease UI scaling") {
      menu.contents += new MenuItem(this) { mnemonic = Key.D}
      def apply(){UserOptions.uiScale *= 0.8}
    }
    val ResetUIScaling = new Action("Reset UI scaling") {
      menu.contents += new MenuItem(this) { mnemonic = Key.R}
      def apply(){UserOptions.uiScale = 1}
    }

  }

  val HelpMenu = new Menu("Help") { menu =>
    val WebsiteAction = new Action("Quantomatic website") {
      menu.contents += new MenuItem(this) { mnemonic = Key.Q }
      def apply() {
        WebHelper.openWebpage("https://quantomatic.github.io/")
      }
    }

    val SimprocAPIAction = new Action("Simproc API") {
      menu.contents += new MenuItem(this) { mnemonic = Key.S }
      def apply() {
        WebHelper.openWebpage("https://quantomatic.github.io/SimprocAPI.html")
      }
    }

    //private val project = getClass.getPackage
    // val version = project.getImplementationVersion()
    // TODO: Implement versioning
    val UpdateAction = new Action(s"Get latest version") {
      menu.contents += new MenuItem(this) { mnemonic = Key.V }
      def apply() {
        WebHelper.openWebpage("https://bintray.com/quantomatic/quantomatic/quantomatic/bleeding-edge")
      }
    }
  }

  val ExportMenu = new Menu("Export") { menu =>
    val ExportAction = new Action("Export to LaTeX") {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_E, CommandMask))
      enabled = false
      menu.contents += new MenuItem(this) { mnemonic = Key.E }
      def apply() {
        MainDocumentTabs.currentContent match {
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

  val StatusBar = new StatusBar()



  listenTo(ProjectFileTree, MainDocumentTabs.selection)

  reactions += {
    case PageClosed(p : DocumentPage) =>
      MainDocumentTabs.remove(p)
    case FileContextRequested(file, e) =>
      if(file.isDirectory){
        popup(FolderContextMenu(file), e)
      } else {
        popup(FileContextMenu(file), e)
      }
    case FileOpened(file) =>
      CurrentProject match {
        case Some(project) =>
          val existingPage = MainDocumentTabs.documents.find { p =>
            p.content match {
              case doc : HasDocument => doc.document.file.exists(_.getPath == file.getPath)
              case _ => false
            }
          }

          existingPage match {
            case Some(p) =>
              MainDocumentTabs.focus(p)
            case None =>
              val extn = file.getName.lastIndexOf('.') match {
                case i if i > 0 => file.getName.substring(i+1) ; case _ => ""}

              val pageOpt = extn match {
                case "qgraph"  => Some(new GraphDocumentPage(project.theory))
                case "qrule"   => Some(new RuleDocumentPage(project.theory))
                case "qderive" => Some(new DerivationDocumentPage(project))
                case "py"      => Some(new PythonDocumentPage)
                case "ML"      => Some(new MLDocumentPage)
                case "qsbr"      => Some(new BatchDerivationResultsPage)
                case _         => None
              }

              pageOpt.foreach{ page =>
                addAndFocusPage(page)

                if (!page.document.load(file)) {
                  MainDocumentTabs.remove(page)
                }
              }
          }
        case None => error("No project open.")
      }

    case SelectionChanged(_) =>
      FileMenu.SaveAction.enabled = false
      FileMenu.SaveAsAction.enabled = false
      FileMenu.SaveAllAction.enabled = false
      FileMenu.EditTheoryAction.enabled = CurrentProject.nonEmpty
      EditMenu.CutAction.enabled = false
      EditMenu.CopyAction.enabled = false
      EditMenu.PasteAction.enabled = false
      EditMenu.SnapToGridAction.enabled = false
      DeriveMenu.StartDerivation.enabled = false
      DeriveMenu.LayoutDerivation.enabled = false
      DeriveMenu.BatchDerivationAction.enabled = CurrentProject.nonEmpty
      WindowMenu.CloseAction.enabled = false
      ExportMenu.ExportAction.enabled = false

      histView = None
      FileMenu.SaveAction.title = "Save"
      FileMenu.SaveAsAction.title = "Save As..."

      MainDocumentTabs.currentContent match {
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

//  val versionResp = core ? Call("!!", "system", "version")
//  versionResp.onSuccess { case Success(JsonString(version)) =>
//    Swing.onEDT { CoreStatus.text = "OK"; CoreStatus.foreground = new Color(0,150,0) }
//  }

  // The highest level GUI contents
  val Main = new BorderPanel {
    add(Split, BorderPanel.Position.Center)
    add(StatusBar.Status, BorderPanel.Position.South)
  }


  val _mainframe = new MainFrame {
    override def title : String = {
      if (CurrentProject.isEmpty) {"Quantomatic"} else {
        CurrentProject.get.name match {
          case "" => "Quantomatic"
          case s => "Quantomatic - $s"
        }
      }
    }
    contents = Main

    if (prefs.getBoolean("fullscreen",false)) {
      peer.setExtendedState(peer.getExtendedState() | Frame.MAXIMIZED_BOTH)
    }
    else {
      size = new Dimension(prefs.getInt("screenwidth",1280),prefs.getInt("screenheight",720))
      peer.setLocation(prefs.getInt("locationx",300),prefs.getInt("locationy",300))
    }
    peer.setVisible(true)


    menuBar = new MenuBar {
      contents += (FileMenu, EditMenu, DeriveMenu, WindowMenu, ExportMenu, HelpMenu)
    }

    import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
    peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)

    override def closeOperation() {
      if (quitQuanto()) scala.sys.exit(0)
    }
  }

  def top = _mainframe
}
