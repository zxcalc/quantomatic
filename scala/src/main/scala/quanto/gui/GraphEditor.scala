package quanto.gui


import swing._
import event.Key
import quanto.data._
import javax.swing.{JToolBar, KeyStroke}
import java.awt.event.KeyEvent
import quanto.util.json.Json
import quanto.layout.ForceLayout


object GraphEditor extends SimpleSwingApplication {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  println("loading theory " + GraphEditor.getClass.getResource("strategy_graph.qtheory"))
  val thyFile = new Json.Input(GraphEditor.getClass.getResourceAsStream("strategy_graph.qtheory"))
  val StringVETheory = Theory.fromJson(Json.parse(thyFile))

  val graphEditPanel = new GraphEditPanel(StringVETheory, readOnly = false)
  val graphDocument = graphEditPanel.graphDocument

  // Main menu

  val FileMenu = new Menu("File") { menu =>
    mnemonic = Key.F

    val NewAction = new Action("New") {
      menu.contents += new MenuItem(this) { mnemonic = Key.N }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_N, CommandMask))
      def apply() {
        if (graphDocument.promptUnsaved()) graphDocument.newGraph()
      }
    }

    val OpenAction = new Action("Open...") {
      menu.contents += new MenuItem(this) { mnemonic = Key.O }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_O, CommandMask))
      def apply() { graphDocument.showOpenDialog() }

      val SaveAction = new Action("Save") {
        menu.contents += new MenuItem(this) { mnemonic = Key.S }
        accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_S, CommandMask))
        enabled = false
        def apply() {
          graphDocument.file match {
            case Some(_) => graphDocument.saveGraph()
            case None    => graphDocument.showSaveAsDialog()
          }
        }

        listenTo(graphDocument)
        reactions += { case GraphChanged(_) | GraphSaved(_) =>
          enabled = graphDocument.unsavedChanges
        }
      }

      val SaveAsAction = new Action("Save As...") {
        menu.contents += new MenuItem(this) { mnemonic = Key.A }
        accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_S, CommandMask | Key.Modifier.Shift))
        def apply() { graphDocument.showSaveAsDialog() }
      }
    }

    val QuitAction = new Action("Quit") {
      menu.contents += new MenuItem(this) { mnemonic = Key.Q }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Q, CommandMask))
      def apply() {
        if (graphDocument.promptUnsaved()) sys.exit(0)
      }
    }
  }

  val EditMenu = new Menu("Edit") {
    mnemonic = Key.E

    val UndoAction = new Action("Undo") with Reactor {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, CommandMask))
      enabled = false
      def apply() { graphDocument.undoStack.undo() }

      listenTo(graphDocument.undoStack)
      reactions += { case _: UndoEvent =>
        enabled = graphDocument.undoStack.canUndo
        title   = "Undo " + graphDocument.undoStack.undoActionName.getOrElse("")
      }
    }

    val RedoAction = new Action("Redo") with Reactor {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, CommandMask | Key.Modifier.Shift))
      enabled = false
      def apply() { graphDocument.undoStack.redo() }

      listenTo(graphDocument.undoStack)
      reactions += { case _: UndoEvent =>
        enabled = graphDocument.undoStack.canRedo
        title = "Redo " + graphDocument.undoStack.redoActionName.getOrElse("")
      }
    }

    contents += new MenuItem(UndoAction) { mnemonic = Key.U }
    contents += new MenuItem(RedoAction) { mnemonic = Key.R }
  }

  def top = new MainFrame {
    title = "PSGraph Editor - " + graphDocument.titleDescription
    contents = graphEditPanel

    size = new Dimension(800,800)

    menuBar = new MenuBar {
      contents += (FileMenu, EditMenu)
    }

    listenTo(graphDocument)
    reactions += {
      case GraphChanged(_)|GraphSaved(_) =>
        title = "PSGraph Editor - " + graphDocument.titleDescription
    }
  }
}