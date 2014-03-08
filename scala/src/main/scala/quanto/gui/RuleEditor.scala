package quanto.gui

import quanto.util.json.Json
import quanto.data.Theory
import scala.swing._
import scala.swing.event.Key
import javax.swing.KeyStroke
import java.awt.event.KeyEvent
import java.awt.Dimension


object RuleEditor extends SimpleSwingApplication {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  // println("loading theory " + GraphEditor.getClass.getResource("strategy_graph.qtheory"))
  // val thyFile = new Json.Input(GraphEditor.getClass.getResourceAsStream("strategy_graph.qtheory"))
  // val StringVETheory = Theory.fromJson(Json.parse(thyFile))
  //val StringVETheory = Theory.DefaultTheory
  println("loading theory " + Theory.getClass.getResource("red_green.qtheory"))
  val thyFile = new Json.Input(Theory.getClass.getResourceAsStream("red_green.qtheory"))
  val thy = Theory.fromJson(Json.parse(thyFile))

  val ruleEditPanel = new RuleEditPanel(thy, readOnly = false)
  val ruleDocument = ruleEditPanel.document

  // Main menu

  val FileMenu = new Menu("File") { menu =>
    mnemonic = Key.F

    val NewAction = new Action("New") {
      menu.contents += new MenuItem(this) { mnemonic = Key.N }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_N, CommandMask))
      def apply() {
        if (ruleDocument.promptUnsaved()) ruleDocument.clear()
      }
    }

    val OpenAction = new Action("Open...") {
      menu.contents += new MenuItem(this) { mnemonic = Key.O }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_O, CommandMask))
      def apply() { ruleDocument.showOpenDialog() }

      val SaveAction = new Action("Save") {
        menu.contents += new MenuItem(this) { mnemonic = Key.S }
        accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_S, CommandMask))
        enabled = false
        def apply() {
          ruleDocument.file match {
            case Some(_) => ruleDocument.save()
            case None    => ruleDocument.showSaveAsDialog()
          }
        }

        listenTo(ruleDocument)
        reactions += { case DocumentChanged(_) | DocumentSaved(_) =>
          enabled = ruleDocument.unsavedChanges
        }
      }

      val SaveAsAction = new Action("Save As...") {
        menu.contents += new MenuItem(this) { mnemonic = Key.A }
        accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_S, CommandMask | Key.Modifier.Shift))
        def apply() { ruleDocument.showSaveAsDialog() }
      }
    }

    val QuitAction = new Action("Quit") {
      menu.contents += new MenuItem(this) { mnemonic = Key.Q }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Q, CommandMask))
      def apply() {
        if (ruleDocument.promptUnsaved()) sys.exit(0)
      }
    }
  }

  val EditMenu = new Menu("Edit") {
    mnemonic = Key.E

    val UndoAction = new Action("Undo") with Reactor {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, CommandMask))
      enabled = false
      def apply() { ruleDocument.undoStack.undo() }

      listenTo(ruleDocument.undoStack)
      reactions += { case _: UndoEvent =>
        enabled = ruleDocument.undoStack.canUndo
        title   = "Undo " + ruleDocument.undoStack.undoActionName.getOrElse("")
      }
    }

    val RedoAction = new Action("Redo") with Reactor {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, CommandMask | Key.Modifier.Shift))
      enabled = false
      def apply() { ruleDocument.undoStack.redo() }

      listenTo(ruleDocument.undoStack)
      reactions += { case _: UndoEvent =>
        enabled = ruleDocument.undoStack.canRedo
        title = "Redo " + ruleDocument.undoStack.redoActionName.getOrElse("")
      }
    }

    val LayoutAction = new Action("Layout Graph") with Reactor {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_L, CommandMask))
      def apply() {
        ruleEditPanel.lhsController.layoutGraph()
        ruleEditPanel.rhsController.layoutGraph()
      }
    }

    contents += new MenuItem(UndoAction) { mnemonic = Key.U }
    contents += new MenuItem(RedoAction) { mnemonic = Key.R }
    contents += new MenuItem(LayoutAction) { mnemonic = Key.L }
  }

  def top = new MainFrame {
    title = "QRule Editor - " + ruleDocument.titleDescription
    contents = ruleEditPanel

    size = new Dimension(1000,800)

    menuBar = new MenuBar {
      contents += (FileMenu, EditMenu)
    }

    listenTo(ruleDocument)
    reactions += {
      case DocumentChanged(_)|DocumentSaved(_) =>
        title = "QRule Editor - " + ruleDocument.titleDescription
    }
  }
}
