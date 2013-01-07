package quanto.gui

import graphview._
import swing._
import event.{Key, UIElementResized}
import quanto.data._
import quanto.layout._
import Names._
import javax.swing.KeyStroke
import java.awt.event.KeyEvent



object GraphEditor extends SimpleSwingApplication {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  // populate with a random graph, for testing
  var randomGraph = QGraph.random(20,40)
  val layoutEngine = new DotLayout

  randomGraph = layoutEngine.layout(randomGraph)

  // GUI components
  val UndoStack_ = new UndoStack

  val GraphView_ = new GraphView {
    graph = randomGraph
    drawGrid = true
    dynamicResize = true
    undoStack = Some(UndoStack_)
    editMode = GraphView.ReadWrite
  }

  val ScrollPane_ = new ScrollPane(GraphView_)


  // Actions associated with main menu
  val UndoAction_ = new Action("Undo") with Reactor {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, CommandMask))
    def apply() { UndoStack_.undo() }

    def update() {
      enabled = UndoStack_.canUndo
      title = "Undo " + UndoStack_.undoActionName.getOrElse("")
    }

    listenTo(UndoStack_)
    reactions += { case _: UndoEvent => update() }; update()
  }

  val RedoAction_ = new Action("Redo") with Reactor {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, CommandMask | Key.Modifier.Shift))
    def apply() { UndoStack_.redo() }

    def update() {
      enabled = UndoStack_.canRedo
      title = "Redo " + UndoStack_.redoActionName.getOrElse("")
    }

    listenTo(UndoStack_)
    reactions += { case _: UndoEvent => update() }; update()
  }

  // Main menu

  val FileMenu_ = new Menu("File") { mnemonic = Key.F }

  val EditMenu_ = new Menu("Edit") {
    mnemonic = Key.E
    contents += new MenuItem(UndoAction_) { mnemonic = Key.U }
    contents += new MenuItem(RedoAction_) { mnemonic = Key.R }
  }

  def top = new MainFrame {
    title = "Quanto Graph Editor"
    contents = ScrollPane_
    size = new Dimension(800,800)

    menuBar = new MenuBar {
      contents += (FileMenu_, EditMenu_)
    }

    listenTo(ScrollPane_)
    reactions += {
      case UIElementResized(ScrollPane_) => GraphView_.repaint()
    }
  }
}