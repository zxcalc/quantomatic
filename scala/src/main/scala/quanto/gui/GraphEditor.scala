package quanto.gui


import swing._
import event.Key
import quanto.data._
import javax.swing.{JToolBar, KeyStroke}
import java.awt.event.KeyEvent
import quanto.util.json.Json


object GraphEditor extends SimpleSwingApplication {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  // println("loading theory " + GraphEditor.getClass.getResource("strategy_graph.qtheory"))
  // val thyFile = new Json.Input(GraphEditor.getClass.getResourceAsStream("strategy_graph.qtheory"))
  // val StringVETheory = Theory.fromJson(Json.parse(thyFile))
  //val StringVETheory = Theory.DefaultTheory
  println("loading theory " + Theory.getClass.getResource("red_green.qtheory"))
  val thyFile = new Json.Input(Theory.getClass.getResourceAsStream("red_green.qtheory"))
  val thy = Theory.fromJson(Json.parse(thyFile))

  val graphEditPanel = new GraphEditPanel(thy, readOnly = false)
  val graphEditController = graphEditPanel.graphEditController
  val graphDocument = graphEditPanel.graphDocument

//  graphEditPanel.graphView.graph = (Graph(thy)
//    addVertex ("b0", WireV())
//    addVertex ("b1", WireV())
//    addVertex ("b2", WireV())
//    addVertex ("b3", WireV())
//    addVertex ("v0", NodeV())
//    addVertex ("v1", NodeV())
//    addVertex ("v2", NodeV())
//    addVertex ("v3", NodeV())
//    addEdge   ("e0", DirEdge(), "v0" -> "v2")
//    addEdge   ("e1", DirEdge(), "v0" -> "v3")
//    addEdge   ("e2", DirEdge(), "v1" -> "v2")
//    addEdge   ("e3", DirEdge(), "v1" -> "v3")
//    addEdge   ("e4", DirEdge(), "b0" -> "v0")
//    addEdge   ("e5", DirEdge(), "b1" -> "v1")
//    addEdge   ("e6", DirEdge(), "v2" -> "b2")
//    addEdge   ("e7", DirEdge(), "v3" -> "b3")
//    //    addBBox   ("bb1", BBData(), Set("b0","v3"))
//    //    addBBox   ("bb2", BBData(), Set("b0","v1"))
//    //    addBBox   ("bb3", BBData(), Set("v0","v3"))
//    )

  // Main menu

  val FileMenu = new Menu("File") { menu =>
    mnemonic = Key.F

    val NewAction = new Action("New") {
      menu.contents += new MenuItem(this) { mnemonic = Key.N }
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_N, CommandMask))
      def apply() {
        if (graphDocument.promptUnsaved()) graphDocument.clear()
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
            case Some(_) => graphDocument.save()
            case None    => graphDocument.showSaveAsDialog()
          }
        }

        listenTo(graphDocument)
        reactions += { case DocumentChanged(_) | DocumentSaved(_) =>
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

    val LayoutAction = new Action("Layout Graph") with Reactor {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_L, CommandMask))
      def apply() { graphEditController.layoutGraph() }
    }

    contents += new MenuItem(UndoAction) { mnemonic = Key.U }
    contents += new MenuItem(RedoAction) { mnemonic = Key.R }
    contents += new MenuItem(LayoutAction) { mnemonic = Key.L }
  }

  def top = new MainFrame {
    title = "QGraph Editor - " + graphDocument.titleDescription
    contents = graphEditPanel

    size = new Dimension(800,800)

    menuBar = new MenuBar {
      contents += (FileMenu, EditMenu)
    }

    listenTo(graphDocument)
    reactions += {
      case DocumentChanged(_)|DocumentSaved(_) =>
        title = "QGraph Editor - " + graphDocument.titleDescription
    }
  }
}