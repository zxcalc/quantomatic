package quanto.gui

import graphview._
import swing._
import event.{ButtonClicked, Key, UIElementResized,Event}
import quanto.data._
import quanto.layout._
import Names._
import javax.swing.{ImageIcon, JToolBar, KeyStroke}
import java.awt.event.KeyEvent
import quanto.util.json.{Json, JsonObject, JsonPath}
import java.awt.Color
import javax.swing.border.EmptyBorder

class ToolBar extends Component with SequentialContainer.Wrapper {
  override lazy val peer: JToolBar = new JToolBar
  def add( action: Action ) { peer.add( action.peer )}
  def add( component: Component ) { peer.add( component.peer )}
}


object GraphEditor extends SimpleSwingApplication {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  println("loading theory " + GraphEditor.getClass.getResource("string_ve.qtheory"))
  val thyFile = new Json.Input(GraphEditor.getClass.getResourceAsStream("string_ve.qtheory"))
  val StringVETheory = Theory.fromJson(Json.parse(thyFile))

  // GUI components
  val graphView = new GraphView(StringVETheory) {
    drawGrid = true
    dynamicResize = true
    focusable = true
  }

  val graphDocument = new GraphDocument(graphView)


  val VertexTypeLabel  = new Label("Vertex Type:  ") { xAlignment = Alignment.Right; enabled = false }
  val VertexTypeSelect = new ComboBox("<wire>" +: StringVETheory.vertexTypes.keys.toSeq) { enabled = false }
  val EdgeTypeLabel    = new Label("Edge Type:  ") { xAlignment = Alignment.Right; enabled = false }
  val EdgeTypeSelect   = new ComboBox(StringVETheory.edgeTypes.keys.toSeq) { enabled = false }
  val EdgeDirected     = new CheckBox("directed") { selected = true; enabled = false }

  // Bottom panel
  object BottomPanel extends GridPanel(1,5) {
    contents += (VertexTypeLabel, VertexTypeSelect)
    contents += (EdgeTypeLabel, EdgeTypeSelect, EdgeDirected)
  }

  val graphEditController = new GraphEditController(graphView) {
    undoStack            = graphDocument.undoStack
    vertexTypeSelect     = VertexTypeSelect
    edgeTypeSelect       = EdgeTypeSelect
    edgeDirectedCheckBox = EdgeDirected
  }

  val GraphViewScrollPane = new ScrollPane(graphView)

  trait ToolButton { var tool: MouseState = SelectTool() }

  val SelectButton = new ToggleButton() with ToolButton {
    icon = new ImageIcon(GraphEditor.getClass.getResource("select-rectangular.png"), "Select")
    selected = true
    tool = SelectTool()
  }

  val AddVertexButton = new ToggleButton() with ToolButton {
    icon = new ImageIcon(GraphEditor.getClass.getResource("draw-ellipse.png"), "Add Vertex")
    tool = AddVertexTool()
  }

  val AddEdgeButton = new ToggleButton() with ToolButton {
    icon = new ImageIcon(GraphEditor.getClass.getResource("draw-path.png"), "Add Edge")
    tool = AddEdgeTool()
  }

  val GraphToolGroup = new ButtonGroup(SelectButton, AddVertexButton, AddEdgeButton)

  val MainToolBar = new ToolBar {
    contents += (SelectButton, AddVertexButton, AddEdgeButton)
  }

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
    title = "QGraph Editor - " + graphDocument.titleDescription
    contents = new BorderPanel {
      add(MainToolBar, BorderPanel.Position.North)
      add(GraphViewScrollPane, BorderPanel.Position.Center)
      add(BottomPanel, BorderPanel.Position.South)
    }

    size = new Dimension(800,800)

    menuBar = new MenuBar {
      contents += (FileMenu, EditMenu)
    }

    listenTo(GraphViewScrollPane, graphDocument)
    GraphToolGroup.buttons.foreach(listenTo(_))
    reactions += {
      case UIElementResized(GraphViewScrollPane) => graphView.repaint()
      case ButtonClicked(t: ToolButton) =>
        graphEditController.mouseState = t.tool
        t.tool match {
          case SelectTool() =>
            VertexTypeLabel.enabled = false
            VertexTypeSelect.enabled = false
            EdgeTypeLabel.enabled = false
            EdgeTypeSelect.enabled = false
            EdgeDirected.enabled = false
          case AddVertexTool() =>
            VertexTypeLabel.enabled = true
            VertexTypeSelect.enabled = true
            EdgeTypeLabel.enabled = false
            EdgeTypeSelect.enabled = false
            EdgeDirected.enabled = false
          case AddEdgeTool() =>
            VertexTypeLabel.enabled = false
            VertexTypeSelect.enabled = false
            EdgeTypeLabel.enabled = true
            EdgeTypeSelect.enabled = true
            EdgeDirected.enabled = true
          case _ =>
        }
      case GraphChanged(_)|GraphSaved(_) =>
        title = "QGraph Editor - " + graphDocument.titleDescription
    }
  }
}