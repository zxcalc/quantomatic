package quanto.gui

import graphview._
import swing._
import event.{ButtonClicked, Key, UIElementResized}
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

  // populate with a random graph, for testing
  //var theGraph = Graph.random(10,15,0)

  println("loading theory " + GraphEditor.getClass.getResource("string_ve.qtheory"))
  val thyFile = new Json.Input(GraphEditor.getClass.getResourceAsStream("string_ve.qtheory"))
  val StringVETheory = Theory.fromJson(Json.parse(thyFile))

  var theGraph = Graph.fromJson(
    """
      |{
      |  "node_vertices": {
      |    "a": {"data": {"type": "string", "value": "some long string"}},
      |    "b": {"data": {"type": "string", "value": "foo"}},
      |    "c": {"data": {"type": "string", "value": "bar"}},
      |    "d": {"data": {"type": "string", "value": "baz"}}
      |  },
      |  "dir_edges": {
      |    "e0": {"src":"b", "tgt":"a", "data": {"type": "string", "value": "here"}},
      |    "e0p": {"src":"b", "tgt":"a", "data": {"type": "string", "value": "there"}},
      |    "e1": {"src":"a", "tgt":"d"},
      |    "e2": {"src":"b", "tgt":"d"},
      |    "e3": {"src":"c", "tgt":"b"}
      |  }
      |}
    """.stripMargin, StringVETheory)
  val layoutEngine = new DotLayout

  theGraph = layoutEngine.layout(theGraph)
  //println(layoutEngine.dotString)

  // GUI components
  val MainGraphView = new GraphView {
    graph = theGraph
    drawGrid = true
    dynamicResize = true
    focusable = true
  }

  val graphEditController = new GraphEditController(MainGraphView)

  val MainUndoStack = graphEditController.undoStack
  val GraphViewScrollPane = new ScrollPane(MainGraphView)

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
    tool = AddEdgeTool(directed = true)
  }

  val GraphToolGroup = new ButtonGroup(SelectButton, AddVertexButton, AddEdgeButton)

  val MainToolBar = new ToolBar {
    contents += (SelectButton, AddVertexButton, AddEdgeButton)
  }


  // Actions associated with main menu
  val UndoAction = new Action("Undo") with Reactor {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, CommandMask))
    def apply() { MainUndoStack.undo() }

    def update() {
      enabled = MainUndoStack.canUndo
      title = "Undo " + MainUndoStack.undoActionName.getOrElse("")
    }

    listenTo(MainUndoStack)
    reactions += { case _: UndoEvent => update() }; update()
  }

  val RedoAction = new Action("Redo") with Reactor {
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, CommandMask | Key.Modifier.Shift))
    def apply() { MainUndoStack.redo() }

    def update() {
      enabled = MainUndoStack.canRedo
      title = "Redo " + MainUndoStack.redoActionName.getOrElse("")
    }

    listenTo(MainUndoStack)
    reactions += { case _: UndoEvent => update() }; update()
  }

  // Bottom panel
  val BottomPanel = new GridPanel(2,1) {
    vGap = 3
    border = new EmptyBorder(3,3,3,3)
    contents += new GridPanel(1,5) {
      contents += new Label("Vertex Type:  ") { xAlignment = Alignment.Right }
      contents += new ComboBox(Seq("<wire>", "string"))
      contents += new Label("Edge Type:  ") { xAlignment = Alignment.Right }
      contents += new ComboBox(Seq("string"))
      contents += new CheckBox("directed") { selected = true }
    }
    contents += new TextField()
  }

  // Main menu

  val FileMenu = new Menu("File") { mnemonic = Key.F }

  val EditMenu = new Menu("Edit") {
    mnemonic = Key.E
    contents += new MenuItem(UndoAction) { mnemonic = Key.U }
    contents += new MenuItem(RedoAction) { mnemonic = Key.R }
  }

  def top = new MainFrame {
    title = "Quanto Graph Editor"
    contents = new BorderPanel {
      add(MainToolBar, BorderPanel.Position.North)
      add(GraphViewScrollPane, BorderPanel.Position.Center)
      add(BottomPanel, BorderPanel.Position.South)
    }

    size = new Dimension(800,800)

    menuBar = new MenuBar {
      contents += (FileMenu, EditMenu)
    }

    listenTo(GraphViewScrollPane)
    GraphToolGroup.buttons.foreach(listenTo(_))
    reactions += {
      case UIElementResized(GraphViewScrollPane) => MainGraphView.repaint()
      case ButtonClicked(t: ToolButton) => graphEditController.mouseState = t.tool
    }
  }
}