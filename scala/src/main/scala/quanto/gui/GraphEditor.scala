package quanto.gui

import graphview._
import swing._
import event.{Key, UIElementResized}
import quanto.data._
import quanto.layout._
import Names._
import javax.swing.{ImageIcon, JToolBar, KeyStroke}
import java.awt.event.KeyEvent
import quanto.util.json.{JsonObject, JsonPath}
import java.awt.Color

class ToolBar extends Component with SequentialContainer.Wrapper {
  override lazy val peer: JToolBar = new JToolBar
  def add( action: Action ) { peer.add( action.peer )}
  def add( component: Component ) { peer.add( component.peer )}
}


object GraphEditor extends SimpleSwingApplication {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  // populate with a random graph, for testing
  //var theGraph = Graph.random(10,15,0)

  val StringVETheory = Theory(
    name = "String vertex/edge theory",
    coreName = "string_ve_theory",
    vertexTypes = Map(
      "string" -> Theory.VertexDesc(
        value = Theory.ValueDesc(
          path = JsonPath("$.value"),
          typ = Theory.ValueType.String
        ),
        style = Theory.VertexStyleDesc(
          shape = Theory.VertexShape.Rectangle,
          labelPosition = Theory.VertexLabelPosition.Inside
        ),
        defaultData = JsonObject("type"->"string","value"->"")
      )
    ),
    edgeTypes = Map(
      "string" -> Theory.EdgeDesc(
        value = Theory.ValueDesc(
          path = JsonPath("$.value"),
          typ = Theory.ValueType.String
        ),
        style = Theory.EdgeStyleDesc(
          labelPosition = Theory.EdgeLabelPosition.Center,
          labelForegroundColor = Color.BLUE,
          labelBackgroundColor = Some(new Color(0.8f,0.8f,1.0f,0.7f))
        ),
        defaultData = JsonObject("type"->"string","value"->"")
      )
    ),
    defaultVertexType = "string",
    defaultEdgeType = "string"
  )

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
  }

  val graphEditController = new GraphEditController(MainGraphView)

  val MainUndoStack = graphEditController.undoStack
  val GraphViewScrollPane = new ScrollPane(MainGraphView)

  val SelectButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("select-rectangular.png"), "Select")
  }

  val AddVertexButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("draw-ellipse.png"), "Add Vertex")
  }

  val AddEdgeButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("draw-path.png"), "Add Edge")
  }

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
    }

    size = new Dimension(800,800)

    menuBar = new MenuBar {
      contents += (FileMenu, EditMenu)
    }

    listenTo(GraphViewScrollPane)
    reactions += {
      case UIElementResized(GraphViewScrollPane) => MainGraphView.repaint()
    }
  }
}