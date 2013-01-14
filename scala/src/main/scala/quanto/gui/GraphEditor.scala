package quanto.gui

import graphview._
import swing._
import event.{Key, UIElementResized}
import quanto.data._
import quanto.layout._
import Names._
import javax.swing.KeyStroke
import java.awt.event.KeyEvent
import quanto.util.json.{JsonObject, JsonPath}
import java.awt.Color


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
  val GraphView_ = new GraphView {
    graph = theGraph
    drawGrid = true
    dynamicResize = true
  }

  val GraphEditController_ = new GraphEditController(GraphView_)
  val UndoStack_ = GraphEditController_.undoStack
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