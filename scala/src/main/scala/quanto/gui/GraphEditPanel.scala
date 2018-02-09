package quanto.gui

import graphview.GraphView
import quanto.data._

import swing._
import swing.event._
import javax.swing.ImageIcon

import quanto.util.UserAlerts
import quanto.util.swing.ToolBar

case class MouseStateChanged(m : MouseState) extends Event

class GraphEditControls(theory: Theory) extends Publisher {

  val VertexTypeLabel  = new Label("Vertex Type:  ") { xAlignment = Alignment.Right; enabled = false }
  val vertexOptions : Seq[String] = theory.vertexTypes.keys.toSeq :+ "<wire>"
  val VertexTypeSelect = new ComboBox(vertexOptions) { enabled = false }
  val EdgeTypeLabel    = new Label("Edge Type:  ") { xAlignment = Alignment.Right; enabled = false }
  val edgeOptions : Seq[String] = theory.edgeTypes.keys.toSeq
  val EdgeTypeSelect   = new ComboBox(edgeOptions) { enabled = false }
  val EdgeDirected     = new CheckBox("directed") { selected = false; enabled = false }

  // Bottom panel
  object BottomPanel extends GridPanel(1,5) {
    contents += (VertexTypeLabel, VertexTypeSelect)
    contents += (EdgeTypeLabel, EdgeTypeSelect, EdgeDirected)
  }

  trait ToolButton { var tool: MouseState = SelectTool() }

  val ge = GraphEditor.getClass

//  val icon = new ImageIcon(GraphEditor.getClass.getResource("select-rectangular.png"), "Select")

  val SelectButton = new ToggleButton() with ToolButton {
    icon = new ImageIcon(GraphEditor.getClass.getResource("select-rectangular.png"), "Select")
    tool = SelectTool()
    tooltip = "Select (S)"
    selected = true
  }

  val AddVertexButton = new ToggleButton() with ToolButton {
    icon = new ImageIcon(GraphEditor.getClass.getResource("draw-ellipse.png"), "Add Vertex")
    tool = AddVertexTool()
    tooltip = "Add Vertex (V)"
  }

  val AddBoundaryButton = new ToggleButton() with ToolButton {
    icon = new ImageIcon(GraphEditor.getClass.getResource("draw-ellipse-b.png"), "Add Boundary")
    tool = AddBoundaryTool()
    tooltip = "Add Boundary (I/O)"
  }

  val AddEdgeButton = new ToggleButton() with ToolButton {
    icon = new ImageIcon(GraphEditor.getClass.getResource("add-edge.png"), "Add Edge")
    tool = AddEdgeTool()
    tooltip = "Add Edge (E)"
  }

  val AddBangBoxButton = new ToggleButton() with ToolButton {
    icon = new ImageIcon(GraphEditor.getClass.getResource("draw-bbox.png"), "Add Bang Box")
    tool = AddBangBoxTool()
    tooltip = "Add Bang Box (B)"
  }

  val RelaxButton = new ToggleButton() with ToolButton {
    icon = new ImageIcon(GraphEditor.getClass.getResource("expand.png"), "Relax graph")
    tool = RelaxToolDown()
    tooltip = "Relax graph (R/shift-R)"
  }


  val FreehandButton = new ToggleButton() with ToolButton {
    icon = new ImageIcon(GraphEditor.getClass.getResource("draw-path.png"), "Freehand drawing")
    tool = FreehandTool(None, None)
    tooltip = "Freehand draw (F)"
  }



  val NormaliseButton = new ToggleButton() with ToolButton {
    icon = new ImageIcon(GraphEditor.getClass.getResource("normalise.png"), "Normalise")
    tooltip = "Normalise and straighten edges (N)"
    tool = RequestNormaliseGraph()
  }

  val GraphToolGroup = new ButtonGroup(SelectButton,
    AddVertexButton,
    AddBoundaryButton,
    AddEdgeButton,
    AddBangBoxButton,
    FreehandButton)

  def setMouseState(m : MouseState) {
    val previousToolButton = GraphToolGroup.selected
    publish(MouseStateChanged(m))
    m match {
      case FreehandTool(_,_) =>
        VertexTypeLabel.enabled = false
        VertexTypeSelect.enabled = false
        EdgeTypeLabel.enabled = false
        EdgeTypeSelect.enabled = false
        EdgeDirected.enabled = false
        GraphToolGroup.select(FreehandButton)
      case SelectTool() =>
        VertexTypeLabel.enabled = false
        VertexTypeSelect.enabled = false
        EdgeTypeLabel.enabled = false
        EdgeTypeSelect.enabled = false
        EdgeDirected.enabled = false
        GraphToolGroup.select(SelectButton)
      case AddVertexTool() =>
        if(previousToolButton.nonEmpty && previousToolButton.get == AddVertexButton){
          //VertexTypeSelect.selection.index = (VertexTypeSelect.selection.index + 1) % vertexOptions.size
        }
        VertexTypeLabel.enabled = true
        VertexTypeSelect.enabled = true
        EdgeTypeLabel.enabled = false
        EdgeTypeSelect.enabled = false
        EdgeDirected.enabled = false
        GraphToolGroup.select(AddVertexButton)
      case AddEdgeTool() =>
        if(previousToolButton.nonEmpty && previousToolButton.get == AddEdgeButton){
          //EdgeTypeSelect.selection.index = (EdgeTypeSelect.selection.index + 1) % edgeOptions.size
        }
        VertexTypeLabel.enabled = false
        VertexTypeSelect.enabled = false
        EdgeTypeLabel.enabled = true
        EdgeTypeSelect.enabled = true
        EdgeDirected.enabled = true
        GraphToolGroup.select(AddEdgeButton)
      case AddBangBoxTool() =>
        VertexTypeLabel.enabled = false
        VertexTypeSelect.enabled = false
        EdgeTypeLabel.enabled = false
        EdgeTypeSelect.enabled = false
        EdgeDirected.enabled = false
        GraphToolGroup.select(AddBangBoxButton)
      case AddBoundaryTool() =>
        VertexTypeLabel.enabled = true
        VertexTypeSelect.enabled = true
        EdgeTypeLabel.enabled = false
        EdgeTypeSelect.enabled = false
        EdgeDirected.enabled = false
        GraphToolGroup.select(AddBoundaryButton)
      case _ =>
    }
  }

  GraphToolGroup.buttons.foreach(listenTo(_))
  reactions += {
    case ButtonClicked(t: ToolButton) =>
      setMouseState(t.tool)
  }

  listenTo(RelaxButton.mouse.clicks, NormaliseButton)

  reactions += {
    case MousePressed(RelaxButton,_,_,_,_) =>
      RelaxButton.selected = false
      publish(MouseStateChanged(RelaxToolDown()))
    case MouseReleased(RelaxButton,_,_,_,_) =>
      RelaxButton.selected= false
      publish(MouseStateChanged(RelaxToolUp()))
    case ButtonClicked(NormaliseButton) =>
      NormaliseButton.selected = false
      //publish(MouseStateChanged(RequestNormaliseGraph()))
  }

  val MainToolBar = new ToolBar {
    contents += (SelectButton, AddVertexButton, AddBoundaryButton, AddEdgeButton, AddBangBoxButton, FreehandButton)
  }
  MainToolBar.peer.addSeparator()
  MainToolBar.contents += RelaxButton
  MainToolBar.contents += NormaliseButton

}


class GraphEditPanel(val theory: Theory, val readOnly: Boolean = false)
extends BorderPanel
with HasDocument
{

  val document = new GraphDocument(this, theory)
//  def graph = document.graph
//  def graph_=(g: Graph) { document.graph = g }

  // GUI components
  val graphView = new GraphView(theory, document) {
    drawGrid = true
    focusable = true
  }

  val controls = new GraphEditControls(theory)

  // alias for graph_=, used in java code
//  def setGraph(g: Graph) { graph_=(g) }

  val graphEditController = new GraphEditController(graphView, document.undoStack, readOnly)
  graphEditController.controlsOpt = Some(controls)

  val GraphViewScrollPane = new ScrollPane(graphView)

  if (!readOnly) {
    add(controls.MainToolBar, BorderPanel.Position.North)
    add(controls.BottomPanel, BorderPanel.Position.South)
  }

  add(GraphViewScrollPane, BorderPanel.Position.Center)


  listenTo(GraphViewScrollPane, controls, document)

  reactions += {
    case UIElementResized(GraphViewScrollPane) =>
      graphView.resizeViewToFit()
      graphView.repaint()
    case MouseStateChanged(RelaxToolDown()) => graphEditController.startRelaxGraph(true)
    case MouseStateChanged(RelaxToolUp()) => graphEditController.endRelaxGraph()
    case MouseStateChanged(RequestNormaliseGraph()) => graphEditController.normaliseGraph()
    case MouseStateChanged(m) =>
      if (graphEditController.rDown) graphEditController.endRelaxGraph()
      graphEditController.mouseState = m
    }
}
