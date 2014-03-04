package quanto.gui

import quanto.gui.graphview.GraphView
import quanto.data.{Theory, Graph}
import scala.swing.{BorderPanel, ScrollPane}
import scala.swing.event.UIElementResized

class RuleEditPanel(val theory: Theory, val readOnly: Boolean)
extends BorderPanel
with GraphEditControls
{
  // GUI components
  val graphView = new GraphView(theory) {
    drawGrid = true
    focusable = true
  }

  val graphDocument = new GraphDocument(graphView)
  def graph = graphDocument.graph
  def graph_=(g: Graph) { graphDocument.graph = g }

  // alias for graph_=, used in java code
  def setGraph(g: Graph) { graph_=(g) }

  val graphEditController = new GraphEditController(graphView, readOnly) {
    undoStack            = graphDocument.undoStack
    vertexTypeSelect     = VertexTypeSelect
    edgeTypeSelect       = EdgeTypeSelect
    edgeDirectedCheckBox = EdgeDirected
  }

  def setMouseState(m: MouseState) { graphEditController.mouseState = m }

  val GraphViewScrollPane = new ScrollPane(graphView)

  if (!readOnly) {
    add(MainToolBar, BorderPanel.Position.North)
    add(BottomPanel, BorderPanel.Position.South)
  }

  add(GraphViewScrollPane, BorderPanel.Position.Center)


  listenTo(GraphViewScrollPane, graphDocument)

  reactions += {
    case UIElementResized(GraphViewScrollPane) =>
      graphView.resizeViewToFit()
      graphView.repaint()
  }
}
