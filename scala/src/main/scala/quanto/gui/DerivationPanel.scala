package quanto.gui

import quanto.data._
import scala.swing.{GridPanel, ScrollPane, BorderPanel}
import quanto.gui.graphview.GraphView
import scala.swing.event.UIElementResized

class DerivationPanel(val theory: Theory, val readOnly: Boolean = false)
  extends BorderPanel
  with GraphEditControls
  with HasDocument
{

  object DummyRef extends HasGraph { var graph = Graph(theory) }

  // GUI components
  val LhsView = new GraphView(theory, DummyRef) {
    drawGrid = true
    focusable = true
  }

  val RhsView = new GraphView(theory, DummyRef) {
    drawGrid = true
    focusable = true
  }

  val document = new DerivationDocument(this)

  val lhsController = new GraphEditController(DummyRef, LhsView, readOnly) {
    undoStack            = document.undoStack
    vertexTypeSelect     = VertexTypeSelect
    edgeTypeSelect       = EdgeTypeSelect
    edgeDirectedCheckBox = EdgeDirected
  }

  val rhsController = new GraphEditController(DummyRef, RhsView, readOnly) {
    undoStack            = document.undoStack
    vertexTypeSelect     = VertexTypeSelect
    edgeTypeSelect       = EdgeTypeSelect
    edgeDirectedCheckBox = EdgeDirected
  }

  def setMouseState(m: MouseState) {
    lhsController.mouseState = m
    rhsController.mouseState = m
  }

  val LhsScrollPane = new ScrollPane(LhsView)
  val RhsScrollPane = new ScrollPane(RhsView)

  object GraphViewPanel extends GridPanel(1,2) {
    contents += LhsScrollPane
    contents += RhsScrollPane
  }

  if (!readOnly) {
    add(MainToolBar, BorderPanel.Position.North)
    add(BottomPanel, BorderPanel.Position.South)
  }

  add(GraphViewPanel, BorderPanel.Position.Center)


  listenTo(LhsScrollPane, RhsScrollPane, document)

  reactions += {
    case UIElementResized(LhsScrollPane) =>
      LhsView.resizeViewToFit()
      LhsView.repaint()
    case UIElementResized(RhsScrollPane) =>
      RhsView.resizeViewToFit()
      RhsView.repaint()
  }
}