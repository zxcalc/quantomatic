package quanto.gui

import quanto.gui.graphview.GraphView
import quanto.data.{Theory, Graph}
import scala.swing.{GridPanel, BorderPanel, ScrollPane}
import scala.swing.event.UIElementResized

class RuleEditPanel(val theory: Theory, val readOnly: Boolean = false)
extends BorderPanel
with GraphEditControls
with HasDocument
{
  // GUI components
  val lhsView = new GraphView(theory) {
    drawGrid = true
    focusable = true
  }

  val rhsView = new GraphView(theory) {
    drawGrid = true
    focusable = true
  }

  val document = new RuleDocument(lhsView, rhsView)

  val lhsController = new GraphEditController(lhsView, readOnly) {
    undoStack            = document.undoStack
    vertexTypeSelect     = VertexTypeSelect
    edgeTypeSelect       = EdgeTypeSelect
    edgeDirectedCheckBox = EdgeDirected
  }

  val rhsController = new GraphEditController(rhsView, readOnly) {
    undoStack            = document.undoStack
    vertexTypeSelect     = VertexTypeSelect
    edgeTypeSelect       = EdgeTypeSelect
    edgeDirectedCheckBox = EdgeDirected
  }

  def setMouseState(m: MouseState) {
    lhsController.mouseState = m
    rhsController.mouseState = m
  }

  val LhsScrollPane = new ScrollPane(lhsView)
  val RhsScrollPane = new ScrollPane(rhsView)

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
      lhsView.resizeViewToFit()
      lhsView.repaint()
    case UIElementResized(RhsScrollPane) =>
      rhsView.resizeViewToFit()
      rhsView.repaint()
  }
}
