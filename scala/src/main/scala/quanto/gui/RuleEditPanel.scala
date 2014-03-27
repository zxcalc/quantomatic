package quanto.gui

import quanto.gui.graphview.GraphView
import quanto.data.{HasGraph, Theory, Graph}
import scala.swing.{GridPanel, BorderPanel, ScrollPane}
import scala.swing.event.UIElementResized

class RuleEditPanel(val theory: Theory, val readOnly: Boolean = false)
extends BorderPanel
with HasDocument
{
  val document = new RuleDocument(this, theory)

  // GUI components
  val lhsView = new GraphView(theory, document.lhsRef)
  val rhsView = new GraphView(theory, document.rhsRef)

  val controls = new GraphEditControls(theory)

  val lhsController = new GraphEditController(lhsView, readOnly) {
    undoStack            = document.undoStack
    vertexTypeSelect     = controls.VertexTypeSelect
    edgeTypeSelect       = controls.EdgeTypeSelect
    edgeDirectedCheckBox = controls.EdgeDirected
  }

  val rhsController = new GraphEditController(rhsView, readOnly) {
    undoStack            = document.undoStack
    vertexTypeSelect     = controls.VertexTypeSelect
    edgeTypeSelect       = controls.EdgeTypeSelect
    edgeDirectedCheckBox = controls.EdgeDirected
  }

  val LhsScrollPane = new ScrollPane(lhsView)
  val RhsScrollPane = new ScrollPane(rhsView)

  object GraphViewPanel extends GridPanel(1,2) {
    contents += LhsScrollPane
    contents += RhsScrollPane
  }

  if (!readOnly) {
    add(controls.MainToolBar, BorderPanel.Position.North)
    add(controls.BottomPanel, BorderPanel.Position.South)
  }

  add(GraphViewPanel, BorderPanel.Position.Center)


  listenTo(LhsScrollPane, RhsScrollPane, document, controls)

  reactions += {
    case UIElementResized(LhsScrollPane) =>
      lhsView.resizeViewToFit()
      lhsView.repaint()
    case UIElementResized(RhsScrollPane) =>
      rhsView.resizeViewToFit()
      rhsView.repaint()
    case MouseStateChanged(m) =>
      lhsController.mouseState = m
      rhsController.mouseState = m
  }
}
