package quanto.gui

import quanto.data._
import scala.swing._
import quanto.util.swing.ToolBar
import quanto.gui.graphview.GraphView
import scala.swing.event._
import javax.swing.ImageIcon
import quanto.gui.histview.HistView


class DerivationPanel(val theory: Theory)
  extends BorderPanel
  with HasDocument
{
  val document = new DerivationDocument(this)
  def derivation = document.derivation

  object DummyRef extends HasGraph { var gr = Graph(theory) }

  // GUI components
  val LhsView = new GraphView(theory, document.rootRef) {
    drawGrid = true
    focusable = true
  }

  LhsView.trans.scale = 20.0f
  LhsView.trans.origin = (100.0f, 100.0f)

  val RhsView = new GraphView(theory, DummyRef) {
    drawGrid = true
    focusable = true
  }

  RhsView.trans.scale = 20.0f
  RhsView.trans.origin = (100.0f, 100.0f)

  val controls = new GraphEditControls(theory)

  val lhsController = new GraphEditController(LhsView, true) {
    undoStack            = document.undoStack
    vertexTypeSelect     = controls.VertexTypeSelect
    edgeTypeSelect       = controls.EdgeTypeSelect
    edgeDirectedCheckBox = controls.EdgeDirected
  }

  val rhsController = new GraphEditController(RhsView, true) {
    undoStack            = document.undoStack
    vertexTypeSelect     = controls.VertexTypeSelect
    edgeTypeSelect       = controls.EdgeTypeSelect
    edgeDirectedCheckBox = controls.EdgeDirected
  }

  val RewindButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("go-first.png"), "First step")
  }

  val PreviousButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("go-previous.png"), "Previous step")
  }

  val NextButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("go-next.png"), "Next step")
  }

  val FastForwardButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("go-last.png"), "Last step")
  }

  val navigationButtons = List(RewindButton, PreviousButton, NextButton, FastForwardButton)

  val DeriveToolbar = new ToolBar {
    contents += (RewindButton, PreviousButton, NextButton, FastForwardButton)
  }

  val LhsGraphPane = new ScrollPane(LhsView)
  val RhsGraphPane = new ScrollPane(RhsView)
  val RhsRewritePane = new BorderPanel

  val LhsLabel = new Label("(root)")
  val RhsLabel = new Label("(head)")

  val Lhs = new BorderPanel {
    add(LhsGraphPane, BorderPanel.Position.Center)
    add(LhsLabel, BorderPanel.Position.South)
  }

  val Rhs = new BorderPanel {
    def setStepMode() {
      add(RhsGraphPane, BorderPanel.Position.Center)
      revalidate()
      repaint()
    }
    
    def setHeadMode() {
      add(RhsRewritePane, BorderPanel.Position.Center)
      revalidate()
      repaint()
    }

    add(RhsLabel, BorderPanel.Position.South)
  }

  Rhs.setStepMode()

  object GraphViewPanel extends GridPanel(1,2) {
    contents += (Lhs, Rhs)
  }

  val histView = new HistView(derivation)

  add(DeriveToolbar, BorderPanel.Position.North)
  add(GraphViewPanel, BorderPanel.Position.Center)

  reactions += {
    case UIElementResized(LhsGraphPane) =>
      LhsView.resizeViewToFit()
      LhsView.repaint()
    case UIElementResized(RhsGraphPane) =>
      RhsView.resizeViewToFit()
      RhsView.repaint()
  }

  // construct the controller last, as it depends on the panel elements already being initialised
  val controller = new DerivationController(this)
}