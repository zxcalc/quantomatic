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

  val RhsView = new GraphView(theory, DummyRef) {
    drawGrid = true
    focusable = true
  }

  LhsView.zoom = 0.6
  RhsView.zoom = 0.6

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

  val RewriteList = new ListView
  RewriteList.preferredSize = new Dimension(400,200)

  val RewritePreview = new GraphView(theory, DummyRef) {
    drawGrid = true
    focusable = true
  }

  RewritePreview.zoom = 0.5

  val PreviewGraphPane = new ScrollPane(RewritePreview)

  val ManualRewritePane = new BorderPanel {
    val AddRuleButton = new Button {
      icon = new ImageIcon(GraphEditor.getClass.getResource("list-add.png"), "Add Rule")
      preferredSize = RewindButton.preferredSize
    }
    val RemoveRuleButton = new Button {
      icon = new ImageIcon(GraphEditor.getClass.getResource("list-remove.png"), "Remove Rule")
      preferredSize = RewindButton.preferredSize
    }
    add(new BorderPanel {
      add(RewriteList, BorderPanel.Position.Center)
      add(new FlowPanel(FlowPanel.Alignment.Left)(AddRuleButton, RemoveRuleButton),
        BorderPanel.Position.South)
    }, BorderPanel.Position.North)
    add(PreviewGraphPane, BorderPanel.Position.Center)
  }

  val RhsRewritePane = new TabbedPane
  RhsRewritePane.pages += new TabbedPane.Page("Rewrite", ManualRewritePane)
  RhsRewritePane.pages += new TabbedPane.Page("Simplify", new BorderPanel)

  val LhsLabel = new Label("(root)")
  val RhsLabel = new Label("(head)")

  val Lhs = new BorderPanel {
    add(DeriveToolbar, BorderPanel.Position.North)
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