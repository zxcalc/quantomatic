package quanto.gui

import quanto.data._
import scala.swing._
import quanto.util.swing.ToolBar
import quanto.gui.graphview.GraphView
import scala.swing.event._
import javax.swing.ImageIcon
import quanto.gui.histview.HistView


class DerivationPanel(val project: Project)
  extends BorderPanel
  with HasDocument
{
  def theory = project.theory
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

  val lhsController = new GraphEditController(LhsView, document.undoStack, readOnly = true)
  val rhsController = new GraphEditController(RhsView, document.undoStack, readOnly = true)

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

  val toolbarDim = RewindButton.preferredSize

  val ManualRewritePane = new BorderPanel {
    val Rewrites = new ListView[ResultLine]
    val RewritesScrollPane = new ScrollPane(Rewrites)
    RewritesScrollPane.preferredSize = new Dimension(400,200)

    val Preview = new GraphView(theory, DummyRef)
    val PreviewScrollPane = new ScrollPane(Preview)
    Preview.zoom = 0.6

    val rewritePreviewController =
      new GraphEditController(Preview, document.undoStack, readOnly = true)
    
    val AddRuleButton = new Button {
      icon = new ImageIcon(GraphEditor.getClass.getResource("list-add.png"), "Add Rule")
      preferredSize = toolbarDim
    }
    val RemoveRuleButton = new Button {
      icon = new ImageIcon(GraphEditor.getClass.getResource("list-remove.png"), "Remove Rule")
      preferredSize = toolbarDim
    }

    val PreviousResultButton = new Button() {
      icon = new ImageIcon(GraphEditor.getClass.getResource("go-previous.png"), "Previous result")
      preferredSize = toolbarDim
    }

    val NextResultButton = new Button() {
      icon = new ImageIcon(GraphEditor.getClass.getResource("go-next.png"), "Next result")
      preferredSize = toolbarDim
    }

    val ApplyButton = new Button("Apply")
    ApplyButton.preferredSize = new Dimension(ApplyButton.preferredSize.width, toolbarDim.height)

    val topPane = new BorderPanel {
      add(RewritesScrollPane, BorderPanel.Position.Center)
      add(new FlowPanel(FlowPanel.Alignment.Left)(
        AddRuleButton, RemoveRuleButton, PreviousResultButton, NextResultButton, ApplyButton
      ), BorderPanel.Position.South)
    }

    add(new SplitPane(Orientation.Horizontal, topPane, PreviewScrollPane), BorderPanel.Position.Center)
  }

//  val SimplifyPane = new BorderPanel {
//    val RefreshButton = new Button {
//      icon = new ImageIcon(GraphEditor.getClass.getResource("view-refresh.png"), "Refresh")
//      preferredSize = toolbarDim
//    }
//
//    val SimplifyButton = new Button("Simplify")
//    SimplifyButton.preferredSize = new Dimension(SimplifyButton.preferredSize.width, toolbarDim.height)
//
//    val topPane = new BorderPanel {
//      add(RewritePane, BorderPanel.Position.Center)
//      add(new FlowPanel(FlowPanel.Alignment.Left)(
//        RefreshButton, SimplifyButton
//      ), BorderPanel.Position.South)
//    }
//
//    add(new SplitPane(Orientation.Horizontal, topPane, RewritePane.RewritePreviewPane), BorderPanel.Position.Center)
//  }

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
  listenTo(LhsGraphPane, RhsGraphPane, ManualRewritePane.PreviewScrollPane)

  reactions += {
    case UIElementResized(LhsGraphPane) =>
      LhsView.resizeViewToFit()
      LhsView.repaint()
    case UIElementResized(RhsGraphPane) =>
      RhsView.resizeViewToFit()
      RhsView.repaint()
    case UIElementResized(ManualRewritePane.PreviewScrollPane) =>
      ManualRewritePane.Preview.resizeViewToFit()
      ManualRewritePane.Preview.repaint()
  }

  // construct the controller last, as it depends on the panel elements already being initialised
  val controller = new DerivationController(this)

  val rewriteController = new RewriteController(this)
//  rewriteController.rules = Vector(RuleDesc("axioms/test1", inverse = false), RuleDesc("axioms/test2", inverse = true))
}