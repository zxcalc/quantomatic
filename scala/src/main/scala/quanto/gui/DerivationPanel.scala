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

  //val controls = new GraphEditControls(theory)

  val lhsController = new GraphEditController(LhsView, document.undoStack, readOnly = true)
  val rhsController = new GraphEditController(RhsView, document.undoStack, readOnly = true)

  val RewindButton = new Button() {
    icon = new ImageIcon(getClass.getResource("go-first.png"), "First step")
    tooltip = "First Step"
  }

  val PreviousButton = new Button() {
    icon = new ImageIcon(getClass.getResource("go-previous.png"), "Previous step")
    tooltip = "Previous Step"
  }

  val NextButton = new Button() {
    icon = new ImageIcon(getClass.getResource("go-next.png"), "Next step")
    tooltip = "Next step"
  }

  val FastForwardButton = new Button() {
    icon = new ImageIcon(getClass.getResource("go-last.png"), "Last step")
    tooltip = "Last step"
  }

  val NewHeadButton = new Button() {
    icon = new ImageIcon(getClass.getResource("edit-copy.png"), "New proof head")
    tooltip = "New proof head"
  }

  val DeleteStepButton = new Button() {
    icon = new ImageIcon(getClass.getResource("edit-delete.png"), "Delete proof step(s)")
    tooltip = "Delete proof step(s)"
  }

  val ExportTheoremButton = new Button() {
    icon = new ImageIcon(getClass.getResource("document-save.png"), "Export theorem")
    tooltip = "Export theorem"
  }

  val derivationButtons = List(
    RewindButton, PreviousButton, NextButton, FastForwardButton,
    NewHeadButton, DeleteStepButton, ExportTheoremButton)

  val DeriveToolbar = new ToolBar {
    contents += (RewindButton, PreviousButton, NextButton, FastForwardButton, NewHeadButton,
      DeleteStepButton, ExportTheoremButton)
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

    // Note that the preview panel doesn't register to the derivation's undo stack. Things
    // could go weird if we allow undo's in previews, the graphRef changes so much.
    val rewritePreviewController =
      new GraphEditController(Preview, new UndoStack(), readOnly = true)
    
    val AddRuleButton = new Button {
      icon = new ImageIcon(GraphEditor.getClass.getResource("list-add.png"), "Add Rule")
      preferredSize = toolbarDim
      tooltip = "Add Rule"
    }
    val RemoveRuleButton = new Button {
      icon = new ImageIcon(GraphEditor.getClass.getResource("list-remove.png"), "Remove Rule")
      preferredSize = toolbarDim
      tooltip = "Remove Rule"
    }

    val PreviousResultButton = new Button() {
      icon = new ImageIcon(GraphEditor.getClass.getResource("go-previous.png"), "Previous result")
      preferredSize = toolbarDim
      tooltip = "Previous result"
    }

    val NextResultButton = new Button() {
      icon = new ImageIcon(GraphEditor.getClass.getResource("go-next.png"), "Next result")
      preferredSize = toolbarDim
      tooltip = "Next result"
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

  val SimplifyPane = new BorderPanel {
    val Simprocs = new ListView[String]
    val SimprocsScrollPane = new ScrollPane(Simprocs)
    SimprocsScrollPane.preferredSize = new Dimension(400,200)

    val Preview = new GraphView(theory, DummyRef)
    val PreviewScrollPane = new ScrollPane(Preview)
    Preview.zoom = 0.6

    val RefreshButton = new Button {
      icon = new ImageIcon(GraphEditor.getClass.getResource("view-refresh.png"), "Refresh")
      preferredSize = toolbarDim
      tooltip = "Refresh"
    }

    val SimplifyButton = new Button {
      icon = new ImageIcon(GraphEditor.getClass.getResource("start.png"))
      preferredSize = toolbarDim
      tooltip = "Start"
    }

    val StopButton = new Button {
      icon = new ImageIcon(GraphEditor.getClass.getResource("stop.png"))
      preferredSize = toolbarDim
      tooltip = "Stop"
    }


    val GreedyButton = new Button("Greedy Reduce")
    val AnnealButton = new Button("Anneal")
    val RandomButton = new Button("Random x100")
    val LTEButton = new Button("LTE x100")
    val EvaluateButton = new Button("Evaluate")

    val topPane = new BorderPanel {
      add(new FlowPanel(FlowPanel.Alignment.Left)(
        AnnealButton, GreedyButton, RandomButton, LTEButton, EvaluateButton
      ), BorderPanel.Position.Center)
    }

    add(new SplitPane(Orientation.Horizontal, topPane, PreviewScrollPane), BorderPanel.Position.Center)
  }

  val RhsRewritePane = new TabbedPane
  RhsRewritePane.pages += new TabbedPane.Page("Rewrite", ManualRewritePane)
  RhsRewritePane.pages += new TabbedPane.Page("Simplify", SimplifyPane)

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
  listenTo(LhsGraphPane, RhsGraphPane)
  listenTo(ManualRewritePane.PreviewScrollPane, SimplifyPane.PreviewScrollPane)

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
    case UIElementResized(SimplifyPane.PreviewScrollPane) =>
      SimplifyPane.Preview.resizeViewToFit()
      SimplifyPane.Preview.repaint()
  }

  // construct the controller last, as it depends on the panel elements already being initialised
  val controller = new DerivationController(this)

  val rewriteController = new RewriteController(this)
  val simplifyController = new SimplifyController(this)
//  rewriteController.rules = Vector(RuleDesc("axioms/test1", inverse = false), RuleDesc("axioms/test2", inverse = true))
}
