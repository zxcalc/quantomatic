package quanto.gui

import quanto.data._
import scala.swing._
import quanto.util.swing.ToolBar
import quanto.gui.graphview.GraphView
import scala.swing.event._
import javax.swing.ImageIcon

sealed abstract class DeriveState
case class StepState(s: DSName) extends DeriveState
case class HeadState(h: Option[DSName]) extends DeriveState

class DerivationPanel(val theory: Theory)
  extends BorderPanel
  with HasDocument
{
  val document = new DerivationDocument(this)
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


  val FirstButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("go-first.png"), "First step")
  }

  val PreviousButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("go-previous.png"), "Previous step")
  }

  val NextButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("go-next.png"), "Next step")
  }

  val LastButton = new Button() {
    icon = new ImageIcon(GraphEditor.getClass.getResource("go-last.png"), "Last step")
  }

  val navigationButtons = List(FirstButton, PreviousButton, NextButton, LastButton)

  navigationButtons.foreach { listenTo(_) }

  val DeriveToolbar = new ToolBar {
    contents += (FirstButton, PreviousButton, NextButton, LastButton)
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

  add(DeriveToolbar, BorderPanel.Position.North)
  add(GraphViewPanel, BorderPanel.Position.Center)


  // move to a named step in the derivation
  private var _state : DeriveState = HeadState(None)
  def state = _state
  def state_=(s: DeriveState) {
    _state = s

    s match {
      case HeadState(headOpt) =>
        Rhs.setHeadMode()
        RhsLabel.text = "(head)"

        headOpt match {
          case Some(head) =>
            PreviousButton.enabled = true
            LhsLabel.text = head.toString
          case None => // at the root
            PreviousButton.enabled = false
            LhsLabel.text = "(root)"
        }

      case StepState(step) =>
        Rhs.setStepMode()
        RhsLabel.text = step.toString
        document.derivation.parent.get(step) match {
          case Some(parent) =>
            PreviousButton.enabled = true
            LhsLabel.text = parent.toString
          case None =>
            PreviousButton.enabled = false
            LhsLabel.text = "(root)"
        }
        NextButton.enabled = (document.derivation.isHead(step) && !document.derivation.hasChildren(step)) ||
                             (!document.derivation.isHead(step) && document.derivation.hasUniqueChild(step))
        LastButton.enabled = true
    }
  }

  state = HeadState(None)

  // move to a named head, or None for root (as a head)
  def moveToHead(s: Option[DSName]) {

  }


  listenTo(LhsGraphPane, RhsGraphPane, document)

  reactions += {
    case UIElementResized(LhsGraphPane) =>
      LhsView.resizeViewToFit()
      LhsView.repaint()
    case UIElementResized(RhsGraphPane) =>
      RhsView.resizeViewToFit()
      RhsView.repaint()
    case MouseStateChanged(m) =>
      lhsController.mouseState = m
      rhsController.mouseState = m
    case ButtonClicked(FirstButton) =>
      state = HeadState(None)
    case ButtonClicked(LastButton) =>
    case ButtonClicked(PreviousButton) =>
      state match {
        case HeadState(Some(s)) => state = StepState(s)
        case StepState(s) =>
          document.derivation.parent.get(s) match {
            case Some(p) => state = StepState(p)
            case None => state = HeadState(None)
          }
        case _ => // do nothing
      }
    case ButtonClicked(NextButton) =>
      state match {
        case StepState(s) =>
          if (document.derivation.isHead(s) && !document.derivation.hasChildren(s))
            state = HeadState(Some(s))
          else
            document.derivation.uniqueChild(s).map { ch => state = StepState(ch) }
        case _ => // do nothing
      }
  }
}