package quanto.gui

import quanto.data.{Derivation, DSName}
import scala.swing._
import scala.swing.event.{SelectionChanged, ButtonClicked, Event}
import quanto.gui.histview._
import java.awt.Color
import quanto.gui.graphview.Highlight
import quanto.layout.DeriveLayout

sealed abstract class DeriveState extends HistNode
case class StepState(s: DSName) extends DeriveState {
  def color = new Color(180,255,180)
  def label = s.toString
}

case class HeadState(hOpt: Option[DSName]) extends DeriveState {
  def color = Color.WHITE
  def label = if (hOpt.isEmpty) "(root)" else "(head)"
}

case class DeriveStateChanged(state: DeriveState) extends Event

class DerivationController(panel: DerivationPanel) extends Publisher {
  def derivation = panel.derivation

  // move to a named step in the derivation
  private var _state : DeriveState = HeadState(None)
  def state = _state
  def state_=(s: DeriveState) {
    _state match {
      case HeadState(_) => panel.rewriteController.deafTo(panel.LhsView)
      case _ =>
    }

    _state = s
    panel.histView.selectedNode = Some(s)

    s match {
      case HeadState(headOpt) =>
        panel.Rhs.setHeadMode()
        panel.RhsLabel.text = "(head)"
        panel.RhsView.graphRef = panel.DummyRef

        headOpt match {
          case Some(head) => // at a named head
            panel.PreviousButton.enabled = true
            panel.NextButton.enabled = false
            panel.RewindButton.enabled = true
            panel.FastForwardButton.enabled = false
            panel.LhsLabel.text = head.toString
            panel.LhsView.graphRef = panel.document.stepRef(head)
          case None => // at the root
            panel.PreviousButton.enabled = false
            panel.NextButton.enabled = !derivation.firstSteps.isEmpty
            panel.RewindButton.enabled = false
            panel.FastForwardButton.enabled = !derivation.heads.isEmpty
            panel.LhsLabel.text = "(root)"
            panel.LhsView.graphRef = panel.document.rootRef
        }

        panel.LhsView.clearHighlights()
        panel.RhsView.clearHighlights()

        panel.rewriteController.listenTo(panel.LhsView)


      case StepState(step) =>
        panel.Rhs.setStepMode()
        panel.RhsLabel.text = step.toString
        panel.RhsView.graphRef = panel.document.stepRef(step)

        derivation.parent.get(step) match {
          case Some(parent) => // at a step with parent
            panel.RewindButton.enabled = true
            panel.PreviousButton.enabled = true
            panel.LhsLabel.text = parent.toString
            panel.LhsView.graphRef = panel.document.stepRef(parent)
          case None => // at a step with no parent (i.e. parent is root)
            panel.RewindButton.enabled = false
            panel.PreviousButton.enabled = false
            panel.LhsLabel.text = "(root)"
            panel.LhsView.graphRef = panel.document.rootRef
        }

        val lhsV = panel.LhsView.graphRef.graph.verts
        val rhsV = panel.RhsView.graphRef.graph.verts

        // highlight where the rule was applied
        panel.LhsView.clearHighlights()
        panel.LhsView.addHighlight(new Highlight(Color.RED, lhsV -- rhsV))

        panel.RhsView.clearHighlights()
        panel.RhsView.addHighlight(new Highlight(Color.BLUE, rhsV -- lhsV))

        panel.NextButton.enabled = true
        panel.FastForwardButton.enabled = true
    }

    publish(DeriveStateChanged(s))
  }

  def replaceDerivation(d: Derivation, desc: String) {
    val currentDerivation = panel.document.derivation
    panel.document.derivation = d
    // force state refresh
    state_=(state)

    panel.document.undoStack.register(desc) {
      replaceDerivation(currentDerivation, desc)
    }
  }

  def layoutDerivation() {
    val layoutProc = new DeriveLayout
    val d = layoutProc.layout(panel.document.derivation)
    replaceDerivation(d, "Layout Derivation")
  }

  panel.navigationButtons.foreach { listenTo(_) }
  listenTo(panel.document, panel.histView.selection)

  reactions += {
    case DocumentReplaced(_) =>
      state = HeadState(derivation.firstHead)
      panel.histView.treeData = derivation
      panel.histView.selectedNode = Some(state)
//      println(derivation)
//      println(derivation.toSeq)
    case ButtonClicked(panel.RewindButton) =>
      state = HeadState(None)
    case ButtonClicked(panel.FastForwardButton) =>
      state match {
        case StepState(s) =>
          val ff = derivation.fastForward(s)
          state = HeadState(Some(ff))
        case HeadState(None) => // ff from root goes to the first head
          derivation.firstHead.map { h => state = HeadState(Some(h)) }
        case HeadState(Some(_)) => // can't ff from named head
      }
    case ButtonClicked(panel.PreviousButton) =>
      state match {
        case HeadState(sOpt) => sOpt.map { s => state = StepState(s) }
        case StepState(s) =>
          derivation.parent.get(s).map { p => state = StepState(p) }
      }
    case ButtonClicked(panel.NextButton) =>
      state match {
        case StepState(s) =>
          if (derivation.isHead(s)) state = HeadState(Some(s))
          else derivation.children(s).headOption.map { ch => state = StepState(ch) }
        case HeadState(None) =>
          derivation.firstSteps.headOption.map { ch => state = StepState(ch) }
        case HeadState(Some(_)) => // do nothing
      }
    case SelectionChanged(_) =>
      if (panel.histView.selectedNode != Some(state))
        panel.histView.selectedNode.map { st => state = st }
  }
}
