package quanto.gui

import quanto.data.DSName
import scala.swing._
import scala.swing.event.{SelectionChanged, ButtonClicked, Event}
import quanto.gui.histview.{HistView, HistNode}
import java.awt.Color

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
    _state = s
    panel.histView.selectedNode = Some(s)
    publish(DeriveStateChanged(s))

    s match {
      case HeadState(headOpt) =>
        panel.Rhs.setHeadMode()
        panel.RhsLabel.text = "(head)"

        headOpt match {
          case Some(head) => // at a named head
            panel.PreviousButton.enabled = true
            panel.RewindButton.enabled = true
            panel.FastForwardButton.enabled = false
            panel.LhsLabel.text = head.toString
          case None => // at the root
            panel.PreviousButton.enabled = false
            panel.RewindButton.enabled = false
            panel.FastForwardButton.enabled = !derivation.heads.isEmpty
            panel.LhsLabel.text = "(root)"
        }

        panel.NextButton.enabled = false


      case StepState(step) =>
        panel.Rhs.setStepMode()
        panel.RhsLabel.text = step.toString

        derivation.parent.get(step) match {
          case Some(parent) => // at a step with parent
            panel.RewindButton.enabled = true
            panel.PreviousButton.enabled = true
            panel.LhsLabel.text = parent.toString
          case None => // at a step with no parent (i.e. parent is root)
            panel.RewindButton.enabled = false
            panel.PreviousButton.enabled = false
            panel.LhsLabel.text = "(root)"
        }

        panel.NextButton.enabled = true
        panel.FastForwardButton.enabled = true
    }
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
