package quanto.gui

import quanto.data.DSName
import scala.swing._
import scala.swing.event.{ButtonClicked, Event}

sealed abstract class DeriveState
case class StepState(s: DSName) extends DeriveState
case class HeadState(h: Option[DSName]) extends DeriveState

case class DeriveStateChanged(state: DeriveState) extends Event

class DerivationController(panel: DerivationPanel) extends Publisher {
  def derivation = panel.derivation

  // move to a named step in the derivation
  private var _state : DeriveState = HeadState(None)
  def state = _state
  def state_=(s: DeriveState) {
    _state = s
    publish(DeriveStateChanged(s))

    s match {
      case HeadState(headOpt) =>
        panel.Rhs.setHeadMode()
        panel.RhsLabel.text = "(head)"

        headOpt match {
          case Some(head) => // at a named head
            panel.PreviousButton.enabled = true
            panel.RewindButton.enabled = true
            panel.LhsLabel.text = head.toString
          case None => // at the root
            panel.PreviousButton.enabled = false
            panel.RewindButton.enabled = false
            panel.LhsLabel.text = "(root)"
        }

        panel.NextButton.enabled = false
        panel.FastForwardButton.enabled = false

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

        panel.NextButton.enabled = (derivation.isHead(step) && !derivation.hasChildren(step)) ||
                                   (!derivation.isHead(step) && derivation.hasUniqueChild(step))
        panel.FastForwardButton.enabled = derivation.isHead(step) ||
                                          derivation.fastForward(step) != step
    }
  }

  panel.navigationButtons.foreach { listenTo(_) }
  listenTo(panel.document)

  reactions += {
    case DocumentReplaced(_) =>
      state = HeadState(derivation.firstHead)
    case ButtonClicked(panel.RewindButton) =>
      state match {
        case StepState(s) => state = StepState(derivation.rewind(s))
        case HeadState(Some(s)) => state = StepState(derivation.rewind(s))
        case HeadState(None) => // can't rewind from root
      }
    case ButtonClicked(panel.FastForwardButton) =>
      state match {
        case StepState(s) =>
          val ff = derivation.fastForward(s)
          state = if (derivation.isHead(ff)) HeadState(Some(ff)) else StepState(ff)
        case HeadState(_) => // can't ff from head
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
          if (derivation.isHead(s) && !derivation.hasChildren(s))
            state = HeadState(Some(s))
          else
            derivation.uniqueChild(s).map { ch => state = StepState(ch) }
        case HeadState(_) => // do nothing
      }
  }
}
