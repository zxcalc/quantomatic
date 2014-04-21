package quanto.gui

import quanto.data.{Derivation, DSName}
import scala.swing._
import scala.swing.event.{SelectionChanged, ButtonClicked, Event}
import quanto.gui.histview._
import java.awt.Color
import quanto.gui.graphview.Highlight
import quanto.layout.DeriveLayout

sealed abstract class DeriveState extends HistNode { def step: Option[DSName] }
case class StepState(s: DSName) extends DeriveState {
  def color = new Color(180,255,180)
  def label = s.toString
  def step = Some(s)
}

case class HeadState(hOpt: Option[DSName]) extends DeriveState {
  def color = Color.WHITE
  def label = if (hOpt.isEmpty) "(root)" else "(head)"
  def step = hOpt
}

case class DeriveStateChanged(state: DeriveState) extends Event

class DerivationController(panel: DerivationPanel) extends Publisher {
  def derivation = panel.derivation

  // move to a named step in the derivation
  private var _state : DeriveState = HeadState(None)
  def state = _state
  def state_=(s: DeriveState) {
    if (_state != s) {
      val oldState = _state
      panel.document.undoStack.register("Change proof step") { state = oldState }
    }

    _state match {
      case HeadState(_) => panel.rewriteController.deafTo(panel.LhsView)
      case _ =>
    }

    _state = s
    panel.histView.selectedNode = Some(s)
    //panel.document.undoStack.clear() // weird things can happen if we keep the old undo stack around

    panel.PreviousButton.enabled = false
    panel.NextButton.enabled = false
    panel.RewindButton.enabled = false
    panel.FastForwardButton.enabled = false
    panel.NewHeadButton.enabled = false
    panel.DeleteStepButton.enabled = false

    s match {
      case HeadState(headOpt) =>
        panel.Rhs.setHeadMode()
        panel.RhsLabel.text = "(head)"
        panel.RhsView.graphRef = panel.DummyRef

        headOpt match {
          case Some(head) => // at a named head
            panel.PreviousButton.enabled = true
            panel.RewindButton.enabled = true
            panel.DeleteStepButton.enabled = derivation.hasChildren(head)
            panel.LhsLabel.text = head.toString
            panel.LhsView.graphRef = panel.document.stepRef(head)
          case None => // at the root
            panel.NextButton.enabled = !derivation.firstSteps.isEmpty
            panel.FastForwardButton.enabled = !derivation.heads.isEmpty
            panel.LhsLabel.text = "(root)"
            panel.LhsView.graphRef = panel.document.rootRef
        }

        panel.LhsView.clearHighlights()
        panel.RhsView.clearHighlights()

        panel.rewriteController.listenTo(panel.LhsView)
        panel.rewriteController.restartSearch()


      case StepState(step) =>
        panel.NewHeadButton.enabled = !derivation.isHead(step)
        panel.DeleteStepButton.enabled = true
        panel.Rhs.setStepMode()
        panel.RhsLabel.text = step.toString
        panel.RhsView.graphRef = panel.document.stepRef(step)

        derivation.parentMap.get(step) match {
          case Some(parent) => // at a step with parent
            panel.RewindButton.enabled = true
            panel.PreviousButton.enabled = true
            panel.LhsLabel.text = parent.toString
            panel.LhsView.graphRef = panel.document.stepRef(parent)
          case None => // at a step with no parent (i.e. parent is root)
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

  panel.derivationButtons.foreach { listenTo(_) }
  listenTo(panel.document, panel.histView.selection)

  reactions += {
    case DocumentChanged(_) =>
      panel.histView.treeData = derivation
      panel.histView.selectedNode = Some(state)
      panel.histView.ensureIndexIsVisible(panel.histView.selection.leadIndex)
    case DocumentReplaced(_) =>
      _state = HeadState(derivation.firstHead)
      state = state
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
          derivation.parentMap.get(s).map { p => state = StepState(p) }
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
    case ButtonClicked(panel.NewHeadButton) =>
      state match {
        case StepState(s) =>
          panel.document.derivation = derivation.addHead(s)
          state = HeadState(Some(s))
        case _ => // do nothing
      }

    case ButtonClicked(panel.DeleteStepButton) =>
      state match {
        case HeadState(Some(s)) =>
          panel.document.derivation = derivation.deleteHead(s)
          state = StepState(s)
        case StepState(s) =>
          // TODO: make deletion undo-able?
          if (Dialog.showConfirmation(
                title = "Confirm deletion",
                message = "This will delete " + derivation.allChildren(s).size +
                  " proof steps, and cannot be\nundone. Do you wish to continue?")
              == Dialog.Result.Yes)
          {
            val parentOpt = derivation.parentMap.get(s)
            panel.document.derivation = derivation.deleteStep(s)
            state = HeadState(parentOpt)
          }
        case _ => // do nothing on root
      }
    case SelectionChanged(_) =>
      if (panel.histView.selectedNode != Some(state))
        panel.histView.selectedNode.map { st => state = st }
  }
}
