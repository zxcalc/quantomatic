package quanto.gui

import java.io.File

import scala.swing._
import quanto.core._
import quanto.data._
import quanto.data.Names._
import quanto.util.json._
import akka.pattern.ask
import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing.event.ButtonClicked
import quanto.cosy.AutoReduce._
import quanto.data.Derivation.DerivationWithHead

import scala.util.Random

//import quanto.cosy.AutoReduce
import quanto.cosy.ThreadedAutoReduce._


class SimplifyController(panel: DerivationPanel) extends Publisher {
  implicit val timeout = QuantoDerive.timeout
  private var simpId = 0 // incrementing the simpId will (lazily) cancel any pending simplification jobs

  listenTo(panel.SimplifyPane.RefreshButton,
    panel.SimplifyPane.SimplifyButton,
    panel.SimplifyPane.StopButton,
    panel.SimplifyPane.AnnealButton,
    panel.SimplifyPane.GreedyButton,
    panel.SimplifyPane.RandomButton,
    panel.SimplifyPane.LTEButton,
    panel.SimplifyPane.EvaluateButton)

  def refreshSimprocs() {
    simpId += 1
    //    val res = QuantoDerive.core ? Call(theory.coreName, "simplify", "list")
    //    res.map {
    //      case Success(JsonArray(procs)) =>
    //        Swing.onEDT { panel.SimplifyPane.Simprocs.listData = procs.map(_.stringValue) }
    //      case r => println("ERROR: Unexpected result from core: " + r) // TODO: errror dialogs
    //    }
  }

  def theory = panel.theory

  private def pullSimp(simproc: String, sid: Int, stack: String, parentOpt: Option[DSName]) {
    //    if (simpId == sid) {
    //      val res = QuantoDerive.core ? Call(theory.coreName, "simplify", "pull_next_step",
    //        JsonObject("stack" -> JsonString(stack)))
    //
    //      res.map {
    //        case Success(JsonNull) => // out of steps
    //          Swing.onEDT { QuantoDerive.ConsoleProgress.indeterminate = false }
    //          simpId += 1
    //        case Success(json) =>
    //          if (simpId == sid) {
    //            val suggest = simproc + "-" + (json / "rule_name").stringValue.replaceFirst("^.*\\/", "") + "-0"
    //            val sname = panel.derivation.steps.freshWithSuggestion(DSName(suggest))
    //            val step = DStep.fromJson(sname, json, theory).layout
    //
    //            Swing.onEDT {
    //              panel.document.derivation = panel.document.derivation.addStep(parentOpt, step)
    //              panel.controller.state = HeadState(Some(step.name))
    //              pullSimp(simproc, sid, stack, Some(sname))
    //            }
    //
    //          } else {
    //            Swing.onEDT { QuantoDerive.ConsoleProgress.indeterminate = false }
    //            QuantoDerive.core ! Call(theory.coreName, "simplify", "delete_stack",
    //              JsonObject("stack" -> JsonString(stack)))
    //          }
    //        case _ => println("ERROR: Unexpected result from core: " + res) // TODO: errror dialogs
    //      }
    //    } else {
    //      Swing.onEDT { QuantoDerive.ConsoleProgress.indeterminate = false }
    //      QuantoDerive.core ! Call(theory.coreName, "simplify", "delete_stack",
    //        JsonObject("stack" -> JsonString(stack)))
    //    }
  }

  private def evaluateSimproc() : Unit = {
    val d = new EvaluationInputPanel(panel.project)
    d.centerOnScreen()
    d.open()

    val targetString: String = d.TargetText.text.replaceAll(raw"\\",raw"\\\\")
    val replacementString : String = d.ReplacementText.text.replaceAll(raw"\\",raw"\\\\")
    val initialDerivation = (panel.derivation, panel.controller.state.step)
    if (targetString.length > 0) {

      val initialState: EvaluationInternalState = new EvaluationInternalState(
        List(),
        0,
        Some(initialDerivation.verts.size),
        new Random(),
        initialDerivation.verts.toList,
        targetString,
        replacementString,
        Some(initialDerivation.verts.size)
      )
      val simproc = new SimplificationProcedure[EvaluationInternalState](
        initialDerivation,
        initialState,
        evaluationStep,
        evaluationProgress,
        (der,state) => state.currentStep == state.maxSteps.get
      )
      val evluationProgressController = new SimprocProgress[EvaluationInternalState](
        panel.project,"Evaluation", simproc
      )
      evluationProgressController.centerOnScreen()
      evluationProgressController.open()
      updateDerivation(evluationProgressController.returningDerivation, "evaluation")
    }
  }

  private def annealSimproc(): Unit = {
    val d = new SimulatedAnnealingDialog(panel.project)
    d.centerOnScreen()
    d.open()

    val timeSteps = d.MainPanel.TimeSteps.text.toInt
    val vertexLimit = d.MainPanel.vertexLimit()
    if (timeSteps > 0) {

      val initialState : AnnealingInternalState = new AnnealingInternalState(allowedRules,0,Some(timeSteps),new Random(),3,vertexLimit)
      val simproc = new SimplificationProcedure[AnnealingInternalState](
        (panel.derivation, panel.controller.state.step),
        initialState,
        annealingStep,
        annealingProgress,
        (der,state) => state.currentStep == state.maxSteps.get
      )
      val simulatedAnnealingController = new SimprocProgress[AnnealingInternalState](
        panel.project,"Simulated Annealing", simproc
      )
      simulatedAnnealingController.centerOnScreen()
      simulatedAnnealingController.open()
      updateDerivation(simulatedAnnealingController.returningDerivation, "annealing reduce")
    }
  }

  refreshSimprocs()

  private def greedySimproc(): Unit = {

    val initialState: GreedyInternalState = new GreedyInternalState(
      allowedRules,
      0,
      None,
      new Random(),
      allowedRules,
      None)
    val simproc = new SimplificationProcedure[GreedyInternalState](
      (panel.derivation, panel.controller.state.step),
      initialState,
      greedyStep,
      greedyProgress,
      (der, state) => state.currentStep == state.maxSteps.getOrElse(-1) || state.remainingRules.isEmpty
    )
    val progressController = new SimprocProgress[GreedyInternalState](
      panel.project, "Greedy Reduction", simproc
    )
    progressController.centerOnScreen()
    progressController.open()
    updateDerivation(progressController.returningDerivation, "greedy reduce")
  }

  implicit def ruleFromDesc(ruleDesc: RuleDesc): Rule = {
    Rule.fromJson(Json.parse(new File(panel.project.rootFolder + "/" + ruleDesc.name + ".qrule")),
      theory,
      description = Some(ruleDesc))
  }

  private def allowedRules = panel.rewriteController.rules.map(ruleFromDesc).toList

  private def lteSimproc(): Unit = {
    val reducedDerivation = randomApply((panel.derivation, panel.controller.state.step),
      allowedRules, 100, (suggested, current) => suggested <= current)
    updateDerivation(reducedDerivation, "lte reduce")
  }

  private def randomSimproc(): Unit = {
    val reducedDerivation = randomApply((panel.derivation, panel.controller.state.step),
      allowedRules, 100)
    updateDerivation(reducedDerivation, "random reduce")
  }

  private def moveToStep(stepName: DSName): Unit = {
    panel.controller.state = StepState(stepName)
  }

  private def updateDerivation(derivationWithHead: DerivationWithHead, desc: String): Unit = {
    val currentDerivation = panel.document.derivation

    panel.document.undoStack.register(desc) {
      updateDerivation((currentDerivation, panel.controller.state.step), desc)
    }

    panel.document.derivation = derivationWithHead._1
    derivationWithHead._2 match {
      case Some(stepName) => panel.controller.state = StepState(stepName)
      case None => panel.controller.state = HeadState(None)
    }
  }


  reactions += {
    case ButtonClicked(panel.SimplifyPane.RefreshButton) => refreshSimprocs()
    case ButtonClicked(panel.SimplifyPane.AnnealButton) => annealSimproc()
    case ButtonClicked(panel.SimplifyPane.GreedyButton) => greedySimproc()
    case ButtonClicked(panel.SimplifyPane.RandomButton) => randomSimproc()
    case ButtonClicked(panel.SimplifyPane.LTEButton) => lteSimproc()
    case ButtonClicked(panel.SimplifyPane.EvaluateButton) => evaluateSimproc()
    case ButtonClicked(panel.SimplifyPane.SimplifyButton) =>
    //      if (!panel.SimplifyPane.Simprocs.selection.indices.isEmpty) {
    //        simpId += 1
    //        val simproc = panel.SimplifyPane.Simprocs.selection.items(0).asInstanceOf[String]
    //        val res = QuantoDerive.core ? Call(theory.coreName, "simplify", "simplify", JsonObject(
    //          "simproc" -> JsonString(simproc),
    //          "graph"   -> Graph.toJson(panel.LhsView.graph, theory)
    //        ))
    //        res.map {
    //          case Success(JsonString(stack)) =>
    //            Swing.onEDT {
    //              QuantoDerive.ConsoleProgress.indeterminate = true
    //              pullSimp(simproc, simpId, stack, panel.controller.state.step)
    //            }
    //
    //          case _ => println("ERROR: Unexpected result from core: " + res) // TODO: errror dialogs
    //        }
    //      }
    case ButtonClicked(panel.SimplifyPane.StopButton) =>
      Swing.onEDT {
        QuantoDerive.ConsoleProgress.indeterminate = false
      }
      simpId += 1
  }

}
