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
import quanto.cosy.SimplificationProcedure
import quanto.cosy.GraphAnalysis
import quanto.data.Derivation.DerivationWithHead

import scala.util.Random


class SimplifyBuiltInController(panel: DerivationPanel) extends Publisher {
  implicit val timeout = QuantoDerive.timeout
  private var simpId = 0 // incrementing the simpId will (lazily) cancel any pending simplification jobs

  listenTo(panel.SimplifyBuiltInPane.SimplifyButton)

  def refreshSimprocs() {
    simpId += 1
    //    val res = QuantoDerive.core ? Call(theory.coreName, "simplify", "list")
    //    res.map {
    //      case Success(JsonArray(procs)) =>
    //        Swing.onEDT { panel.SimplifyBuiltInPane.Simprocs.listData = procs.map(_.stringValue) }
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

  private def evaluateSimproc(): Unit = {
    val d = new EvaluationInputPanel(panel.project)
    d.centerOnScreen()
    d.open()

    val targetString: String = d.TargetText.text.replaceAll(raw"\\", raw"\\\\")
    val replacementString: String = d.ReplacementText.text.replaceAll(raw"\\", raw"\\\\")
    val initialDerivation = (panel.derivation, panel.controller.state.step)

    import quanto.cosy.SimplificationProcedure.Evaluation._

    if (targetString.length > 0) {

      val initialState: State = new State(
        List(),
        0,
        Some(initialDerivation.verts.size),
        new Random(),
        initialDerivation.verts.toList,
        targetString,
        replacementString,
        Some(initialDerivation.verts.size)
      )
      val simproc = new SimplificationProcedure[State](
        initialDerivation,
        initialState,
        step,
        progress,
        (der, state) => state.currentStep == state.maxSteps.get
      )
      val evluationProgressController = new SimprocProgress[State](
        panel.project, "Evaluation", simproc
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
import quanto.cosy.SimplificationProcedure.Annealing._
      val initialState: State = new State(allowedRules, 0, Some(timeSteps), new Random(), 3, vertexLimit)
      val simproc = new SimplificationProcedure[State](
        (panel.derivation, panel.controller.state.step),
        initialState,
        step,
        progress,
        (_, state) => state.currentStep == state.maxSteps.get
      )
      val simulatedAnnealingController = new SimprocProgress[State](
        panel.project, "Simulated Annealing", simproc
      )
      simulatedAnnealingController.centerOnScreen()
      simulatedAnnealingController.open()
      updateDerivation(simulatedAnnealingController.returningDerivation, "annealing reduce")
    }
  }

  refreshSimprocs()

  private def pullErrorsSimproc(): Unit = {
    val initialDerivation = (panel.derivation, panel.controller.state.step)
    val graph = Derivation.derivationHeadPairToGraph(initialDerivation)
    val boundaries = graph.verts.filter(v => graph.vdata(v).isBoundary)
    val d = new SimpleSelectionPanel(panel.project,
      "Select target boundaries:",
      boundaries.toList.sorted.map(_.toString))
    d.centerOnScreen()
    d.open()

    val targets = d.MainPanel.OptionList.selection.items.map(s => VName(s)).toList

    val e = new SimpleSelectionPanel(panel.project,
      "Select greedy rules:",
      allowedRules.map(_.description.name))
    e.centerOnScreen()
    e.open()

    val greedyRules = e.MainPanel.OptionList.selection.items.map(s => ruleFromDesc(RuleDesc(s))).toList



    println(targets)
    import quanto.cosy.SimplificationProcedure.PullErrors
    if (targets.nonEmpty && allowedRules.nonEmpty) {

      val initialState: PullErrors.State = PullErrors.State(
        allowedRules.filterNot(r => greedyRules.contains(r)),
        0,
        None,
        new Random(),
        PullErrors.errorsDistance(targets.toSet),
        greedyRules = Some(greedyRules),
        currentDistance = None,
        heldVertices = None,
        vertexLimit = None
      )


      val simplificationProcedure = new quanto.cosy.SimplificationProcedure[PullErrors.State](
        initialDerivation,
        initialState,
        PullErrors.step,
        PullErrors.progress,
        (_, state) => state.currentStep == state.maxSteps.getOrElse(-1) || state.currentDistance.getOrElse(2.0) < 1
      )
      val progressController = new SimprocProgress[PullErrors.State](
        panel.project, "Pull Errors Through", simplificationProcedure
      )
      progressController.centerOnScreen()
      progressController.open()
      updateDerivation(progressController.returningDerivation, "pull errors")
    }
  }


  private def pullSpecialsSimproc(): Unit = {
    val initialDerivation = (panel.derivation, panel.controller.state.step)
    val graph = Derivation.derivationHeadPairToGraph(initialDerivation)
    val boundaries = graph.verts.filter(v => graph.vdata(v).isBoundary)
    val d = new SimpleSelectionPanel(panel.project,
      "Select target boundaries:",
      boundaries.toList.sorted.map(_.toString))
    d.centerOnScreen()
    d.open()

    val e = new SimpleSelectionPanel(panel.project,
      "Select vertices to hold in place:",
      graph.verts.toList.sorted.map(_.toString))
    e.centerOnScreen()
    e.open()

    val targets = d.MainPanel.OptionList.selection.items.map(s => VName(s)).toList
    val specials = e.MainPanel.OptionList.selection.items.map(s => VName(s)).toList
    import quanto.cosy.SimplificationProcedure.LTEByWeight._
    println(targets)
    if (targets.nonEmpty) {
      val initialState: State = State(
        allowedRules,
        0,
        None,
        new Random(),
        quanto.cosy.GraphAnalysis.distanceSpecialFromEnds(specials)(targets),
        None,
        heldVertices = Some(specials.toSet),
        None
      )
      val simproc = new SimplificationProcedure[State](
        (panel.derivation, panel.controller.state.step),
        initialState,
        step,
        progress,
        (_, state) => state.currentStep == state.maxSteps.getOrElse(-1) || state.currentDistance.getOrElse(1) == 0
      )
      val progressController = new SimprocProgress[State](
        panel.project, "Pull Errors Through", simproc
      )
      progressController.centerOnScreen()
      progressController.open()
      updateDerivation(progressController.returningDerivation, "pull errors")
    }
  }

  private def greedySimproc(): Unit = {
    import quanto.cosy.SimplificationProcedure.Greedy._
    val initialState: State = new State(
      allowedRules,
      0,
      None,
      new Random(),
      allowedRules,
      None)
    val simplificationProcedure = new SimplificationProcedure[State](
      (panel.derivation, panel.controller.state.step),
      initialState,
      step,
      progress,
      (_, state) => state.currentStep == state.maxSteps.getOrElse(-1) || state.remainingRules.isEmpty
    )
    val progressController = new SimprocProgress[State](
      panel.project, "Greedy Reduction", simplificationProcedure
    )
    progressController.centerOnScreen()
    progressController.open()
    updateDerivation(progressController.returningDerivation, "greedy reduce")
  }

  private def lteSimproc(): Unit = {
    import quanto.cosy.SimplificationProcedure.LTEByWeight._
    val initialState: State = State(
      rules = allowedRules,
      currentStep = 0,
      currentDistance = None,
      maxSteps = Some(100),
      seed = new Random(),
      weightFunction = g => Some(g.verts.size + g.edges.size),
      heldVertices = None,
      vertexLimit = None)
    val simproc = new SimplificationProcedure[State](
      (panel.derivation, panel.controller.state.step),
      initialState,
      step,
      progress,
      (_, state) => state.currentStep == state.maxSteps.getOrElse(-1)
    )
    val progressController = new SimprocProgress[State](
      panel.project, "Greedy Reduction", simproc
    )
    progressController.centerOnScreen()
    progressController.open()
    updateDerivation(progressController.returningDerivation, "lte reduce")
  }


  private def randomSimproc(): Unit = {
    import quanto.cosy.SimplificationProcedure.LTEByWeight._
    val initialState: State = State(
      rules = allowedRules,
      currentStep = 0,
      currentDistance = None,
      maxSteps = Some(100),
      seed = new Random(),
      weightFunction = _ => None,
      heldVertices = None,
      vertexLimit = None)
    val simproc = new SimplificationProcedure[State](
      (panel.derivation, panel.controller.state.step),
      initialState,
      step,
      progress,
      (_, state) => state.currentStep == state.maxSteps.getOrElse(-1)
    )
    val progressController = new SimprocProgress[State](
      panel.project, "Random Rule Application", simproc
    )
    progressController.centerOnScreen()
    progressController.open()
    updateDerivation(progressController.returningDerivation, "random apply")
  }


  val availableProcedures: Map[String, () => Unit] = Map(
    "Random x 100" -> randomSimproc,
    "Graph shrink x 100" -> lteSimproc,
    "Greedy reduce" -> greedySimproc,
    "Pull specials" -> pullSpecialsSimproc,
    "Pull pi-errors" -> pullErrorsSimproc,
    "Anneal" -> annealSimproc,
    "Evaluate" -> evaluateSimproc
  )
  Swing.onEDT { panel.SimplifyBuiltInPane.Simprocs.listData = availableProcedures.keys.toSeq }

  implicit def ruleFromDesc(ruleDesc: RuleDesc): Rule = {
    Rule.fromJson(Json.parse(new File(panel.project.rootFolder + "/" + ruleDesc.name + ".qrule")),
      theory,
      description = Some(ruleDesc))
  }

  private def allowedRules = panel.rewriteController.rules.map(ruleFromDesc).toList


  private def updateDerivation(derivationWithHead: DerivationWithHead, desc: String): Unit = {
    println("updating derivation")
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
    case ButtonClicked(panel.SimplifyBuiltInPane.SimplifyButton) =>
        if (panel.SimplifyBuiltInPane.Simprocs.selection.indices.nonEmpty) {
          val procedureName: String = panel.SimplifyBuiltInPane.Simprocs.selection.items(0)
          val procedure = availableProcedures(procedureName)
          procedure.apply()
        }
  }

}
