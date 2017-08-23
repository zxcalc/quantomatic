package quanto.gui

import java.io.File

import scala.swing._
import quanto.core._
import quanto.data._
import quanto.data.Names._
import quanto.util.json._
import akka.pattern.ask

import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing.event.ButtonClicked
import quanto.cosy.AutoReduce._
import quanto.data.Derivation.DerivationWithHead


class SimplifyController(panel: DerivationPanel) extends Publisher {
  implicit val timeout = QuantoDerive.timeout
  private var simpId = 0 // incrementing the simpId will (lazily) cancel any pending simplification jobs

  listenTo(panel.SimplifyPane.RefreshButton,
    panel.SimplifyPane.SimplifyButton,
    panel.SimplifyPane.StopButton,
    panel.SimplifyPane.AnnealButton,
    panel.SimplifyPane.GreedyButton,
    panel.SimplifyPane.RandomButton,
    panel.SimplifyPane.LTEButton)

  def refreshSimprocs() {
    simpId += 1
    //    val res = QuantoDerive.core ? Call(theory.coreName, "simplify", "list")
    //    res.map {
    //      case Success(JsonArray(procs)) =>
    //        Swing.onEDT { panel.SimplifyPane.Simprocs.listData = procs.map(_.stringValue) }
    //      case r => println("ERROR: Unexpected result from core: " + r) // TODO: errror dialogs
    //    }
  }

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
  def theory = panel.theory

  private def annealSimproc(): Unit = {
    val reducedDerivation = genericReduce((panel.derivation, panel.controller.state.step),
      allowedRules)
    updateDerivation(reducedDerivation, "anneal")
  }

  refreshSimprocs()

  private def greedySimproc(): Unit = {
    val reducedDerivation = greedyReduce((panel.derivation, panel.controller.state.step),
      allowedRules)
    updateDerivation(reducedDerivation, "greedy reduce")
  }

  implicit def ruleFromDesc(ruleDesc: RuleDesc): Rule = {
    Rule.fromJson(Json.parse(new File(panel.project.rootFolder + "/" + ruleDesc.name + ".qrule")), theory)
  }

  private def allowedRules = panel.rewriteController.rules.map(ruleFromDesc).toList

  private def lteSimproc(): Unit = {
    val reducedDerivation = randomApply((panel.derivation, panel.controller.state.step),
      allowedRules.filter(r => r.lhs > r.rhs), 100)
    updateDerivation(reducedDerivation, "lte reduce")
  }

  private def randomSimproc(): Unit = {
    val reducedDerivation = randomApply((panel.derivation, panel.controller.state.step),
      allowedRules, 100)
    updateDerivation(reducedDerivation, "random reduce")
  }

  private def moveToStep(stepName: DSName) : Unit = {
    panel.controller.state = StepState(stepName)
  }

  private def updateDerivation(derivationWithHead: DerivationWithHead, desc: String) : Unit = {
    val currentDerivation = panel.document.derivation

    panel.document.undoStack.register(desc) {
      updateDerivation((currentDerivation,panel.controller.state.step), desc)
    }

    panel.document.derivation = derivationWithHead._1
    derivationWithHead._2 match{
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
