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


class SimplifyController(panel: DerivationPanel) extends Publisher {
  implicit val timeout = QuantoDerive.timeout
  private var simpId = 0 // incrementing the simpId will (lazily) cancel any pending simplification jobs

  listenTo(panel.SimplifyPane.RefreshButton,
    panel.SimplifyPane.SimplifyButton,
    panel.SimplifyPane.StopButton,
    panel.SimplifyPane.AnnealButton,
    panel.SimplifyPane.GreedyButton)

  def theory = panel.theory

  def refreshSimprocs() {
    simpId += 1
//    val res = QuantoDerive.core ? Call(theory.coreName, "simplify", "list")
//    res.map {
//      case Success(JsonArray(procs)) =>
//        Swing.onEDT { panel.SimplifyPane.Simprocs.listData = procs.map(_.stringValue) }
//      case r => println("ERROR: Unexpected result from core: " + r) // TODO: errror dialogs
//    }
  }

  refreshSimprocs()

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

  implicit def ruleFromDesc(ruleDesc: RuleDesc) : Rule = {
    Rule.fromJson(Json.parse(new File(panel.project.rootFolder + "/" + ruleDesc.name + ".qrule")), theory)
  }

  private def annealSimproc() : Unit = {
    val reducedDerivation = genericReduce((panel.derivation, panel.controller.state.step),
      panel.rewriteController.rules.map(ruleFromDesc).toList)._1
    panel.document.derivation = reducedDerivation
  }

  private def greedySimproc() : Unit = {
    val reducedDerivation = greedyReduce((panel.derivation, panel.controller.state.step),
      panel.rewriteController.rules.map(ruleFromDesc).toList)._1
    panel.document.derivation = reducedDerivation
  }

  reactions += {
    case ButtonClicked(panel.SimplifyPane.RefreshButton) => refreshSimprocs()
    case ButtonClicked(panel.SimplifyPane.AnnealButton) => annealSimproc()
    case ButtonClicked(panel.SimplifyPane.GreedyButton) => greedySimproc()
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
      Swing.onEDT { QuantoDerive.ConsoleProgress.indeterminate = false }
      simpId += 1
  }

}
