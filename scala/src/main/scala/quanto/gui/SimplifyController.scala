package quanto.gui

import scala.concurrent.Future
import scala.swing._
import quanto.core._
import quanto.data._
import quanto.data.Names._
import quanto.util.json._
import akka.pattern.ask
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing.event.ButtonClicked


class SimplifyController(panel: DerivationPanel) extends Publisher {
  implicit val timeout = QuantoDerive.timeout
  private var simpId = 0
  private var activeSimp: Option[Future[Boolean]] = None


  listenTo(panel.SimplifyPane.RefreshButton, panel.SimplifyPane.SimplifyButton, panel.SimplifyPane.StopButton)

  def theory = panel.theory

  def refreshSimprocs() {
    simpId += 1
    Swing.onEDT {
      QuantoDerive.CurrentProject.foreach { p =>
        panel.SimplifyPane.Simprocs.listData = p.simprocs.keys.toSeq
      }
    }
  }

  refreshSimprocs()

  private def pullSimp(simproc: String, sid: Int, stack: String, parentOpt: Option[DSName]) {
    if (simpId == sid) {
      //val res = QuantoDerive.core ? Call(theory.coreName, "simplify", "pull_next_step",
      //  JsonObject("stack" -> JsonString(stack)))

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
      None
    } else {
//      Swing.onEDT { QuantoDerive.ConsoleProgress.indeterminate = false }
//      QuantoDerive.core ! Call(theory.coreName, "simplify", "delete_stack",
//        JsonObject("stack" -> JsonString(stack)))
      None
    }
  }

  reactions += {
    case ButtonClicked(panel.SimplifyPane.RefreshButton) => refreshSimprocs()
    case ButtonClicked(panel.SimplifyPane.SimplifyButton) =>
      if (panel.SimplifyPane.Simprocs.selection.indices.nonEmpty) {
        simpId += 1
        val simpName = panel.SimplifyPane.Simprocs.selection.items(0)

        QuantoDerive.CurrentProject.flatMap { pr => pr.simprocs.get(simpName) }.foreach { simproc =>
          var parentOpt = panel.controller.state.step
          QuantoDerive.ConsoleProgress.indeterminate = true

          val res = Future[Boolean] {
            for ((graph, rule) <- simproc.simp(panel.LhsView.graph)) {
              val suggest = simpName + "-" + rule.name.replaceFirst("^.*\\/", "") + "-0"
              val step = DStep(
                name = panel.derivation.steps.freshWithSuggestion(DSName(suggest)),
                rule = rule,
                graph = graph.minimise) // layout is already done by simproc now

              panel.document.derivation = panel.document.derivation.addStep(parentOpt, step)
              parentOpt = Some(step.name)

              Swing.onEDT { panel.controller.state = HeadState(Some(step.name)) }
            }

            true
          }

          res.onComplete(_ => QuantoDerive.ConsoleProgress.indeterminate = false)
        }


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
      }
    case ButtonClicked(panel.SimplifyPane.StopButton) =>
      Swing.onEDT { QuantoDerive.ConsoleProgress.indeterminate = false }
      simpId += 1
  }

}
