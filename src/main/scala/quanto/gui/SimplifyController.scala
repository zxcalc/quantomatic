package quanto.gui

import scala.concurrent.Future
import scala.swing._
import quanto.data._
import quanto.data.Names._
import quanto.util.json._
import akka.pattern.ask
import quanto.util.UserAlerts

import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing.event.{ButtonClicked, Event}
import scala.util.{Failure, Success, Try}
import quanto.util.UserAlerts.SelfAlertingProcess


class SimplifyController(panel: DerivationPanel) extends Publisher {
  implicit val timeout = QuantoDerive.timeout
  private var simpId = 0
  private var activeSimp: Option[Future[Boolean]] = None



  case class StartSimproc(simproc: String) extends Event
  case class RefreshSimprocs() extends Event
  case class ReRunSimproc() extends Event
  case class HaltSimproc() extends Event
  case class SimprocHaltedWhileRunning() extends Exception("Simproc halted")

  listenTo(this,
    panel.SimplifyPane.RefreshButton,
    panel.SimplifyPane.SimplifyButton,
    panel.SimplifyPane.StopButton,
    panel,
    PythonEditPanel)

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

  private var _lastRunSimproc : String = ""

  reactions += {
    case RequestReRunSimproc() =>
      publish(ReRunSimproc())
    case ReRunSimproc() =>
      publish(StartSimproc(_lastRunSimproc))
    case StartSimproc(simpName) =>
      _lastRunSimproc = simpName
      QuantoDerive.CurrentProject.flatMap { pr => pr.simprocs.get(simpName) }.foreach { simproc =>
        var parentOpt = panel.controller.state.step
        val sourceMessage = s"Running simproc '$simpName'".concat(
        if (simproc.sourceFile != "") {
          s" from ${simproc.sourceFile}"
        } else {
          ""
        }
        )
        UserAlerts.alert(sourceMessage)
        val processReporting = new SelfAlertingProcess("Simproc: " + simpName)
        val simpIdAtStart = simpId
        val res = Future[Boolean] {
          val iteratedSimp : Iterator[(Graph, Rule)] = simproc.simp(panel.LhsView.graph)

          // Don't update the derivation if the simpId call has changed
          while(iteratedSimp.hasNext && simpId == simpIdAtStart){
            val (graph, rule) = iteratedSimp.next()
            val suggest = simpName + "-" + rule.name.replaceFirst("^.*\\/", "") + "-0"
            val step = DStep(
              name = panel.derivation.steps.freshWithSuggestion(DSName(suggest)),
              rule = rule,
              graph = graph.minimise) // layout is already done by simproc now

              panel.document.derivation = panel.document.derivation.addStep(parentOpt, step)
              parentOpt = Some(step.name)
              Swing.onEDT {
                panel.controller.state = HeadState(Some(step.name))
              }
          }


          if(simpId != simpIdAtStart) {
            throw SimprocHaltedWhileRunning()
          }

          true
        }

        res.onComplete {
          case Success(b) =>
            processReporting.finish()
          case Failure(SimprocHaltedWhileRunning()) =>
            processReporting.halt()
          case Failure(e) =>
            e.printStackTrace()
        }
      }
    case HaltSimproc() =>
      simpId += 1
    case RefreshSimprocs() =>
      refreshSimprocs()
    case ButtonClicked(panel.SimplifyPane.RefreshButton) =>
      publish(RefreshSimprocs())
    case ButtonClicked(panel.SimplifyPane.SimplifyButton) =>
      if (panel.SimplifyPane.Simprocs.selection.indices.nonEmpty) {
        simpId += 1
        val simpName = panel.SimplifyPane.Simprocs.selection.items(0)
        publish(StartSimproc(simpName))
      }
    case ButtonClicked(panel.SimplifyPane.StopButton) =>
      publish(HaltSimproc())
    case SimprocsUpdated() =>
      refreshSimprocs()
  }

}
