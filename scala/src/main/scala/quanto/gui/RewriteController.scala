package quanto.gui

import quanto.data._
import quanto.data.Names._
import scala.concurrent.Lock
import scala.swing._
import scala.swing.event._
import scala.swing.event.ButtonClicked
import quanto.core.{Success, Call}
import quanto.util.json._
import akka.pattern.ask
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.File
import quanto.layout.ForceLayout


class RewriteController(panel: DerivationPanel) extends Publisher {
  implicit val timeout = QuantoDerive.timeout
  var queryId = 0
  val resultLock = new Lock
  var resultSet = ResultSet(Vector())
  def theory = panel.project.theory

  class ResultGraphRef(rule: RuleDesc, i: Int) extends HasGraph {
    protected def gr_=(g: Graph) {
      resultLock.acquire()
      resultSet = resultSet.replaceGraph(rule, i, g)
      resultLock.release()
    }

    protected def gr = resultSet.graph(rule, i)
  }

  def rules = resultSet.rules
  def rules_=(rules: Vector[RuleDesc]) {
    resultLock.acquire()
    resultSet = ResultSet(rules)
    queryId += 1

    //for (i <- 0 to rules.length - 1) new DummyRuleSeach(i, queryId).start()
    val sel = if (!panel.LhsView.selectedVerts.isEmpty) panel.LhsView.selectedVerts
              else panel.LhsView.graph.verts

    for (rd <- rules) {
      val rule = Rule.fromJson(Json.parse(new File(panel.project.rootFolder + "/" + rd.name + ".qrule")), theory)
      val resp = QuantoDerive.core ? Call(theory.coreName, "rewrite", "find_rewrites",
        JsonObject(
          "rule" -> Rule.toJson(if (rd.inverse) rule.inverse else rule, theory),
          "graph" -> Graph.toJson(panel.LhsView.graph, theory),
          "vertices" -> JsonArray(sel.toVector.map(v => JsonString(v.toString)))
        ))
      resp.map { case Success(JsonString(stack)) => pullRewrite(queryId, rd, stack); case _ => }
    }

    resultLock.release()
    
    refreshRewriteDisplay(clearSelection = true)
  }

  def restartSearch() { rules = rules }

  private def pullRewrite(qid: Int, rd: RuleDesc, stack: String) {
    val resp = QuantoDerive.core ? Call(panel.project.theory.coreName, "rewrite", "pull_rewrite",
      JsonObject("stack" -> JsonString(stack)))
    resp.map {
      case Success(obj : JsonObject) =>
        // layout the graph before acquiring the lock, so many can be done in parallel
        val step = DStep(
          name = DSName("s"),
          ruleName = rd.name,
          rule = Rule.fromJson(obj / "rule", theory),
          variant = if (rd.inverse) RuleInverse else RuleNormal,
          graph = Graph.fromJson(obj / "graph", theory)).layout

        //println("found nodes: " + (rule.rhs.verts intersect graph.verts))

        resultLock.acquire()

        // make sure this rewrite query is still in progress, and the rule hasn't been manually removed by the user
        if (qid == queryId && resultSet.rules.contains(rd)) {
          resultSet += rd -> step

          if (resultSet.numResults(rd) < 50) pullRewrite(qid, rd, stack)
          else QuantoDerive.core ! Call(theory.coreName, "rewrite", "delete_rewrite_stack",
            JsonObject("stack" -> JsonString(stack)))
        } else {
          // clean up if this rule has been removed from the result list, or if this rewrite query has expired
          QuantoDerive.core ! Call(theory.coreName, "rewrite", "delete_rewrite_stack",
            JsonObject("stack" -> JsonString(stack)))
        }

        resultLock.release()
        refreshRewriteDisplay()

      case Success(JsonNull) =>
      case _ =>
    }
  }


  def refreshRewriteDisplay(clearSelection: Boolean = false) {
    Swing.onEDT {
      resultLock.acquire()

      if (clearSelection) {
        panel.ManualRewritePane.PreviousResultButton.enabled = false
        panel.ManualRewritePane.NextResultButton.enabled = false
        panel.ManualRewritePane.ApplyButton.enabled = false
        panel.ManualRewritePane.Rewrites.selection.indices.clear()
        panel.ManualRewritePane.Rewrites.listData = resultSet.resultLines
      } else {
        val sel = panel.ManualRewritePane.Rewrites.selection.items.seq.map(res => res.rule).toSet
        panel.ManualRewritePane.Rewrites.listData = resultSet.resultLines
        for (i <- 0 to panel.ManualRewritePane.Rewrites.listData.length - 1) {
          if (sel.contains(panel.ManualRewritePane.Rewrites.listData(i).rule))
            panel.ManualRewritePane.Rewrites.selection.indices += i
        }
      }

      resultLock.release()
    }
  }

  def selectedRule =
    if (panel.ManualRewritePane.Rewrites.selection.items.length == 1)
      Some(panel.ManualRewritePane.Rewrites.selection.items(0).asInstanceOf[ResultLine].rule)
    else None

  listenTo(panel.ManualRewritePane.AddRuleButton, panel.ManualRewritePane.RemoveRuleButton)
  listenTo(panel.ManualRewritePane.PreviousResultButton, panel.ManualRewritePane.NextResultButton)
  listenTo(panel.ManualRewritePane.ApplyButton)
  listenTo(panel.ManualRewritePane.Rewrites.selection)

  reactions += {
    case ButtonClicked(panel.ManualRewritePane.AddRuleButton) =>
      val d = new AddRuleDialog(panel.project)
      d.centerOnScreen()
      d.open()

      val currentRules = rules.toSet
      val newRules = d.result.filter(!currentRules.contains(_))

      if (!newRules.isEmpty) rules ++= newRules
    case ButtonClicked(panel.ManualRewritePane.RemoveRuleButton) =>
      resultLock.acquire()
      panel.ManualRewritePane.Rewrites.selection.items.foreach { line => resultSet -= line.rule }
      resultLock.release()
      refreshRewriteDisplay()
    case ButtonClicked(panel.ManualRewritePane.PreviousResultButton) =>
      resultLock.acquire()
      selectedRule match {
        case Some(rd) =>
          resultSet = resultSet.previousResult(rd)
          panel.ManualRewritePane.Preview.graphRef = resultSet.resultIndex(rd) match
            { case 0 => panel.DummyRef ; case i => new ResultGraphRef(rd, i) }
        case None =>
      }
      resultLock.release()
      refreshRewriteDisplay()
    case ButtonClicked(panel.ManualRewritePane.NextResultButton) =>
      resultLock.acquire()
      selectedRule match {
        case Some(rd) =>
          resultSet = resultSet.nextResult(rd)
          panel.ManualRewritePane.Preview.graphRef = resultSet.resultIndex(rd) match
            { case 0 => panel.DummyRef ; case i => new ResultGraphRef(rd, i) }
        case None =>
      }
      resultLock.release()
      refreshRewriteDisplay()
    case ButtonClicked(panel.ManualRewritePane.ApplyButton) =>
      selectedRule.map { rd => resultSet.currentResult(rd).map { step =>
        val parentOpt = panel.controller.state.step

        val stepFr = step.copy(name = panel.derivation.steps.freshWithSuggestion(DSName(rd.name.replaceFirst("^.*\\/", "") + "-0")))
        panel.ManualRewritePane.Preview.graphRef = panel.DummyRef

        panel.document.undoStack.start("Apply rewrite")
        panel.controller.replaceDerivation(panel.derivation.addStep(parentOpt, stepFr), "")
        panel.controller.state = HeadState(Some(stepFr.name))
        panel.document.undoStack.commit()
      }}

    case VertexSelectionChanged(_,_) =>
      restartSearch()
    case SelectionChanged(_) =>
      selectedRule match {
        case Some(rd) =>
          panel.ManualRewritePane.Preview.graphRef = resultSet.resultIndex(rd) match
            { case 0 => panel.DummyRef ; case i => new ResultGraphRef(rd, i) }
          panel.ManualRewritePane.PreviousResultButton.enabled = true
          panel.ManualRewritePane.NextResultButton.enabled = true
          panel.ManualRewritePane.ApplyButton.enabled = true
        case None =>
          panel.ManualRewritePane.Preview.graphRef = panel.DummyRef
          panel.ManualRewritePane.PreviousResultButton.enabled = false
          panel.ManualRewritePane.NextResultButton.enabled = false
          panel.ManualRewritePane.ApplyButton.enabled = false
      }
  }

//  class DummyRuleSeach(i : Int, qid: Int) extends Thread {
//    override def run() {
//      Thread.sleep(math.abs(new Random().nextInt()) % 200)
//      Swing.onEDT {
//        if (queryId == qid) {
//          resultLock.acquire()
//          val r = resultSet.rules(i)
//          if (resultSet.numResults(r) < 20) {
//            resultSet +=
//              r -> DStep(
//                name = DSName("s"),
//                ruleName = r.name,
//                rule = Rule(lhs = Graph(), rhs = Graph()),
//                variant = RuleNormal,
//                graph = Graph())
//
//            refreshRewriteDisplay()
//            new DummyRuleSeach(i, qid).start()
//          }
//          resultLock.release()
//        }
//      }
//    }
//  }
}
