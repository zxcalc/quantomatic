package quanto.gui

import quanto.data._
import scala.concurrent.Lock
import scala.swing._
import scala.util.Random
import scala.swing.event.ButtonClicked
import quanto.core.{Success, Call}
import quanto.util.json._
import akka.pattern.ask
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.File


class RewriteController(panel: DerivationPanel) extends Publisher {
  implicit val timeout = QuantoDerive.timeout
  var queryId = 0
  val resultLock = new Lock
  var resultSet = ResultSet(Vector())
  def theory = panel.project.theory

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
      resp.map { case Success(JsonString(stack)) => pullRewrite(rd, stack); case _ => }
    }

    resultLock.release()
    
    refreshRewriteDisplay()
  }

  def restartSearch() { rules = rules }

  private def pullRewrite(rd: RuleDesc, stack: String) {
    val resp = QuantoDerive.core ? Call(panel.project.theory.coreName, "rewrite", "pull_rewrite",
      JsonObject("stack" -> JsonString(stack)))
    resp.map {
      case Success(obj : JsonObject) =>
        resultLock.acquire()
        resultSet +=
          rd -> DStep(
            name = DSName("s"),
            ruleName = rd.name,
            rule = Rule.fromJson(obj / "rule", theory),
            variant = if (rd.inverse) RuleInverse else RuleNormal,
            graph = Graph.fromJson(obj / "graph", theory))
        resultLock.release()

        Swing.onEDT { refreshRewriteDisplay() }

        if (resultSet.numResults(rd) < 20) pullRewrite(rd, stack)
        else QuantoDerive.core ! Call(theory.coreName, "rewrite", "delete_rewrite_stack",
                                      JsonObject("stack" -> JsonString(stack)))
      case Success(JsonNull) => println("no more rewrites for stack " + stack)
      case _ =>
    }
  }


  def refreshRewriteDisplay() {
    Swing.onEDT {
      resultLock.acquire()
      
      val sel = panel.RewriteList.selection.items.seq.map(res => res.rule).toSet

      panel.RewriteList.listData = resultSet.resultLines
      for (i <- 0 to panel.RewriteList.listData.length - 1) {
        if (sel.contains(panel.RewriteList.listData(i).rule))
          panel.RewriteList.selection.indices += i
      }
      
      resultLock.release()
    }
  }

  listenTo(panel.ManualRewritePane.AddRuleButton)

  reactions += {
    case ButtonClicked(panel.ManualRewritePane.AddRuleButton) =>
      val d = new AddRuleDialog(panel.project)
      d.centerOnScreen()
      d.open()

      if (!d.result.isEmpty) rules ++= d.result
    case VertexSelectionChanged(_,_) =>
      restartSearch()
  }

  class DummyRuleSeach(i : Int, qid: Int) extends Thread {
    override def run() {
      Thread.sleep(math.abs(new Random().nextInt()) % 200)
      Swing.onEDT {
        if (queryId == qid) {
          resultLock.acquire()
          val r = resultSet.rules(i)
          if (resultSet.numResults(r) < 20) {
            resultSet +=
              r -> DStep(
                name = DSName("s"),
                ruleName = r.name,
                rule = Rule(lhs = Graph(), rhs = Graph()),
                variant = RuleNormal,
                graph = Graph())

            refreshRewriteDisplay()
            new DummyRuleSeach(i, qid).start()
          }
          resultLock.release()
        }
      }
    }
  }
}
