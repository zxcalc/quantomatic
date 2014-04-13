package quanto.gui

import quanto.data._
import scala.concurrent.Lock
import scala.swing._
import scala.util.Random
import scala.swing.event.ButtonClicked


class RewriteController(panel: DerivationPanel) extends Publisher {
  var queryId = 0
  val resultLock = new Lock
  var resultSet = ResultSet(Vector())

  def rules = resultSet.rules
  def rules_=(rules: Vector[RuleDesc]) {
    resultLock.acquire()
    resultSet = ResultSet(rules)
    queryId += 1

    for (i <- 0 to rules.length - 1) new DummyRuleSeach(i, queryId).start()
    resultLock.release()
    
    refreshRewriteDisplay()
  }

  def restartSearch() { rules = rules }

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
