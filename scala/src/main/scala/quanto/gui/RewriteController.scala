package quanto.gui

import quanto.data._
import scala.concurrent.Lock
import scala.swing.Swing
import java.awt.event.{ActionEvent, ActionListener}
import scala.util.Random


class RewriteController(panel: DerivationPanel) {
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

  class DummyRuleSeach(i : Int, qid: Int) extends Thread {
    override def run() {
      Thread.sleep(math.abs(new Random().nextInt()) % 5000)
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
