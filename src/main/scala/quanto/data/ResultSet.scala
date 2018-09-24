package quanto.data

case class ResultLine(rule: RuleDesc, index: Int, total: Int) {
  override def toString: String = {
    (if (rule.inverse) "<- " else "-> ") + rule.name + " (" + index + "/" + total + ")"
  }
}

case class ResultSet(rules: Vector[RuleDesc], results: Map[RuleDesc, (Int, Vector[DStep])]) {
  def currentResult(rule: RuleDesc): Option[DStep] =
    results.get(rule).flatMap { case (i, vec) => if (i == 0) None else Some(vec(i - 1)) }

  def nextResult(rule: RuleDesc): ResultSet = {
    val i = resultIndex(rule) + 1
    if (i <= numResults(rule)) setResultIndex(rule, i)
    else this
  }

  def setResultIndex(rule: RuleDesc, i: Int): ResultSet =
    copy(results = results + (rule -> (results(rule) match {
      case (_, vec) => (i, vec)
    })))

  def previousResult(rule: RuleDesc): ResultSet = {
    val i = resultIndex(rule) - 1
    if (i > 0) setResultIndex(rule, i)
    else this
  }

  def replaceGraph(rule: RuleDesc, i: Int, graph: Graph): ResultSet = {
    val x = results(rule)
    val res = x._2(i - 1).copy(graph = graph)
    copy(results = results + (rule -> (x._1, x._2.updated(i - 1, res))))
  }

  def graph(rule: RuleDesc, i: Int): Graph = results(rule)._2(i - 1).graph

  def +(res: (RuleDesc, DStep)): ResultSet = {
    val rs = results(res._1)
    copy(results = results + (res._1 -> (if (rs._1 == 0) 1 else rs._1, rs._2 :+ res._2)))
  }

  def -(rule: RuleDesc): ResultSet = {
    copy(rules = rules.filter(_ != rule), results = results - rule)
  }

  def copy(rules: Vector[RuleDesc] = rules, results: Map[RuleDesc, (Int, Vector[DStep])] = results) =
    ResultSet(rules, results)

  def resultLines: Vector[ResultLine] = rules.map { r => ResultLine(r, resultIndex(r), numResults(r)) }

  def resultIndex(rule: RuleDesc): Int = results(rule)._1

  def numResults(rule: RuleDesc): Int = results(rule)._2.size
}

object ResultSet {
  def apply(rules: Vector[RuleDesc]): ResultSet = {
    val results = rules.foldLeft(Map[RuleDesc, (Int, Vector[DStep])]()) {
      case (rs, rule) => rs + (rule -> (0, Vector()))
    }
    ResultSet(rules, results)
  }
}
