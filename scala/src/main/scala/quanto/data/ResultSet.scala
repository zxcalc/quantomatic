package quanto.data

case class ResultLine(rule: RuleDesc, index: Int, total: Int) {
  override def toString = {
    rule.name + (if (rule.inverse) "[inverse]" else "") + " (" + index + "/" + total + ")"
  }
}

case class ResultSet(rules: Vector[RuleDesc], results: Map[RuleDesc,(Int,Vector[DStep])]) {
  def copy(rules: Vector[RuleDesc] = rules, results: Map[RuleDesc,(Int,Vector[DStep])] = results) =
    ResultSet(rules, results)

  def currentResult(rule: RuleDesc): Option[DStep] = results(rule) match {
    case (i, vec) if i > 0 => Some(vec(i - 1))
    case _ => None
  }

  def resultIndex(rule: RuleDesc) = results(rule)._1
  def setResultIndex(rule: RuleDesc, i: Int) =
    copy(results = results + (rule -> (results(rule) match { case (_, vec) => (i, vec) })))

  def numResults(rule: RuleDesc) = results(rule)._2.size
  def +(res: (RuleDesc,DStep)) = {
    val rs = results(res._1)
    copy(results = results + (res._1 -> (if (rs._1 == 0) 1 else rs._1, rs._2 :+ res._2)))
  }

  def resultLines = rules.map { r => ResultLine(r, resultIndex(r), numResults(r)) }
}

object ResultSet {
  def apply(rules: Vector[RuleDesc]): ResultSet = {
    val results = rules.foldLeft(Map[RuleDesc,(Int,Vector[DStep])]()) {
      case (rs, rule) => rs + (rule -> (0, Vector())) }
    ResultSet(rules, results)
  }
}
