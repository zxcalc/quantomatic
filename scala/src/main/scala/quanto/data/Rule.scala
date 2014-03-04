package quanto.data

import quanto.util.json._

trait RuleException
class RuleLoadException(message: String, cause: Throwable = null)
  extends Exception(message, cause)
  with RuleException

case class Rule(lhs: Graph, rhs:Graph)

object Rule {
  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory) = try {
    Rule(lhs = Graph.fromJson(json / "lhs", thy),
         rhs = Graph.fromJson(json / "rhs", thy))
  } catch {
    case e: Exception => throw new RuleLoadException("Error reading JSON", e)
  }

  def toJson(rule: Rule, thy: Theory = Theory.DefaultTheory) = {
    JsonObject("lhs" -> Graph.toJson(rule.lhs, thy),
               "rhs" -> Graph.toJson(rule.rhs, thy))
  }
}