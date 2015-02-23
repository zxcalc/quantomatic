package quanto.data

import quanto.util.json._

trait RuleException
case class RuleLoadException(message: String, cause: Throwable = null)
  extends Exception(message, cause)
  with RuleException

case class Rule(lhs: Graph, rhs:Graph, derivation: Option[String] = None) {
  def inverse = Rule(rhs, lhs, derivation)
}

case class RuleDesc(name: String, inverse: Boolean)

object Rule {
  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory) = try {
    Rule(lhs = Graph.fromJson(json / "lhs", thy),
         rhs = Graph.fromJson(json / "rhs", thy),
         derivation = json.get("derivation") match { case Some(JsonString(s)) => Some(s); case _ => None })
  } catch {
    case e: JsonAccessException =>
      throw new RuleLoadException(e.getMessage, e)
    case e: GraphLoadException =>
      throw new RuleLoadException("Graph: " + e.getMessage, e)
    case e: Exception =>
      e.printStackTrace()
      throw new RuleLoadException("Unexpected error reading JSON", e)
  }

  def toJson(rule: Rule, thy: Theory = Theory.DefaultTheory): Json = {
    val obj = JsonObject(
      "lhs" -> Graph.toJson(rule.lhs, thy),
      "rhs" -> Graph.toJson(rule.rhs, thy)
    )

    rule.derivation match {
      case Some(x) => obj + ("derivation" -> JsonString(x))
      case None => obj
    }
  }
}