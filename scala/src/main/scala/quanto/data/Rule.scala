package quanto.data

import quanto.util.json._

trait RuleException

case class RuleLoadException(message: String, cause: Throwable = null)
  extends Exception(message, cause)
    with RuleException

case class Rule(lhs: Graph, rhs: Graph, derivation: Option[String] = None, description: Option[RuleDesc] = None) {
  def inverse: Rule = {
    Rule(rhs, lhs, derivation, if (description.isDefined) Some(description.get.invert) else None)
  }

  override def toString: String = if (description.isDefined) {
    description.get.name + (if (description.get.inverse) " inverted" else "")
  } else {
    lhs.toString + " --> " + rhs.toString
  }
}

case class RuleDesc(name: String, inverse: Boolean = false) {
  def invert: RuleDesc = RuleDesc(name, !inverse)
}

object Rule {
  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory, description: Option[RuleDesc] = None): Rule = try {
    Rule(lhs = Graph.fromJson(json / "lhs", thy),
      rhs = Graph.fromJson(json / "rhs", thy),
      derivation = json.get("derivation") match {
        case Some(JsonString(s)) => Some(s);
        case _ => None
      },
      description = if (description.isDefined) description else json.get("description") match {
        case Some(JsonString(s)) => Some(RuleDesc(s));
        case _ => None
      })
  } catch {
    case e: JsonAccessException =>
      throw RuleLoadException(e.getMessage, e)
    case e: GraphLoadException =>
      throw RuleLoadException("Graph: " + e.getMessage, e)
    case e: Exception =>
      e.printStackTrace()
      throw RuleLoadException("Unexpected error reading JSON", e)
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