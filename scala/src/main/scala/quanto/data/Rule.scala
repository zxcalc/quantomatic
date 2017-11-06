package quanto.data

import quanto.util.json._

trait RuleException

case class RuleLoadException(message: String, cause: Throwable = null)
  extends Exception(message, cause)
    with RuleException

case class Rule(private val _lhs: Graph,
                private val _rhs: Graph,
                derivation: Option[String] = None,
                description: RuleDesc = RuleDesc("unnamed")) {

  def inverse: Rule = {
    Rule(lhs, rhs, derivation, description.invert)
  }

  val lhs: Graph = if (description.inverse) _rhs else _lhs

  val rhs: Graph = if (description.inverse) _lhs else _rhs

  val name: String = description.name + (if (description.inverse) " inverted" else "")

  override def toString: String = name + " := "+ _lhs.toString +
    (if (description.inverse) {
      "<--"
    } else {
      "-->"
    }) +
    _rhs.toString
}

case class RuleDesc(name: String = "unnamed", inverse: Boolean = false) {
  def invert: RuleDesc = RuleDesc(name, !inverse)
}

object Rule {
  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory, description: Option[RuleDesc] = None): Rule = try {
    Rule(_lhs = Graph.fromJson(json / "lhs", thy),
      _rhs = Graph.fromJson(json / "rhs", thy),
      derivation = json.get("derivation") match {
        case Some(JsonString(s)) => Some(s);
        case _ => None
      },
      description = if (description.isDefined) description.get else json.get("description") match {
        case Some(JsonString(s)) => RuleDesc(s);
        case _ => RuleDesc("unnamed")
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