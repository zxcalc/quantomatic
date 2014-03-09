package quanto.data

import quanto.util.json._

trait DerivationException
case class DerivationLoadException(message: String, cause: Throwable = null)
  extends Exception(message, cause)
  with DerivationException

case class DStep(name: DSName,
                 parent: Option[DStep],
                 rule: String,
                 matchedVertices: Set[VName],
                 replacedVertices: Set[VName],
                 graph: Graph)

object DStep {
  def toJson(dstep: DStep, thy: Theory = Theory.DefaultTheory): Json = {
    JsonObject("name" -> dstep.name.toString)
  }
}

case class Derivation(theory: Theory, head: Graph, steps: Map[DSName,DStep])

object Derivation {
  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory) = try {
    Rule(lhs = Graph.fromJson(json / "lhs", thy),
      rhs = Graph.fromJson(json / "rhs", thy),
      derivation = json.get("derivation").map(_.stringValue))
  } catch {
    case e: Exception => throw new RuleLoadException("Error reading JSON", e)
  }

  def toJson(derive: Derivation, thy: Theory = Theory.DefaultTheory) = {
    val steps = derive.steps.map { case (k, v) => (k.toString, DStep.toJson(v,thy)) }
    JsonObject(
      "head" -> Graph.toJson(derive.head, thy),
      "steps" -> JsonObject(steps)
    ).noEmpty
  }
}
