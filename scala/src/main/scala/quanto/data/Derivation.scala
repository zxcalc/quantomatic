package quanto.data

import quanto.util.json._

trait DerivationException
case class DerivationLoadException(message: String, cause: Throwable = null)
  extends Exception(message, cause)
  with DerivationException

sealed abstract class RuleVariant
case object RuleNormal extends RuleVariant { override def toString = "normal" }
case object RuleInverse extends RuleVariant { override def toString = "inverse" }

case class DStep(name: DSName,
                 parent: Option[DSName],
                 rule: String,
                 variant: RuleVariant,
                 matchedVertices: Set[VName],
                 replacedVertices: Set[VName],
                 graph: Graph)

object DStep {
  def toJson(dstep: DStep, thy: Theory = Theory.DefaultTheory): Json = {
    JsonObject(
      "name" -> dstep.name,
      "parent" -> dstep.parent,
      "rule" -> dstep.rule,
      "rule_variant" -> (dstep.variant match { case RuleNormal => JsonNull; case v => v.toString }),
      "matched" -> JsonObject("vertices" -> dstep.matchedVertices),
      "replaced" -> JsonObject("vertices" -> dstep.replacedVertices),
      "graph" -> Graph.toJson(dstep.graph, thy)
    ).noEmpty
  }

  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory) : DStep = try {
    DStep(
      name = DSName((json / "name").stringValue),
      parent = json.get("parent").map { p => DSName(p.stringValue) },
      rule = (json / "rule").stringValue,
      variant = json ? "rule_variant" match { case JsonString("inverse") => RuleInverse; case _ => RuleNormal },
      matchedVertices = (json / "matched" / "vertices").vectorValue.map(v => VName(v.stringValue)).toSet,
      replacedVertices = null,
      graph = null
    )
  } catch {
    case e: Exception => throw new DerivationLoadException("Error reading JSON")
  }
}

case class Derivation(theory: Theory, head: Graph, steps: Map[DSName,DStep])

object Derivation {
  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory) = try {
    Rule(lhs = Graph.fromJson(json / "lhs", thy),
      rhs = Graph.fromJson(json / "rhs", thy),
      derivation = json.get("derivation").map(_.stringValue))
  } catch {
    case e: Exception => throw new DerivationLoadException("Error reading JSON", e)
  }

  def toJson(derive: Derivation, thy: Theory = Theory.DefaultTheory) = {
    val steps = derive.steps.map { case (k, v) => (k.toString, DStep.toJson(v,thy)) }
    JsonObject(
      "head" -> Graph.toJson(derive.head, thy),
      "steps" -> JsonObject(steps)
    ).noEmpty
  }
}
