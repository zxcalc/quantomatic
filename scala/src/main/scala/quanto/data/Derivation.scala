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
                 rule: String,
                 variant: RuleVariant,
                 matchedVertices: Set[VName],
                 replacedVertices: Set[VName],
                 graph: Graph)

object DStep {
  def toJson(dstep: DStep, parent: Option[DSName], thy: Theory = Theory.DefaultTheory): Json = {
    JsonObject(
      "name" -> dstep.name.toString,
      "parent" -> parent.map(_.toString),
      "rule" -> dstep.rule,
      "rule_variant" -> (dstep.variant match { case RuleNormal => JsonNull; case v => v.toString }),
      "matched" -> JsonObject("vertices" -> dstep.matchedVertices.map(_.toString)),
      "replaced" -> JsonObject("vertices" -> dstep.replacedVertices.map(_.toString)),
      "graph" -> Graph.toJson(dstep.graph, thy)
    ).noEmpty
  }

  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory) : DStep = try {
    DStep(
      name = DSName((json / "name").stringValue),
      rule = (json / "rule").stringValue,
      variant = json ? "rule_variant" match { case JsonString("inverse") => RuleInverse; case _ => RuleNormal },
      matchedVertices = (json / "matched" / "vertices").vectorValue.map(v => VName(v.stringValue)).toSet,
      replacedVertices = (json / "replaced" / "vertices").vectorValue.map(v => VName(v.stringValue)).toSet,
      graph = null
    )
  } catch {
    case e: Exception => throw new DerivationLoadException("Error reading JSON")
  }
}

case class Derivation(theory: Theory,
                      root: Graph,
                      steps: Map[DSName,DStep],
                      heads: Set[DSName],
                      parent: PFun[DSName,DSName])

object Derivation {
  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory) = try {
    Rule(lhs = Graph.fromJson(json / "lhs", thy),
      rhs = Graph.fromJson(json / "rhs", thy),
      derivation = json.get("derivation").map(_.stringValue))
  } catch {
    case e: Exception => throw new DerivationLoadException("Error reading JSON", e)
  }

  def toJson(derive: Derivation, thy: Theory = Theory.DefaultTheory) = {
    val steps = derive.steps.map { case (k, v) => (k.toString, DStep.toJson(v, derive.parent.get(k), thy)) }
    JsonObject(
      "root" -> Graph.toJson(derive.root, thy),
      "steps" -> JsonObject(steps),
      "heads" -> JsonArray(derive.heads.map(_.toString))
    ).noEmpty
  }
}
