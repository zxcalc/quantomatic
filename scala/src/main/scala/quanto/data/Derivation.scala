package quanto.data

import quanto.util.json._
import javax.management.remote.rmi._RMIConnectionImpl_Tie

trait DerivationException
case class DerivationLoadException(message: String, cause: Throwable = null)
  extends Exception(message, cause)
  with DerivationException

sealed abstract class RuleVariant
case object RuleNormal extends RuleVariant { override def toString = "normal" }
case object RuleInverse extends RuleVariant { override def toString = "inverse" }

case class DStep(name: DSName,
                 ruleName: String,
                 rule: Rule,
                 variant: RuleVariant,
                 graph: Graph)

object DStep {
  def toJson(dstep: DStep, parent: Option[DSName], thy: Theory = Theory.DefaultTheory): Json = {
    JsonObject(
      "name" -> dstep.name.toString,
      "parent" -> parent.map(_.toString),
      "rule_name" -> dstep.ruleName,
      "rule" -> Rule.toJson(dstep.rule, thy),
      "rule_variant" -> (dstep.variant match { case RuleNormal => JsonNull; case v => v.toString }),
      "graph" -> Graph.toJson(dstep.graph, thy)
    ).noEmpty
  }

  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory) : DStep = try {
    DStep(
      name = DSName((json / "name").stringValue),
      ruleName = (json / "ruleName").stringValue,
      rule = Rule.fromJson(json / "rule", thy),
      variant = json ? "rule_variant" match { case JsonString("inverse") => RuleInverse; case _ => RuleNormal },
      graph = Graph.fromJson(json / "graph", thy)
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
    val parent = (json / "steps").asObject.foldLeft(PFun[DSName,DSName]()) {
      case (pf,(step,obj)) => obj.get("parent") match {
        case Some(JsonString(p)) => pf + (DSName(step) -> DSName(p))
        case _ => pf
      }
    }

    val steps = (json / "steps").asObject.foldLeft(Map[DSName,DStep]()) {
      case (mp,(step,obj)) => mp + (DSName(step) -> DStep.fromJson(obj, thy))
    }

    val heads = (json / "heads").asArray.map(h => DSName(h.stringValue)).toSet


    Derivation(
      theory = thy,
      root = Graph.fromJson(json / "root", thy),
      steps = steps,
      heads = heads,
      parent = parent
    )
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
