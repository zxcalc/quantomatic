package quanto.data

import quanto.util.json._
import javax.management.remote.rmi._RMIConnectionImpl_Tie
import scala.collection.SortedSet
import quanto.gui.{StepState, HeadState, DeriveState}
import quanto.util.TreeSeq

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

  def fromJson(name: DSName, json: Json, thy: Theory = Theory.DefaultTheory) : DStep = try {
    DStep(
      name = name,
      ruleName = (json / "rule_name").stringValue,
      rule = Rule.fromJson(json / "rule", thy),
      variant = json ? "rule_variant" match { case JsonString("inverse") => RuleInverse; case _ => RuleNormal },
      graph = Graph.fromJson(json / "graph", thy)
    )
  } catch {
    case e: JsonAccessException =>
      throw new DerivationLoadException(e.getMessage)
    case e: RuleLoadException =>
      throw new DerivationLoadException("Rule at step '" + name + "': " + e.getMessage)
    case e: GraphLoadException =>
      throw new DerivationLoadException("Graph at step '" + name + "': " + e.getMessage)
    case e: Exception =>
      e.printStackTrace()
      throw new DerivationLoadException("Unexpected error reading JSON")
  }
}

case class Derivation(theory: Theory,
                      root: Graph,
                      steps: Map[DSName,DStep] = Map(),
                      heads: SortedSet[DSName] = SortedSet(),
                      parent: PFun[DSName,DSName] = PFun())
extends TreeSeq[DeriveState]
{
  def copy(theory: Theory = theory,
           root: Graph = root,
           steps: Map[DSName,DStep] = steps,
           heads: SortedSet[DSName] = heads,
           parent: PFun[DSName,DSName] = parent) = Derivation(theory,root,steps,heads,parent)

  def stepsTo(head: DSName): Array[DSName] =
    (parent.get(head) match {
      case Some(p) => stepsTo(p)
      case None => Array()
    }) :+ head

  def graphsTo(head : DSName) = root +: stepsTo(head).map(s => steps(s).graph)

  def addStep(parent: DSName, step: DStep) = copy (
    steps = steps + (step.name -> step),
    heads = (if (heads.contains(parent)) heads - parent else heads) + step.name
  )

  def children(s: DSName) = parent.codf(s)

  def uniqueChild(s: DSName) = {
    val set = children(s)
    if (set.size == 1) Some(set.head)
    else None
  }

  def hasParent(s: DSName) = parent.domSet.contains(s)
  def hasUniqueChild(s: DSName) = parent.codf(s).size == 1
  def hasChildren(s: DSName) = parent.codSet.contains(s)
  def isHead(s: DSName) = heads.contains(s)

  def firstHead = heads.headOption
  def firstSteps = steps.keySet.filter(!parent.domSet.contains(_))

//  def nextHead(s: DSName) = heads.find(s1 => s < s1)
//  def hasNextHead(s: DSName) = heads.lastOption match { case Some(s1) => s != s1; case None => false }

//  def rewind(s: DSName): DSName = parent.get(s) match { case Some(p) => rewind(p); case None => s }

  // find a head that is downstream from this step
  def fastForward(s: DSName): DSName =
    if (isHead(s)) s
    else children(s).headOption match { case Some(ch) => fastForward(ch); case None => s }

  private def dft(step: DSName, rest: Vector[DeriveState]) : Vector[DeriveState] =
    (if (isHead(step)) Vector(StepState(step), HeadState(Some(step)))
     else Vector(StepState(step))) ++
    children(step).foldRight(rest) { case (ch,rest1) => dft(ch,rest1) }

  lazy val stateVector: Vector[DeriveState] = {
    HeadState(None) +:
    firstSteps.foldRight(Vector[DeriveState]()) { case (step, rest) => dft(step, rest) }
  }

  // implementations of TreeSeq methods
  def toSeq : Seq[DeriveState] = stateVector
  def indexOf(state : DeriveState): Int = stateVector.indexOf(state)
  def parent(state: DeriveState): Option[DeriveState] =
    state match {
      case HeadState(None) => None
      case HeadState(Some(step)) => Some(StepState(step))
      case StepState(step) => Some(parent.get(step) match {case Some(p) => StepState(p); case None => HeadState(None)})
    }
  def children(state: DeriveState): Seq[DeriveState] =
    state match {
      case HeadState(None) => firstSteps.toSeq.map(StepState)
      case HeadState(_) => Seq()
      case StepState(step) =>
        if (isHead(step)) children(step).toSeq.map(StepState) :+ HeadState(Some(step))
        else children(step).toSeq.map(StepState)
    }
}

object Derivation {
  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory) = try {
    val parent = (json ? "steps").asObject.foldLeft(PFun[DSName,DSName]()) {
      case (pf,(step,obj)) => obj.get("parent") match {
        case Some(JsonString(p)) => pf + (DSName(step) -> DSName(p))
        case _ => pf
      }
    }

    val steps = (json ? "steps").asObject.foldLeft(Map[DSName,DStep]()) {
      case (mp,(step,obj)) => mp + (DSName(step) -> DStep.fromJson(DSName(step), obj, thy))
    }

    val heads = (json ? "heads").asArray.foldLeft(SortedSet[DSName]()) { case (set,h) => set + DSName(h.stringValue) }

    Derivation(
      theory = thy,
      root = Graph.fromJson(json / "root", thy),
      steps = steps,
      heads = heads,
      parent = parent
    )
  } catch {
    case e: JsonAccessException => throw new DerivationLoadException(e.getMessage, e)
    case e: GraphLoadException =>
      throw new DerivationLoadException("Graph 'root': " + e.getMessage, e)
    case e: DerivationLoadException => throw e
    case e: Exception =>
      e.printStackTrace()
      throw new DerivationLoadException("Error reading JSON", e)
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
