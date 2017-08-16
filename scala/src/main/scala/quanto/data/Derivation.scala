package quanto.data

import quanto.util.json._
import javax.management.remote.rmi._RMIConnectionImpl_Tie
import scala.collection.SortedSet
import quanto.gui.{StepState, HeadState, DeriveState}
import quanto.util.TreeSeq
import quanto.layout.ForceLayout

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
                 graph: Graph) {
  def copy(name: DSName = name,
           ruleName: String = ruleName,
           rule: Rule = rule,
           variant: RuleVariant = variant,
           graph: Graph = graph)
  = DStep(name,ruleName,rule,variant,graph)

  def layout: DStep = {
    val layoutProc = new ForceLayout
    layoutProc.maxIterations = 400
    layoutProc.keepCentered = false
    layoutProc.nodeCharge = 0.0
    //layoutProc.edgeLength = 0.1
    layoutProc.gravity = 0.0


    graph.verts.foreach { v =>
      if (graph.isBoundary(v) || (graph.vdata(v).coord != (0.0,0.0) && graph.vdata(v).coord != (0.0,-1.0))) layoutProc.lockVertex(v)
      //if (graph.isBoundary(v) || !rule.rhs.verts.contains(v)) layoutProc.lockVertex(v)
    }

    // layout the graph before acquiring the lock, so many can be done in parallel
    val graph1 = layoutProc.layout(graph, randomCoords = false).snapToGrid()
    copy(graph = graph1)
  }
}

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
                      parentMap: PFun[DSName,DSName] = PFun())
extends TreeSeq[DeriveState]
{
  def copy(theory: Theory = theory,
           root: Graph = root,
           steps: Map[DSName,DStep] = steps,
           heads: SortedSet[DSName] = heads,
           parent: PFun[DSName,DSName] = parentMap) = Derivation(theory,root,steps,heads,parent)

  def stepsTo(head: DSName): Array[DSName] =
    (parentMap.get(head) match {
      case Some(p) => stepsTo(p)
      case None => Array()
    }) :+ head

  def graphsTo(head : DSName) = root +: stepsTo(head).map(s => steps(s).graph)


  def updateGraphInStep(s: DSName, g: Graph) = {
    val s1 = steps(s).copy(graph = g)
    copy (steps = steps + (s -> s1))
  }


  def children(s: DSName) = parentMap.codf(s)

  def allChildren(s: DSName): Set[DSName] =
    children(s).foldLeft(Set[DSName]()) { case (set,c) => set union allChildren(c) } + s

  def hasParent(s: DSName) = parentMap.domSet.contains(s)
  def hasChildren(s: DSName) = parentMap.codSet.contains(s)
  def isHead(s: DSName) = heads.contains(s)

  def firstHead = heads.headOption
  def firstSteps = steps.keySet.filter(!parentMap.domSet.contains(_))

  def addHead(h: DSName) = copy(heads = heads + h)
  def deleteHead(h: DSName) = copy(heads = heads - h)

  def addStep(parentOpt: Option[DSName], step: DStep) = parentOpt match {
    case Some(p) =>
      copy (
        steps = steps + (step.name -> step),
        heads = (if (heads.contains(p)) heads - p else heads) + step.name,
        parent = parentMap + (step.name -> p))
    case None =>
      copy (
        steps = steps + (step.name -> step),
        heads = heads + step.name)
  }

  def deleteStep(s: DSName) = {
    val parentOpt = parentMap.get(s)
    val (steps1,heads1,parent1) = allChildren(s).foldLeft((steps,heads,parentMap)) {
      case ((ss,hs,p), s1) => (ss - s1, hs - s1, p - s1)
    }

    copy(
      steps = steps1,
      heads = parentOpt match { case Some(s) => heads1 + s ; case _ => heads1 },
      parent = parent1
    )
  }

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
      case StepState(step) => Some(parentMap.get(step) match {case Some(p) => StepState(p); case None => HeadState(None)})
    }
  def children(state: DeriveState): Seq[DeriveState] =
    state match {
      case HeadState(None) => firstSteps.toSeq.map(StepState)
      case HeadState(_) => Seq()
      case StepState(step) =>
        if (isHead(step)) children(step).toSeq.map(StepState) :+ HeadState(Some(step))
        else children(step).toSeq.map(StepState)
    }

  def toJson(theory: Theory) : Json = Derivation.toJson(this, theory)
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
      parentMap = parent
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
    val steps = derive.steps.map { case (k, v) => (k.toString, DStep.toJson(v, derive.parentMap.get(k), thy)) }
    JsonObject(
      "root" -> Graph.toJson(derive.root, thy),
      "steps" -> JsonObject(steps),
      "heads" -> JsonArray(derive.heads.map(_.toString))
    ).noEmpty
  }
}
