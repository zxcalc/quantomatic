package quanto.cosy

import quanto.data._
import quanto.data.Derivation.DerivationWithHead
import quanto.rewrite._
import quanto.util.json.Json

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by hector on 29/06/17.
  */
object RuleSynthesis {

  def loadRuleDirectory(directory: String): List[Rule] = {
    quanto.util.FileHelper.getListOfFiles(directory, raw".*\.qrule").
      map(file => (file.getName.replaceFirst(raw"\.qrule", ""), Json.parse(file))).
      map(nameAndJS => Rule.fromJson(nameAndJS._2, Theory.fromFile("red_green"), Some(RuleDesc(nameAndJS._1)))
      )
  }

  /** Given an equivalence class creates rules for any irreducible members of the class */
  def graphEquivClassReduction[T](makeGraph: (T => Graph),
                                  equivalenceClass: EquivalenceClass[T],
                                  knownRules: List[Rule]): List[Rule] = {
    val reductionRules = knownRules.filter(r => r.lhs > r.rhs)
    // If you want to include the other direction as well, then pass a list of rule.inverse
    val irreducibleMembers = equivalenceClass.members.filter(
      m =>
        reductionRules.forall(r => Matcher.findMatches(r.lhs, makeGraph(m)).nonEmpty)
    )

    if (irreducibleMembers.nonEmpty) {
      val smallestMember = irreducibleMembers.minBy(makeGraph(_).verts.size)
      irreducibleMembers.filter(_ != smallestMember).map(
        member => new Rule(makeGraph(member), makeGraph(smallestMember))
      )
    } else List()
  }

  def rulesFromEquivalenceClasses[T](makeGraph: (T => Graph),
                                     equivalenceClasses: List[EquivalenceClass[T]],
                                     knownRules: List[Rule]): List[Rule] = {
    equivalenceClasses match {
      case Nil => knownRules
      case x :: xs =>
        val newRules = graphEquivClassReduction[T](makeGraph, x, knownRules)
        newRules ::: rulesFromEquivalenceClasses(makeGraph, xs, newRules)
    }
  }

  def discardDirectlyReducibleRules(rules: List[Rule], theory: Theory, seed: Random = new Random()): List[Rule] = {
    rules.filter(rule =>
      AutoReduce.genericReduce(graphToDerivation(rule.lhs, theory), rules.filter(r => r != rule), seed) >= rule.lhs
    )
  }

  def minimiseRuleset(rules: List[Rule], theory: Theory, seed: Random = new Random()): List[Rule] = {
    rules.map(rule => minimiseRuleInPresenceOf(rule, rules.filter(otherRule => otherRule != rule), theory))
  }

  def minimiseRuleInPresenceOf(rule: Rule, otherRules: List[Rule], theory: Theory, seed: Random = new Random()): Rule = {
    val minLhs: Graph = AutoReduce.genericReduce(graphToDerivation(rule.lhs, theory), otherRules, seed)
    val minRhs: Graph = AutoReduce.genericReduce(graphToDerivation(rule.rhs, theory), otherRules, seed)
    val wasItReduced = (minLhs < rule.lhs) || (minRhs < rule.rhs)
    new Rule(minLhs, minRhs, description = RuleDesc(
      rule.name + (if (wasItReduced) " reduced" else "")))
  }

  def graphToDerivation(graph: Graph, theory: Theory): DerivationWithHead = {
    (new Derivation(theory, graph), None)
  }

  Derivation
}

/**
  * Automatically apply given rules to a given graph
  */

object AutoReduce {

  implicit def inverseToRuleVariant(inverse: Boolean): RuleVariant = if (inverse) RuleInverse else RuleNormal

  def smallestStepNameBelow(derivationHeadPair: (Derivation, Option[DSName])): Option[DSName] = {
    derivationHeadPair._2 match {
      case Some(head) =>
        Some(derivationHeadPair._1.allChildren(head).minBy(step => derivationHeadPair._1.steps(step).graph))
      case None =>
        val allChildren = derivationHeadPair._1.firstSteps.flatMap(
          firstStep => derivationHeadPair._1.allChildren(firstStep)
        )
        if (allChildren.nonEmpty) {
          Some(allChildren.minBy(step => derivationHeadPair._1.steps(step).graph))
        } else {
          None
        }
    }
  }

  // Tries multiple methods and is sure to return nothing larger than what you started with
  def genericReduce(derivationAndHead: DerivationWithHead,
                    rules: List[Rule],
                    seed: Random = new Random()):
  DerivationWithHead = {
    var latestDerivation: DerivationWithHead = derivationAndHead
    latestDerivation._2 match {
      case Some(initialHead) =>
        latestDerivation = annealingReduce(latestDerivation, rules, seed)
        latestDerivation = greedyReduce(latestDerivation, rules)
        latestDerivation = greedyReduce((latestDerivation._1.addHead(initialHead), Some(initialHead)), rules)
      case None =>
        latestDerivation = annealingReduce(latestDerivation, rules, seed)
        latestDerivation = greedyReduce(latestDerivation, rules)
    }

    // Go back to original request, find smallest child
    (latestDerivation._1, smallestStepNameBelow(latestDerivation))
  }

  // Simplest entry point
  def annealingReduce(derivationHeadPair: DerivationWithHead,
                      rules: List[Rule],
                      seed: Random = new Random(),
                      vertexLimit : Option[Int] = None): DerivationWithHead = {
    val maxTime = math.pow(derivationHeadPair.verts.size, 2).toInt // Set as squaring #vertices for now
    val timeDilation = 3 // Gives an e^-3 ~ 0.05% chance of a non-reduction rule on the final step
    annealingReduce(derivationHeadPair, rules, maxTime, timeDilation, seed, vertexLimit)
  }

  // Enter here to have control over e.g. how long it runs for
  def annealingReduce(derivationHeadPair: DerivationWithHead,
                      rules: List[Rule],
                      maxTime: Int,
                      timeDilation: Double,
                      seed: Random,
                      vertexLimit: Option[Int]): DerivationWithHead = {
    (0 until maxTime).foldLeft(derivationHeadPair) { (d, time) =>
      val allowIncrease = seed.nextDouble() < math.exp(-timeDilation * time / maxTime)
      if (rules.nonEmpty) {
        val randRule = rules(seed.nextInt(rules.length))
        val suggestedNextStep = randomSingleApply(d, randRule, seed)
        val head = Derivation.derivationHeadPairToGraph(d)
        val smallEnough = vertexLimit.isEmpty || (head.verts.size < vertexLimit.get)
        if ((allowIncrease && smallEnough) || suggestedNextStep < head) suggestedNextStep else d
      } else d
    }
  }

  @tailrec
  def greedyReduce(derivationHeadPair: DerivationWithHead,
                   rules: List[Rule],
                   remainingRules: List[Rule]): DerivationWithHead = {
    remainingRules match {
      case r :: tailRules => Matcher.findMatches(r.lhs, derivationHeadPair) match {
        case ruleMatch #:: t =>
          val reducedGraph = Rewriter.rewrite(ruleMatch, r.rhs)._1.minimise
          val stepName = quanto.data.Names.mapToNameMap(derivationHeadPair._1.steps).
            freshWithSuggestion(DSName(r.description.name))
          greedyReduce((derivationHeadPair._1.addStep(
            derivationHeadPair._2,
            DStep(stepName, r, reducedGraph)
          ), Some(stepName)),
            rules,
            rules)
        case Stream.Empty =>
          greedyReduce(derivationHeadPair, rules, tailRules)
      }
      case Nil => derivationHeadPair
    }
  }

  // Simply apply the first reduction rule it can find until there are none left
  def greedyReduce(derivationHeadPair: DerivationWithHead, rules: List[Rule]): DerivationWithHead = {
    val reducingRules = rules.filter(rule => rule.lhs > rule.rhs)
    val reduced = greedyReduce(derivationHeadPair, reducingRules, reducingRules)
    // Go round again until it stops reducing
    if (reduced < derivationHeadPair) {
      greedyReduce(reduced, rules)
    } else reduced
  }

  def alwaysTrue(a: Graph, b: Graph): Boolean = true

  def randomApply(derivationWithHead: DerivationWithHead,
                  rules: List[Rule],
                  numberOfApplications: Int,
                  requirementToKeep: (Graph, Graph) => Boolean = alwaysTrue,
                  seed: Random = new Random()): DerivationWithHead = {
    require(numberOfApplications > 0)
    if (rules.nonEmpty) {
      (0 until numberOfApplications).foldLeft(derivationWithHead) {
        (d, _) => {
          val suggestedUpdate = randomSingleApply(d, rules(seed.nextInt(rules.length)), seed)
          if (requirementToKeep(suggestedUpdate, d)) suggestedUpdate else d
        }
      }
    } else derivationWithHead
  }

  def randomSingleApply(derivationWithHead: DerivationWithHead,
                        rule: Rule,
                        seed: Random = new Random()): DerivationWithHead = {

    val matches = Matcher.findMatches(rule.lhs, derivationWithHead)
    val chosenMatch: Option[Match] = matches.find(_ => seed.nextBoolean())

    if (chosenMatch.nonEmpty) {
      // apply a randomly chosen instance of the rule to the graph
      val reducedGraph = Rewriter.rewrite(chosenMatch.get, rule.rhs)._1.minimise
      val nextStepName = quanto.data.Names.mapToNameMap(derivationWithHead._1.steps).
        freshWithSuggestion(DSName(rule.description.name.replaceFirst("^.*\\/", "") + "-0"))
      (derivationWithHead._1.addStep(
        derivationWithHead._2,
        DStep(nextStepName,
          rule,
          reducedGraph)
      ), Some(nextStepName))
    } else {
      derivationWithHead
    }
  }
}