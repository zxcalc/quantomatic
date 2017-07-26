package quanto.cosy

import quanto.data._
import quanto.rewrite._
import scala.util.Random

/**
  * Created by hector on 29/06/17.
  */
object RuleSynthesis {

  /** Given an equivalence class creates rules for any irreducible members of the class */
  def graphEquivClassReduction[T <: Ordered[T]](makeGraph: (T => Graph),
                                                equivalenceClass: EquivalenceClass[T],
                                                knownRules: List[Rule]): List[Rule] = {
    val reductionRules = knownRules.filter(r => r.lhs > r.rhs)
    // If you want to include the other direction as well, then pass a list of rule.inverse
    val irreducibleMembers = equivalenceClass.members.filter(
      m =>
        reductionRules.forall(r => Matcher.findMatches(r.lhs, makeGraph(m._1)).nonEmpty)
    )
    var newRules: List[Rule] = List()
    if (irreducibleMembers.nonEmpty) {
      val smallestMember = irreducibleMembers.map(x => x._1).min
      for (member <- irreducibleMembers) {
        if (member._1 != smallestMember) {
          newRules = new Rule(makeGraph(member._1), makeGraph(smallestMember)) :: newRules
        }
      }
    }
    newRules
  }

  def rulesFromEquivalenceClasses[T <: Ordered[T]](makeGraph: (T => Graph),
                                                   equivalenceClasses: List[EquivalenceClass[T]],
                                                   knownRules: List[Rule]): List[Rule] = {
    equivalenceClasses match {
      case Nil => knownRules
      case x :: xs =>
        val newRules = graphEquivClassReduction[T](makeGraph, x, knownRules)
        newRules ::: rulesFromEquivalenceClasses(makeGraph, xs, newRules)
    }
  }

  def discardReducibleRules(rules: List[Rule], seed: Random = new Random()): List[Rule] = {
    var rulesToKeep = List[Rule]()
    for (rule <- rules) {
      val otherRules = rules.filter(r => r != rule)
      val attemptedReduce = AutoReduce.genericReduce(rule.lhs, otherRules, seed)
      if (attemptedReduce >= rule.lhs) rulesToKeep = rule :: rulesToKeep
    }
    rulesToKeep
  }

  def minimiseRuleset(rules: List[Rule], seed: Random = new Random()): List[Rule] = {
    for (rule <- rules) yield {
      minimiseRuleInPresenceOf(rule, rules.filter(r => r != rule), seed)
    }
  }

  def minimiseRuleInPresenceOf(rule: Rule, otherRules: List[Rule], seed: Random = new Random()): Rule = {
    var minLhs = AutoReduce.genericReduce(rule.lhs, otherRules, seed)
    var minRhs = AutoReduce.genericReduce(rule.rhs, otherRules, seed)
    new Rule(minLhs, minRhs)
  }
}

/**
  * Automatically apply given rules to a given graph
  */

object AutoReduce {

  // Tries multiple methods and is sure to return nothing larger than what you started with
  def genericReduce(graph: Graph, rules: List[Rule], seed: Random = new Random()): Graph = {
    (graph :: AutoReduce.greedyReduce(graph, rules)._1 ::
      (for (_ <- 0 until graph.verts.size) yield {
        AutoReduce.annealingReduce(graph, rules, maxTime = math.pow(graph.verts.size, 2).toInt, 3, seed)._1
      }).toList).min
  }

  // Simplest entry point
  def annealingReduce(graph: Graph, rules: List[Rule]): (Graph, List[(Rule, Int)]) = {
    val maxTime = math.pow(graph.verts.size, 2).toInt // Set as squaring #vertices for now
    val timeDilation = 3 // Gives an e^-3 ~ 0.05% chance of a non-reduction rule on the final step
    annealingReduce(graph.normalise, rules, maxTime, timeDilation)
  }

  // Enter here to have control over e.g. how long it runs for
  def annealingReduce(graph: Graph,
                      rules: List[Rule],
                      maxTime: Int,
                      timeDilation: Double,
                      seed: Random = new Random()): (Graph, List[(Rule, Int)]) = {
    val decRules = rules.filter(r => r.lhs > r.rhs)
    val incRules = rules.filterNot(r => r.lhs > r.rhs)
    annealingReduce(graph.normalise, incRules, decRules, 0, maxTime, timeDilation, seed, List())
  }

  // Generally don't enter here except to reproduce results
  def annealingReduce(graph: Graph,
                      incRules: List[Rule],
                      decRules: List[Rule],
                      timeStep: Int,
                      maxTime: Int,
                      timeDilation: Double,
                      seed: Random,
                      priorApplications: List[(Rule, Int)]): (Graph, List[(Rule, Int)]) = {
    val rulesToUse = if (seed.nextDouble() < math.exp(-timeDilation * timeStep / maxTime)) {
      incRules
    } else {
      decRules
    }

    def skipOver = if (timeStep < maxTime) {
      annealingReduce(graph, incRules, decRules, timeStep + 1,
        maxTime, timeDilation, seed, priorApplications)
    } else {
      (graph, priorApplications)
    }

    if (rulesToUse.nonEmpty) {

      val randRule = rulesToUse(seed.nextInt(rulesToUse.length))
      val matches = Matcher.findMatches(randRule.lhs, graph)
      if (matches.nonEmpty) {
        val randMatchIndex = seed.nextInt(matches.length)
        var reducedGraph = graph
        if (matches.nonEmpty) {
          val randMatch = matches(randMatchIndex)
          // apply a randomly chosen instance of a randomly chosen rule to the graph
          reducedGraph = Rewriter.rewrite(randMatch, randRule.rhs)._1.normalise
        }
        if (timeStep == maxTime) {
          // Finished
          (reducedGraph, (randRule, randMatchIndex) :: priorApplications)
        } else {
          // Next step
          annealingReduce(reducedGraph, incRules, decRules, timeStep + 1,
            maxTime, timeDilation, seed, (randRule, randMatchIndex) :: priorApplications)
        }
      }
      skipOver
    } else {
      skipOver
    }
  }

  // Simply apply the first reduction rule it can find until there are none left
  def greedyReduce(graph: Graph, rules: List[Rule], priorApplications: List[(Rule, Int)] = List()):
  (Graph, List[(Rule, Int)]) = {

    var found = false
    var reducedGraph = graph
    var applicationList = priorApplications

    def rewriteMatch(m: Match, rule: Rule, replacement: Graph) = {
      applicationList = (rule, 0) :: applicationList
      reducedGraph = Rewriter.rewrite(m, replacement)._1
      found = true
    }

    val reductionRules = rules.filter(rule => rule.lhs > rule.rhs) :::
      rules.filter(rule => rule.lhs < rule.rhs).map(rule => rule.inverse)
    for (rule <- reductionRules) {
      if (!found) {
        val matches = Matcher.findMatches(rule.lhs, graph)
        if (matches.nonEmpty) {
          rewriteMatch(matches.head, rule, rule.rhs)
        }
      }
    }

    if (!found) {
      (reducedGraph.normalise, applicationList)
    } else {
      greedyReduce(reducedGraph.normalise, reductionRules, applicationList)
    }
  }
}