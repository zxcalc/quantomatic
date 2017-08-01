package quanto.cosy

import quanto.data._
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

  def discardDirectlyReducibleRules(rules: List[Rule], seed: Random = new Random()): List[Rule] = {
    rules.filter(rule =>
      AutoReduce.genericReduce(rule.lhs, rules.filter(r => r != rule), seed) >= rule.lhs
    )
  }

  def minimiseRuleset(rules: List[Rule], seed: Random = new Random()): List[Rule] = {
    rules.map(rule => minimiseRuleInPresenceOf(rule, rules.filter(otherRule => otherRule != rule)))
  }

  def minimiseRuleInPresenceOf(rule: Rule, otherRules: List[Rule], seed: Random = new Random()): Rule = {
    val minLhs = AutoReduce.genericReduce(rule.lhs, otherRules, seed)
    val minRhs = AutoReduce.genericReduce(rule.rhs, otherRules, seed)
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

    if (rulesToUse.nonEmpty && timeStep < maxTime) {

      val randRule = rulesToUse(seed.nextInt(rulesToUse.length))
      val matches = Matcher.findMatches(randRule.lhs, graph)
      if (matches.nonEmpty) {
        // apply a randomly chosen instance of a randomly chosen rule to the graph
        val randMatchIndex = seed.nextInt(matches.length)
        val reducedGraph = Rewriter.rewrite(matches(randMatchIndex), randRule.rhs)._1.normalise
        annealingReduce(reducedGraph, incRules, decRules, timeStep + 1,
          maxTime, timeDilation, seed, (randRule, randMatchIndex) :: priorApplications)
      }
      skipOver
    } else {
      skipOver
    }
  }

  @tailrec
  def greedyReduce(graph: Graph, rules: List[Rule], priorApplications: List[(Rule, Int)], remainingRules: List[Rule]): (Graph, List[(Rule, Int)]) = {
    remainingRules match {
      case r :: tail => Matcher.findMatches(r.lhs, graph) match {
        case ruleMatch #:: t =>
          greedyReduce(Rewriter.rewrite(ruleMatch, r.rhs)._1.normalise, rules, (r, 0) :: priorApplications, rules)
        case Stream.Empty =>
          greedyReduce(graph, rules, priorApplications, tail)
      }
      case Nil => (graph, priorApplications)
    }
  }

  // Simply apply the first reduction rule it can find until there are none left
  def greedyReduce(graph: Graph, rules: List[Rule]):
  (Graph, List[(Rule, Int)]) = {

    greedyReduce(graph, rules.filter(rule => rule.lhs > rule.rhs), List(), rules)
  }
}