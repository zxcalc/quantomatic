package quanto.cosy

import quanto.data
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

  def discardDirectlyReducibleRules(rules: List[Rule], theory: Theory, seed: Random = new Random()): List[Rule] = {
    rules.filter(rule =>
      AutoReduce.genericReduce(rule.lhs, rules.filter(r => r != rule), theory, seed) >= rule.lhs
    )
  }

  def minimiseRuleset(rules: List[Rule], theory: Theory, seed: Random = new Random()): List[Rule] = {
    rules.map(rule => minimiseRuleInPresenceOf(rule, rules.filter(otherRule => otherRule != rule), theory))
  }

  def minimiseRuleInPresenceOf(rule: Rule, otherRules: List[Rule], theory: Theory, seed: Random = new Random()): Rule = {
    val minLhs : Graph = AutoReduce.genericReduce(rule.lhs, otherRules, theory, seed)
    val minRhs : Graph = AutoReduce.genericReduce(rule.rhs, otherRules, theory, seed)
    val wasItReduced = (minLhs < rule.lhs) || (minRhs < rule.rhs)
    new Rule(minLhs, minRhs, description = Some(RuleDesc(
      (if (rule.description.isDefined) rule.description.get.name else "Unnamed rule") +
        (if (rule.description.isDefined && rule.description.get.inverse) " inverted" else "") +
        (if (wasItReduced) " reduced" else "")
    )))
  }


  implicit def derivationToFirstHead(derivation: Derivation) : Graph = AutoReduce.derivationToFirstHead(derivation)
}

/**
  * Automatically apply given rules to a given graph
  */

object AutoReduce {

  implicit def inverseToRuleVariant(inverse: Boolean): RuleVariant = if (inverse) RuleInverse else RuleNormal

  implicit def derivationToFirstHead(derivation: Derivation): Graph = {
    derivation.firstHead match {
      case Some(head) => derivation.steps(head).graph
      case None => derivation.root
    }
  }

  // Tries multiple methods and is sure to return nothing larger than what you started with
  def genericReduce(graph: Graph, rules: List[Rule], theory: Theory, seed: Random = new Random()):
  Derivation = {
    (new Derivation(theory, graph) :: AutoReduce.greedyReduce(graph, rules, theory) ::
      (for (_ <- 0 until 1) yield {
        AutoReduce.annealingReduce(graph, rules, theory, maxTime = math.pow(graph.verts.size, 2).toInt, 3, seed)
      }).toList).minBy(derivationToFirstHead)
  }

  // Simplest entry point
  def annealingReduce(graph: Graph, rules: List[Rule], theory: Theory): Derivation = {
    val maxTime = math.pow(graph.verts.size, 2).toInt // Set as squaring #vertices for now
    val timeDilation = 3 // Gives an e^-3 ~ 0.05% chance of a non-reduction rule on the final step
    annealingReduce(graph.normalise, rules, theory, maxTime, timeDilation)
  }

  // Enter here to have control over e.g. how long it runs for
  def annealingReduce(graph: Graph,
                      rules: List[Rule],
                      theory: Theory,
                      maxTime: Int,
                      timeDilation: Double,
                      seed: Random = new Random()): Derivation = {
    val decRules = rules.filter(r => r.lhs > r.rhs)
    val incRules = rules.filterNot(r => r.lhs > r.rhs)
    annealingReduce(graph.normalise, incRules, decRules, 0, maxTime, timeDilation, seed, new Derivation(theory, graph))
  }

  // Generally don't enter here except to reproduce results
  def annealingReduce(graph: Graph,
                      incRules: List[Rule],
                      decRules: List[Rule],
                      timeStep: Int,
                      maxTime: Int,
                      timeDilation: Double,
                      seed: Random,
                      derivation: Derivation): Derivation = {
    val rulesToUse = if (seed.nextDouble() < math.exp(-timeDilation * timeStep / maxTime)) {
      incRules
    } else {
      decRules
    }

    def skipOver = if (timeStep < maxTime) {
      annealingReduce(graph, incRules, decRules, timeStep + 1,
        maxTime, timeDilation, seed, derivation)
    } else {
      derivation
    }

    if (rulesToUse.nonEmpty && timeStep < maxTime) {

      val randRule = rulesToUse(seed.nextInt(rulesToUse.length))
      val matches = Matcher.findMatches(randRule.lhs, graph)
      if (matches.nonEmpty) {
        // apply a randomly chosen instance of a randomly chosen rule to the graph
        val randMatchIndex = seed.nextInt(matches.length)
        val reducedGraph = Rewriter.rewrite(matches(randMatchIndex), randRule.rhs)._1.normalise
        val description = randRule.description.getOrElse(RuleDesc("unnamed rule"))
        annealingReduce(reducedGraph,
          incRules,
          decRules,
          timeStep + 1,
          maxTime,
          timeDilation,
          seed,
          derivation.addStep(
            derivation.firstHead,
            DStep(quanto.data.Names.mapToNameMap(derivation.steps).freshWithSuggestion(DSName("s")),
              description.name,
              randRule,
              description.inverse,
              reducedGraph)
          )
        )
      } else {
        skipOver
      }
    } else {
      skipOver
    }
  }

  @tailrec
  def greedyReduce(graph: Graph, rules: List[Rule], derivation: Derivation, remainingRules: List[Rule]): Derivation = {
    remainingRules match {
      case r :: tailRules => Matcher.findMatches(r.lhs, graph) match {
        case ruleMatch #:: t =>
          val reducedGraph = Rewriter.rewrite(ruleMatch, r.rhs)._1.normalise
          val description = r.description.getOrElse[RuleDesc](RuleDesc("unnamed rule"))
          greedyReduce(
            reducedGraph,
            rules,
            derivation.addStep(
              derivation.firstHead,
              DStep(quanto.data.Names.mapToNameMap(derivation.steps).freshWithSuggestion(DSName("s")),
                description.name,
                r,
                description.inverse,
                reducedGraph)
            ),
            rules)
        case Stream.Empty =>
          greedyReduce(graph, rules, derivation, tailRules)
      }
      case Nil => derivation
    }
  }

  // Simply apply the first reduction rule it can find until there are none left
  def greedyReduce(graph: Graph, rules: List[Rule], theory: Theory): Derivation = {
    val reducingRules = rules.filter(rule => rule.lhs > rule.rhs)
    greedyReduce(graph, reducingRules, new Derivation(theory, graph), reducingRules)
  }
}