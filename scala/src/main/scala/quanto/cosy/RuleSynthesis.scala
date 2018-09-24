package quanto.cosy

import quanto.cosy.RuleSynthesis.GraphComparison
import quanto.data.Derivation.DerivationWithHead
import quanto.data._
import quanto.rewrite._
import quanto.util.json.{Json, JsonObject}

import scala.annotation.tailrec
import scala.util.Random
import scala.util.matching.Regex
import quanto.data.Names._

/**
  * Created by hector on 29/06/17.
  */
object RuleSynthesis {

  type GraphComparison = (Graph, Graph) => Int
  // returns x where
  // x < 0 iff left < right
  // x > 0 iff left > right
  // x == 0 otherwise

  def basicGraphComparison(left: Graph, right: Graph): Int = {
    if (left < right) -1 else {
      if (left > right) 1 else 0
    }
  }



  def loadRuleDirectory(directory: String): List[Rule] = {
    quanto.util.FileHelper.getListOfFiles(directory, raw".*\.qrule").
      map(file => (file.getName.replaceFirst(raw"\.qrule", ""), Json.parse(file))).
      map(nameAndJS => Rule.fromJson(nameAndJS._2, Theory.fromFile("red_green"), Some(RuleDesc(nameAndJS._1)))
      )
  }

  /** Given an equivalence class creates rules for any irreducible members of the class */
  // Superseded by CoSyRun
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

  def extendMatchingSpidersWithBBoxes(rule: Rule, boundariesRegex : Option[Regex]) : Rule = {
    require(!rule.hasBBoxes)
    // This is not safe to do if the rule already has bboxes.

    val boundaries = GraphAnalysis.boundariesFromRegex(rule.lhs, boundariesRegex).toList
    def nearestNeighbourType(graph: Graph, vName: VName) : Option[(VName, String)] = {
      val neighbours = graph.adjacentNodesAndBoundaries(vName)
      if(neighbours.size == 1){
        val t : String = (graph.vdata(neighbours.head).data / "type").toString
        Some((neighbours.head, t))
      } else None
    }

    def addBBoxIfOkay(rule: Rule, vName: VName) : Rule = {
      val lhsT = nearestNeighbourType(rule.lhs, vName)
      val rhsT = nearestNeighbourType(rule.rhs, vName)
      if(lhsT.nonEmpty && rhsT.nonEmpty && lhsT.get._2 == rhsT.get._2 && rule.lhs.vdata(lhsT.get._1).isInstanceOf[NodeV]){
        val leftNeighbour = lhsT.get._1
        val rightNeighbour = rhsT.get._1
        val bBName = rule.lhs.bboxes.freshWithSuggestion("bb0")

        val lhsB = rule.lhs.addBBox(bBName, BBData(), Set(vName))
        val rhsB = rule.lhs.addBBox(bBName, BBData(), Set(vName))
        Rule(lhsB, rhsB, rule.derivation, rule.description)
      } else rule
    }

    def removeBoundaryIfSuperfluous(rule: Rule, vName: VName): Rule = {
      val lhsT = nearestNeighbourType(rule.lhs, vName)
      val rhsT = nearestNeighbourType(rule.rhs, vName)
      if(lhsT.nonEmpty && rhsT.nonEmpty && lhsT.get == rhsT.get && rule.lhs.vdata(lhsT.get._1).isInstanceOf[NodeV]) {
        val ln = lhsT.get._1
        val rn = rhsT.get._1
        val lNeighbourhood = rule.lhs.adjacentNodesAndBoundaries(ln).intersect(boundaries.toSet)
        val rNeighbourhood = rule.rhs.adjacentNodesAndBoundaries(rn).intersect(boundaries.toSet)
        if((lNeighbourhood intersect rNeighbourhood).size > 1) {
          val lCut = rule.lhs.deleteVertex(vName)
          val rCut = rule.rhs.deleteVertex(vName)
          Rule(lCut, rCut)
        } else rule
      } else rule
    }

    val withBBoxes = boundaries.foldLeft(rule)(addBBoxIfOkay)
    boundaries.foldLeft(withBBoxes)(removeBoundaryIfSuperfluous)
  }

  def removeIsomorphisms(theory: Theory, boundaryRegex: Option[Regex], rules: List[Rule]) : List[Rule] = {
    def isIso(rule: Rule) : Boolean = GraphAnalysis.checkIsomorphic(theory, boundaryRegex)(rule.lhs, rule.rhs)
    rules.filter(!isIso(_))
  }

  def greedyReduceRules(comparison: GraphComparison, throwOutIsos : Option[(Theory, Option[Regex])] = None)
                       (rules: List[Rule]): List[Rule] = {

    // Will automatically invert rules that head upwards

    // This does not throw out isomorphisms for you; it returns the list with each entry altered

    // Yes, this is imperative rather than functional. We really do want to update a list as we act on it.

    var rulesAsMap = rules.zipWithIndex.map(ri => (ri._2, ri._1)).toMap // i -> rule_i
    var updatedThisRun = true

    def isomorphism(rule: Rule): Boolean = throwOutIsos match {
      case Some(tor) =>
        GraphAnalysis.checkIsomorphic(tor._1, tor._2)(rule.lhs, rule.rhs)
      case None => false
    }

    while (updatedThisRun) {
      updatedThisRun = false
      for (i <- rulesAsMap.keys) {
        val rule = rulesAsMap(i)
        val otherRules = rulesAsMap.filterKeys(_ != i).values.toList.filter(!isomorphism(_)).map(
          rule => {
            if (comparison(rule.lhs, rule.rhs) < 0) {
              rule.inverse
            } else rule
          }
        )
        val newLhs: Graph = AutoReduce.greedyReduce(comparison, graphToDerivation(rule.lhs), otherRules)
        val newRhs: Graph = AutoReduce.greedyReduce(comparison, graphToDerivation(rule.rhs), otherRules)
        if (newLhs != rule.lhs || newRhs != rule.rhs) {
          rulesAsMap = rulesAsMap + (i -> new Rule(newLhs,
            newRhs,
            derivation = None,
            RuleDesc(rule.name + " reduced")
          ))
          updatedThisRun = true
        }
      }
    }

    rulesAsMap.values.toList
  }

  def discardDirectlyReducibleRules(comparison: GraphComparison,
                                    rules: List[Rule],
                                    seed: Random = new Random()): List[Rule] = {
    rules.filter(rule =>
      AutoReduce.greedyReduce(comparison, graphToDerivation(rule.lhs), rules.filter(r => r != rule)) >= rule.lhs
    ).filter(rule =>
      AutoReduce.greedyReduce(comparison, graphToDerivation(rule.rhs), rules.filter(r => r != rule)) >= rule.rhs
    )
  }

  def graphToDerivation(graph: Graph): DerivationWithHead = {
    (new Derivation(graph), None)
  }

}

/**
  * Automatically apply given rules to a given graph
  */

object AutoReduce {
  /**
    * Automatically reduce, with no handler or multithreading
    */

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
  def genericReduce(comparison: GraphComparison)
                   (derivationAndHead: DerivationWithHead,
                    rules: List[Rule],
                    seed: Random = new Random()):
  DerivationWithHead = {
    var latestDerivation: DerivationWithHead = derivationAndHead
    latestDerivation._2 match {
      case Some(initialHead) =>
        latestDerivation = annealingReduce(comparison, latestDerivation, rules, seed)
        latestDerivation = greedyReduce(comparison, latestDerivation, rules)
        latestDerivation = greedyReduce(comparison, (latestDerivation._1.addHead(initialHead), Some(initialHead)), rules)
      case None =>
        latestDerivation = annealingReduce(comparison, latestDerivation, rules, seed)
        latestDerivation = greedyReduce(comparison, latestDerivation, rules)
    }

    // Go back to original request, find smallest child
    (latestDerivation._1, smallestStepNameBelow(latestDerivation))
  }

  // Simplest entry point
  def annealingReduce(comparison: GraphComparison,
                      derivationHeadPair: DerivationWithHead,
                      rules: List[Rule],
                      seed: Random = new Random(),
                      vertexLimit: Option[Int] = None): DerivationWithHead = {
    val maxTime = 100 + math.pow(derivationHeadPair.verts.size, 2).toInt // Set as squaring #vertices for now
    val timeDilation = 3 // Gives an e^-3 ~ 0.05% chance of a non-reduction rule on the final step
    annealingReduce(comparison, derivationHeadPair, rules, maxTime, timeDilation, seed, vertexLimit)
  }

  // Enter here to have control over e.g. how long it runs for
  def annealingReduce(comparison: GraphComparison,
                      derivationHeadPair: DerivationWithHead,
                      rules: List[Rule],
                      maxTime: Int,
                      timeDilation: Double,
                      seed: Random,
                      vertexLimit: Option[Int]): DerivationWithHead = {
    (0 until maxTime).foldLeft(derivationHeadPair) { (d, time) =>
      val allowIncrease = seed.nextDouble() < math.exp(-timeDilation * time / maxTime)
      if (rules.nonEmpty) {
        val randRule = rules(seed.nextInt(rules.length))
        val suggestedNextStep = randomSingleApply(d, randRule, seed, None, None, None)
        val head = Derivation.derivationHeadPairToGraph(d)
        val smallEnough = vertexLimit.isEmpty || (head.verts.size < vertexLimit.get)
        if ((allowIncrease && smallEnough) || comparison(suggestedNextStep, head) < 0) suggestedNextStep else d
      } else d
    }
  }

  @tailrec
  def greedyReduce(comparison: GraphComparison,
                   derivationHeadPair: DerivationWithHead,
                   rules: List[Rule],
                   remainingRules: List[Rule]): DerivationWithHead = {
    remainingRules match {
      case r :: tailRules => Matcher.findMatches(r.lhs, derivationHeadPair) match {
        case ruleMatch #:: _ =>
          val reducedGraph = Rewriter.rewrite(ruleMatch, r.rhs)._1.minimise
          val stepName = quanto.data.Names.mapToNameMap(derivationHeadPair._1.steps).
            freshWithSuggestion(DSName(r.description.name))
          greedyReduce(comparison,
            (derivationHeadPair._1.addStep(
              derivationHeadPair._2,
              DStep(stepName, r, reducedGraph)
            ), Some(stepName)),
            rules,
            rules)
        case Stream.Empty =>
          greedyReduce(comparison, derivationHeadPair, rules, tailRules)
      }
      case Nil => derivationHeadPair
    }
  }

  // Simply apply the first reduction rule it can find until there are none left
  def greedyReduce(comparison: GraphComparison,
                   derivationHeadPair: DerivationWithHead,
                   rules: List[Rule]): DerivationWithHead = {
    val reducingRules = rules.filter(rule => rule.lhs > rule.rhs)
    val reduced = greedyReduce(comparison, derivationHeadPair, reducingRules, reducingRules)
    // Go round again until it stops reducing
    if (comparison(reduced, derivationHeadPair) < 0) {
      greedyReduce(comparison, reduced, rules)
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
          val suggestedUpdate = randomSingleApply(d, rules(seed.nextInt(rules.length)), seed, None, None, None)
          if (requirementToKeep(suggestedUpdate, d)) suggestedUpdate else d
        }
      }
    } else derivationWithHead
  }

  def randomSingleApply(derivationWithHead: DerivationWithHead,
                        rule: Rule,
                        seed: Random = new Random(),
                        restrictToVertices: Option[Set[VName]],
                        mustIncludeOneOf: Option[Set[VName]],
                        blockedVertices: Option[Set[VName]]): DerivationWithHead = {
    val matches = restrictToVertices match {
      case Some(vertexSet) => Matcher.findMatches(rule.lhs, derivationWithHead, vertexSet)
      case None => Matcher.findMatches(rule.lhs, derivationWithHead)
    }

    // If no restrictions then random from stream, otherwise calculates all options first

    val chosenMatch: Option[Match] = if (mustIncludeOneOf.nonEmpty || blockedVertices.nonEmpty) {
      matches.toList.filter(m =>
        (blockedVertices.isEmpty || !m.map.v.exists(vv => blockedVertices.get.contains(vv._2))) &&
          (mustIncludeOneOf.isEmpty || m.map.v.exists(vv => mustIncludeOneOf.get.contains(vv._2)))
      ).find(_ => seed.nextBoolean())
    } else matches.find(_ => seed.nextBoolean())

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