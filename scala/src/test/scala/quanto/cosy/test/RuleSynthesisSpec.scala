package quanto.cosy.test

import quanto.cosy._
import org.scalatest.FlatSpec
import quanto.data._
import quanto.rewrite.{Matcher, Rewriter}
import quanto.data.Derivation.DerivationWithHead
import quanto.util.json.Json

import quanto.cosy.RuleSynthesis._
import quanto.cosy.AutoReduce._

import scala.util.Random

/**
  * Created by hector on 28/06/17.
  */

class RuleSynthesisSpec extends FlatSpec {
  behavior of "Rule Synthesiser"

  implicit val rg: Theory = Theory.fromFile("red_green")

  var emptyRuleList: List[Rule] = List()
  var diagramStream = ColbournReadEnum.enumerate(2, 2, 2, 2)
  var results = EquivClassRunAdjMat(numAngles = 2,
    tolerance = EquivClassRunAdjMat.defaultTolerance,
    rulesList = emptyRuleList,
    theory = rg)
  results.findEquivalenceClasses(diagramStream.map(_.hash), "ColbournRead 2 2 2 2")

  it should "turn a class into rules" in {
    var ruleList = RuleSynthesis.graphEquivClassReduction[String](
      x => results.adjMatToGraph(AdjMat.fromHash(x)),
      results.equivalenceClasses.head,
      List()
    )
    println(ruleList.map(rule => "lhs = " + rule.lhs.toString + "\n" +
      "rhs = " + rule.rhs.toString + "\n"))
  }

  it should "make identity-wire rules" in {
    var idClass = results.equivalenceClasses.filter(e => e.centre.toString == "1 0 0 1")
    var ruleList = idClass.foldLeft(List[Rule]())(
      (a, b) => RuleSynthesis.graphEquivClassReduction[String](
        x => results.adjMatToGraph(AdjMat.fromHash(x)),
        b,
        List()
      ) ::: a
    )
    //println(idClass.head.members)
    var reductionRules = ruleList.filter(r => r.lhs > r.rhs)
    println(reductionRules.map(rule => "lhs = " + rule.lhs.toString + "\n" +
      "rhs = " + rule.rhs.toString + "\n"))
    assert(reductionRules.nonEmpty)
  }

  it should "discard reducible rules" in {
    var idClass = results.equivalenceClasses.filter(e => e.centre.toString == "1 0 0 1")
    var ruleList = idClass.foldLeft(List[Rule]())(
      (a, b) => RuleSynthesis.graphEquivClassReduction[String](
        x => results.adjMatToGraph(AdjMat.fromHash(x)),
        b,
        List()
      ) ::: a
    )
    var r1 = ruleList.head
    var m = Matcher.findMatches(r1.lhs, r1.lhs)
    var shrunkRules = RuleSynthesis.discardDirectlyReducibleRules(rules = ruleList, rg, seed = new Random(1))
    println(shrunkRules)
    assert(ruleList.length > shrunkRules.length)
  }

  behavior of "Bian and Perdrix 2Qubit system"

  it should "find small rules" in {
    var results = EquivClassRunBlockStack(1e-14)
    var rowsAllowed = BlockRowMaker(2, BlockRowMaker.Bian2Qubit, maxInOut = Option(2))
    var stacks = BlockStackMaker(2, rowsAllowed)
    stacks.foreach(s => results.add(s))
    results.equivalenceClassesNormalised
      .filter(cls => cls.members.map(x => x.toString).contains("( 1  x  1 )"))
      .foreach(cls => println(cls.members))
  }

  behavior of "ZX Qutrit system"

  it should "find small rules" in {
    var results = EquivClassRunBlockStack(1e-14)
    var rowsAllowed = BlockRowMaker(1, BlockRowMaker.ZXQutrit(3), maxInOut = Option(2))
    var stacks = BlockStackMaker(2, rowsAllowed)
    stacks.foreach(s => results.add(s))
    results.equivalenceClassesNormalised
      // .filter(cls => cls.members.map(x => x._1.toString).contains("( 1  x  1 )"))
      .foreach(cls => println(cls.members))
  }


  behavior of "ZX Qudit system"

  it should "find small rules" in {
    var results = EquivClassRunBlockStack(1e-14)
    var rowsAllowed = BlockRowMaker(1, BlockRowMaker.ZXQudit(4, 2), maxInOut = Option(2))
    var stacks = BlockStackMaker(2, rowsAllowed)
    stacks.foreach(s => results.add(s))
    results.equivalenceClasses
      // .filter(cls => cls.members.map(x => x._1.toString).contains("( 1  x  1 )"))
      .foreach(cls => println(cls.members))
  }

  behavior of "ZXClifford+T Reduction"

  it should "should greedy reduce" in {
    var ctRules = loadRuleDirectory("./examples/ZX_cliffordT")
    // Pick out S1, S2 and REDUCIBLE
    var smallRules = ctRules.filter(_.description.get.name.matches(raw"S\d|RED.*"))
    var reducibleGraph = smallRules.filter(_.description.get.name.matches(raw"RED.*")).head.lhs
    var resultingDerivation = greedyReduce(RuleSynthesis.graphToDerivation(reducibleGraph, rg), smallRules)
    // println(resultingDerivation.stepsTo(resultingDerivation.firstHead))
    assert(Derivation.derivationHeadPairToGraph(resultingDerivation).verts.size < reducibleGraph.verts.size)
  }

  it should "automatically reduce" in {
    var ctRules = RuleSynthesis.loadRuleDirectory("./examples/ZX_cliffordT")
    // Pick out S1, S2 and REDUCIBLE
    var smallRules = ctRules.filter(_.description.get.name.matches(raw"S\d"))
    var minimisedRules = RuleSynthesis.minimiseRuleset(smallRules ::: smallRules.map(_.inverse), rg)
    minimisedRules.foreach(println)
    assert(minimisedRules.exists(r => r.description.get.name.matches(raw".*reduced.*")))
  }

  it should "make a long derivation from annealing" in {
    var ctRules = loadRuleDirectory("./examples/ZX_cliffordT")
    var target = ctRules.filter(_.description.get.name.matches(raw"RED.*")).head.lhs
    var remaining = ctRules.filterNot(_.description.get.name.matches(raw"RED.*"))
    var annealed = annealingReduce(RuleSynthesis.graphToDerivation(target, rg),
      remaining ::: remaining.map(_.inverse),
      100,
      3,
      new Random(3))
    assert(annealed._1.steps.size > target.verts.size)
  }

}