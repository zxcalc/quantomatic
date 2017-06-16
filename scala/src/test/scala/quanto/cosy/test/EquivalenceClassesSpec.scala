package quanto.cosy.test

import java.io.File

import org.scalatest._
import quanto.cosy._
import quanto.data._

/**
  * Created by hector on 02/06/17.
  */
class EquivalenceClassesSpec extends FlatSpec {
  val rg = Theory.fromFile("red_green")
  var emptyRuleList: List[Rule] = List()
  var results = EquivClassRunResults(normalised = false,
    numAngles = 4,
    tolerance = EquivClassRunResults.defaultTolerance,
    rulesList = emptyRuleList,
    theory = rg)
  var resultsNormalised = EquivClassRunResults(normalised = true,
    numAngles = 4,
    tolerance = EquivClassRunResults.defaultTolerance,
    rulesList = emptyRuleList,
    theory = rg)

  behavior of "Equivalence classes"

  it should "generate some diagrams" in {
    var diagramStream = ColbournReadEnum.enumerate(1, 1, 1, 1)
    assert(diagramStream.nonEmpty)
  }

  it should "result in the same number of diagrams when normalising things" in {
    var diagramStream = ColbournReadEnum.enumerate(2, 2, 2, 2)
    results.findEquivalenceClasses(diagramStream, "ColbournRead 2 2 2 2")
    resultsNormalised.findEquivalenceClasses(diagramStream, "ColbournRead 2 2 2 2")
    assert(results.equivalenceClasses.map(x => x.members.length).sum ==
      resultsNormalised.equivalenceClasses.map(x => x.members.length).sum)
  }

  it should "convert an AdjMat into a graph" in {
    var diagramStream = ColbournReadEnum.enumerate(2, 2, 2, 2)
    results.findEquivalenceClasses(diagramStream, "ColbournRead 2 2 2 2")
    var (adj, ten) = results.equivalenceClasses.head.centre
    var graph = results.adjMatToGraph(adj)
    println(graph.toString)
  }

  it should "have a JSON output" in {
    var results = EquivClassRunResults(normalised = false,
      numAngles = 4,
      tolerance = EquivClassRunResults.defaultTolerance,
      rulesList = emptyRuleList,
      theory = rg)
    var diagramStream = ColbournReadEnum.enumerate(2, 2, 2, 2)
    results.findEquivalenceClasses(diagramStream, "ColbournRead 2 2 2 2")
    println(results.toJSON.toString())
  }

  behavior of "Interaction with Rules"

  it should "accept a list of rules" in {
    var amat = new AdjMat(numRedTypes = 2, numGreenTypes = 0)
    // no boundaries
    amat = amat.nextType.get
    amat = amat.nextType.get
    // single red_1
    amat = amat.addVertex(Vector())
    // LHS of rule is a disconnected red dot of value 0
    var lhsG = results.adjMatToGraph(amat)
    var singleRedRule = new Rule(lhs = lhsG, rhs = lhsG)
    println("Rule is " + singleRedRule.toString)

    var resultsWithOneRule = EquivClassRunResults(normalised = false,
      numAngles = 4,
      tolerance = EquivClassRunResults.defaultTolerance,
      rulesList = List(singleRedRule),
      theory = rg)
    var diagramStream = ColbournReadEnum.enumerate(2, 2, 2, 2)
    resultsWithOneRule.findEquivalenceClasses(diagramStream, "ColbournRead 2 2 2 2")
    println(resultsWithOneRule.toJSON.toString())
  }

  behavior of "IO"

  it should "save run results to file" in {
    var testFile = new File("test_run_output.qrun")
    quanto.util.FileHelper.printToFile(testFile, append = false)(
      p => p.println(results.toJSON.toString())
    )
    assert(testFile.delete())
  }

}
