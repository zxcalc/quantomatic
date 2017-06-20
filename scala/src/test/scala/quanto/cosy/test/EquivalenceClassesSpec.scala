package quanto.cosy.test

import java.io.File
import java.nio.file.Paths

import org.scalatest._
import quanto.cosy._
import quanto.data._

/**
  * Created by hector on 02/06/17.
  */
class EquivalenceClassesSpec extends FlatSpec {
  val rg = Theory.fromFile("red_green")
  var emptyRuleList: List[Rule] = List()
  var results = EquivClassRunResults(    numAngles = 4,
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
    assert(results.equivalenceClasses.map(x => x.members.length).sum ==
      results.equivalenceClassesNormalised.map(x => x.members.length).sum)
  }

  it should "convert an AdjMat into a graph" in {
    var diagramStream = ColbournReadEnum.enumerate(2, 2, 2, 2)
    results.findEquivalenceClasses(diagramStream, "ColbournRead 2 2 2 2")
    var ten = results.equivalenceClasses.head.centre
    var graph = results.equivalenceClasses.head.members.head._1
    println(graph.toString)
  }

  it should "have a JSON output" in {
    var results = EquivClassRunResults(
      numAngles = 4,
      tolerance = EquivClassRunResults.defaultTolerance,
      rulesList = emptyRuleList,
      theory = rg)
    var diagramStream = ColbournReadEnum.enumerate(2, 2, 2, 2)
    results.findEquivalenceClasses(diagramStream, "ColbournRead 2 2 2 2")
    println(results.toJSON.toString())
  }

  behavior of "multiple runs"

  it should "accept multiple runs" in {
    var results1 = EquivClassRunResults(
      numAngles = 4,
      tolerance = EquivClassRunResults.defaultTolerance,
      rulesList = emptyRuleList,
      theory = rg)
    var diagramStream1 = ColbournReadEnum.enumerate(2, 2, 2, 2)
    var diagramStream2 = ColbournReadEnum.enumerate(1, 1, 3, 2)
    results1.findEquivalenceClasses(diagramStream1, "ColbournRead 2 2 2 2")
    results1.findEquivalenceClasses(diagramStream2, "ColbournRead 1 1 3 2")
    println(results1.toJSON)
  }

  it should "have results independent of run order" in {
    var results1 = EquivClassRunResults(
      numAngles = 4,
      tolerance = EquivClassRunResults.defaultTolerance,
      rulesList = emptyRuleList,
      theory = rg)
    var results2 = EquivClassRunResults(
      numAngles = 4,
      tolerance = EquivClassRunResults.defaultTolerance,
      rulesList = emptyRuleList,
      theory = rg)
    var diagramStream1 = ColbournReadEnum.enumerate(2, 2, 2, 2)
    var diagramStream2 = ColbournReadEnum.enumerate(1, 1, 3, 2)
    results1.findEquivalenceClasses(diagramStream1, "ColbournRead 2 2 2 2")
    results1.findEquivalenceClasses(diagramStream2, "ColbournRead 1 1 3 2")
    results2.findEquivalenceClasses(diagramStream2, "ColbournRead 1 1 3 2")
    results2.findEquivalenceClasses(diagramStream1, "ColbournRead 2 2 2 2")
    println(results1.equivalenceClasses.toSet == results2.equivalenceClasses.toSet)
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

    var resultsWithOneRule = EquivClassRunResults(
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

  it should "output to and input from JSON" in {
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

    var resultsWithOneRule = EquivClassRunResults(
      numAngles = 4,
      tolerance = EquivClassRunResults.defaultTolerance,
      rulesList = List(singleRedRule),
      theory = rg)
    var diagramStream = ColbournReadEnum.enumerate(2, 2, 2, 2)
    resultsWithOneRule.findEquivalenceClasses(diagramStream, "ColbournRead 2 2 2 2")
    var jsonOutput = resultsWithOneRule.toJSON
    var madeFromJSON = EquivClassRunResults.fromJSON(jsonOutput.asObject)
    assert(madeFromJSON.equivalenceClasses.toSet == resultsWithOneRule.equivalenceClasses.toSet)
    assert(madeFromJSON.equivalenceClassesNormalised.toSet ==
      resultsWithOneRule.equivalenceClassesNormalised.toSet)
    assert(madeFromJSON.messageList == resultsWithOneRule.messageList)
    assert(madeFromJSON.rulesList == resultsWithOneRule.rulesList)
  }

  behavior of "batch runner"

  it should "create an output file" in {
    EquivClassBatchRunner(4,2,2,"test.qrun")
    var testFile = new File(Paths.get(EquivClassBatchRunner.outputPath,"test.qrun").toString)
    assert(testFile.exists())
    assert(testFile.delete())
  }

  it should "allow outputs to home directory" in {
    EquivClassBatchRunner.outputPath = Paths.get( System.getProperty("user.home"), "cosy_synth").toString
    println(EquivClassBatchRunner.outputPath)
    EquivClassBatchRunner.outputPath = "cosy_synth" // reset to avoid problems in later tests
  }

}
