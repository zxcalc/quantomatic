package quanto.cosy.test

import java.io.File

import org.scalatest.FlatSpec
import quanto.cosy.RuleSynthesis._
import quanto.cosy._
import quanto.data._
import quanto.util.Rational

/**
  * Test files for the EQCAnalysis class
  * It should take equivalence classes and return results about that class as a whole
  */
class EQCAnalysisSpec extends FlatSpec {
  behavior of "Connected Components"

  val rg: Theory = Theory.fromFile("red_green")
  val examplesDirectory = "../examples/"
  val ZXRules: List[Rule] = loadRuleDirectory(examplesDirectory + "ZX_CliffordT")
  val ZXErrorRules: List[Rule] = loadRuleDirectory(examplesDirectory + "ZX_errors")
  var emptyRuleList: List[Rule] = List()


  it should "find a 1-colour adjmat" in {
    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(true))
    assert(EQCAnalysis.AdjMatConnectedComponents(amat) == 1)
  }

  it should "find a 2-colour adjmat" in {
    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false))
    assert(EQCAnalysis.AdjMatConnectedComponents(amat) == 2)
  }

  it should "accept an equivalence class" in {
    var results = EquivClassRunAdjMat(numAngles = 2,
      tolerance = EquivClassRunAdjMat.defaultTolerance,
      rulesList = emptyRuleList,
      theory = rg)
    var diagramStream = ColbournReadEnum.enumerate(2, 2, 2, 2)
    results.findEquivalenceClasses(diagramStream.map(_.hash), "ColbournRead 2 2 2 2")
    var eqcConCom = results.equivalenceClassesNormalised.map(
      e => EQCAnalysis.AdjMatConnectedComponents(EquivalenceClassByAdjMat.fromEC(e, rg))
    )
    println(eqcConCom)
  }

  behavior of "Graph Analysis"

  it should "compute adjacency matrices" in {

    val targetGraph = quanto.util.FileHelper.readFile[Graph](
      new File(examplesDirectory + "ZX_errors/ErrorGate.qgraph"),
      Graph.fromJson(_, rg)
    )
    val adjacencyMatrix = GraphAnalysis.adjacencyMatrix(targetGraph)
    // boundary b2, error v2, next gate vertex v3
    val bIndex = adjacencyMatrix._1.indexOf(VName("b2"))
    val eIndex = adjacencyMatrix._1.indexOf(VName("v2"))
    val vIndex = adjacencyMatrix._1.indexOf(VName("v3"))
    assert(adjacencyMatrix._2(bIndex)(eIndex))
    assert(!adjacencyMatrix._2(bIndex)(vIndex))


    val ghostedErrors = GraphAnalysis.bypassSpecial(GraphAnalysis.detectPiNodes)(targetGraph, adjacencyMatrix)
    assert(ghostedErrors._2(bIndex)(eIndex))
    assert(ghostedErrors._2(bIndex)(vIndex))
  }
}
