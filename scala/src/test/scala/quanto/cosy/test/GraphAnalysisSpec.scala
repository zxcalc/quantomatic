package quanto.cosy.test

import java.io.File

import org.scalatest.FlatSpec
import quanto.cosy.RuleSynthesis._
import quanto.cosy._
import quanto.data._
import quanto.util.Rational

/**
  * Test files for the Graph Analysis Object
  * It is used to calculate various graph properties
  */

class GraphAnalysisSpec extends FlatSpec {


  val rg: Theory = Theory.fromFile("red_green")
  val examplesDirectory = "./examples/"
  val ZXRules: List[Rule] = loadRuleDirectory(examplesDirectory + "ZX_CliffordT")
  val ZXErrorRules: List[Rule] = loadRuleDirectory(examplesDirectory + "ZX_errors")
  var emptyRuleList: List[Rule] = List()


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


    val ghostedErrors = GraphAnalysis.bypassSpecial(GraphAnalysis.detectErrors)(targetGraph, adjacencyMatrix)
    assert(ghostedErrors._2(bIndex)(eIndex))
    assert(ghostedErrors._2(bIndex)(vIndex))
  }

  it should "calculate distance from ends" in {

    val targetGraph = quanto.util.FileHelper.readFile[Graph](
      new File(examplesDirectory + "ZX_errors/ErrorGate.qgraph"),
      Graph.fromJson(_, rg)
    )
    val adjacencyMatrix = GraphAnalysis.adjacencyMatrix(targetGraph)

    val errorName = VName("v2")
    val leftBoundary = VName("b2")
    val rightBoundaries = Set(VName("b3"), VName("b4"), VName("b5"))

    // boundary b2, error v2, next gate vertex v3
    val bIndex = adjacencyMatrix._1.indexOf(VName("b2"))
    val eIndex = adjacencyMatrix._1.indexOf(VName("v2"))
    val vIndex = adjacencyMatrix._1.indexOf(VName("v3"))


    val distances = GraphAnalysis.distancesFromInitial(adjacencyMatrix, Set(errorName), rightBoundaries)

    assert(distances(errorName) == 2)
    assert(distances(leftBoundary) == 3)
    assert(distances(VName("v3")) == 2)
  }


  it should "find neighbours" in {

    val targetGraph = quanto.util.FileHelper.readFile[Graph](
      new File(examplesDirectory + "ZX_errors/ErrorGate.qgraph"),
      Graph.fromJson(_, rg)
    )
    val adjacencyMatrix = GraphAnalysis.adjacencyMatrix(targetGraph)

    val errorName = VName("v2")

    assert(GraphAnalysis.neighbours(adjacencyMatrix, errorName).size == 2)
  }


  it should "calculate distance of a given, ignored set from ends" in {

    val targetGraph = quanto.util.FileHelper.readFile[Graph](
      new File(examplesDirectory + "ZX_errors/ErrorGate.qgraph"),
      Graph.fromJson(_, rg)
    )
    val adjacencyMatrix = GraphAnalysis.adjacencyMatrix(targetGraph)
    val ghostedErrors = GraphAnalysis.bypassSpecial(GraphAnalysis.detectErrors)(targetGraph, adjacencyMatrix)

    val errorName = VName("v2")
    val leftBoundary = VName("b2")
    val rightBoundaries = Set(VName("b3"), VName("b4"), VName("b5"))

    val names = ghostedErrors._1

    def getIndex(name: VName) = names.indexOf(name)

    // boundary b2, error v2, next gate vertex v3
    val bIndex = ghostedErrors._1.indexOf(VName("b2"))
    val eIndex = ghostedErrors._1.indexOf(VName("v2"))
    val vIndex = ghostedErrors._1.indexOf(VName("v3"))


    val distances = GraphAnalysis.distancesFromInitial(ghostedErrors, Set(errorName), rightBoundaries)
    val distance = GraphAnalysis.pathDistanceSet(
      ghostedErrors,
      Set(errorName).map(getIndex),
      rightBoundaries.map(getIndex)
    )

    assert(distance.get == 3.0)

    // Now check with the simproc methods

    val eDistances = SimplificationProcedure.PullErrors.errorsDistance(rightBoundaries)(targetGraph, Set(errorName))

    assert(eDistances.get == 2.0)
  }

}
