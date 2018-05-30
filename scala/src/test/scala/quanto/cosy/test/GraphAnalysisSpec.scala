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
  val examplesDirectory = "../examples/"
  val ZXRules: List[Rule] = loadRuleDirectory(examplesDirectory + "ZX_CliffordT")
  val ZXErrorRules: List[Rule] = loadRuleDirectory(examplesDirectory + "ZX_errors")
  var emptyRuleList: List[Rule] = List()


  behavior of "Graph Analysis"


  private val errorGates = quanto.util.FileHelper.readFile[Graph](
    new File(examplesDirectory + "ZX_errors/ErrorGate.qgraph"),
    Graph.fromJson(_, rg)
  )

  it should "compute adjacency matrices" in {

    val adjacencyMatrix = GraphAnalysis.adjacencyMatrix(errorGates)
    // boundary b2, error v2, next gate vertex v3
    val bIndex = adjacencyMatrix._1.indexOf(VName("b2"))
    val eIndex = adjacencyMatrix._1.indexOf(VName("v2"))
    val vIndex = adjacencyMatrix._1.indexOf(VName("v3"))
    assert(adjacencyMatrix._2(bIndex)(eIndex))
    assert(!adjacencyMatrix._2(bIndex)(vIndex))


    val ghostedErrors = GraphAnalysis.bypassSpecial(GraphAnalysis.detectPiNodes)(errorGates, adjacencyMatrix)
    assert(ghostedErrors._2(bIndex)(eIndex))
    assert(ghostedErrors._2(bIndex)(vIndex))
  }

  it should "calculate distance from ends" in {

    val adjacencyMatrix = GraphAnalysis.adjacencyMatrix(errorGates)

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

    val adjacencyMatrix = GraphAnalysis.adjacencyMatrix(errorGates)

    val errorName = VName("v2")

    assert(GraphAnalysis.neighbours(adjacencyMatrix, errorName).size == 2)
  }


  it should "calculate distance of a given, ignored set from ends" in {

    val adjacencyMatrix = GraphAnalysis.adjacencyMatrix(errorGates)
    val ghostedErrors = GraphAnalysis.bypassSpecial(GraphAnalysis.detectPiNodes)(errorGates, adjacencyMatrix)

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

    val eDistances = SimplificationProcedure.PullErrors.errorsDistance(rightBoundaries)(errorGates, Set(errorName))

    assert(eDistances.get == 2.0)
  }

  behavior of "Graph comparison"

  it should "compare all these graphs" in {
    def compare(a: Graph, b: Graph) = GraphAnalysis.zxGraphCompare(a, b)

    implicit def stackToGraph(s: BlockStack) : Graph = s.graph
    implicit def rowToGraph(s: BlockRow) : Graph = s.graph
    implicit def blockToGraph(s: Block) : Graph = s.graph

    def zx(s: String) : Graph = BlockGenerators.ZXClifford.filter(b => b.name == s).head

    val hadamard = zx(" H ")
    val id = zx(" 1 ")

    assert(compare(hadamard, id) > 0)
  }

}
