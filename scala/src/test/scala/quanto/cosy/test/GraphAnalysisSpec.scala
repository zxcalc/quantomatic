package quanto.cosy.test

import java.io.File

import org.scalatest.FlatSpec
import quanto.cosy.RuleSynthesis._
import quanto.cosy.GraphAnalysis._
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

  behavior of "Connectiviy analysis"

  it should "leave empty graph disconnected" in {
    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.addVertex(Vector(false, false))
    val cc = connectionClasses(amat)
    assert(cc == (0 until 3).toVector)
  }


  it should "connect all in line graph" in {
    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(true))
    amat = amat.addVertex(Vector(false, true))
    val cc = connectionClasses(amat)
    assert(cc == Vector(0,0,0))
  }


  it should "find two classes" in {
    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.addVertex(Vector(true, false))
    val cc = connectionClasses(amat)
    assert(cc == Vector(0,1,0))
  }

  it should "detect no scalars in all boundaries" in {
    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.addVertex(Vector(true, false))
    assert(!containsScalars(amat))
  }

  it should "detect no scalars with some non-boundaries" in {
    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.addVertex(Vector(true, false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, true, false))
    assert(!containsScalars(amat))
  }


  it should "detect scalars with some non-boundaries" in {
    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.addVertex(Vector(true, false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, false, false))
    amat = amat.addVertex(Vector(false, false, false, true))
    assert(containsScalars(amat))
  }

  behavior of "circuit analysis"

  // Only works for circuits generated inside CoSy
  // Note that this will not magically give you reductions - it is not graph-invariant by circuit-invariant
  // Turning a graph upside down gives very a very different measure

  it should "distill circuit placement from name via regex" in {
    val example = "r-2-bl-1-h-1"
    val output = CircuitPlacementParser.p(example)
    assert (output == (2,1,"h"))
  }

  it should "bias circuits to the left" in {
    val blocks: List[Block] = BlockGenerators.ZXGates(1)
    val rows: List[BlockRow] = BlockRowMaker.makeRowsUpToSize(2, blocks, Some(2))
    val stacks = BlockStackMaker.makeStacksOfSize(1, rows)
    val e1 = stacks.find(_.toString == "( 1  x 0Z1)").get
    val e2 = stacks.find(_.toString == "(0Z1 x  1 )").get
    assert(zxCircuitCompare(e1.graph, e2.graph) > 0)
  }

  it should "bias circuits down" in {
    val blocks: List[Block] = BlockGenerators.ZXGates(1)
    val rows: List[BlockRow] = BlockRowMaker.makeRowsUpToSize(1, blocks, Some(1))
    val stacks = BlockStackMaker.makeStacksOfSize(2, rows)
    val e1 = stacks.find(_.toString == "( 1 ) o (0Z1)").get
    val e2 = stacks.find(_.toString == "(0Z1) o ( 1 )").get
    assert(zxCircuitCompare(e1.graph, e2.graph) < 0)
  }

}
