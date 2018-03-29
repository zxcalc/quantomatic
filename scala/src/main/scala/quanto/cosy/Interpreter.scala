package quanto.cosy

import quanto.data.Theory.ValueType
import quanto.data._

/**
  * An interpreter is given a diagram (as an adjMat and variable assignment) and returns a tensor
  */

object Interpreter {
  // Converts a given graph or spider into tensor form
  type cachedSpiders = collection.mutable.Map[String, Tensor]
  type AngleMap = Int => Double
  val cached: cachedSpiders = collection.mutable.Map.empty[String, Tensor]

  def makeHadamards(n: Int, current: Tensor = Tensor.id(1)): Tensor = n match {
    // generates the n-fold tensor products of Hadamard maps
    case 0 => Tensor.id(1)
    case _ => Tensor.hadamard x makeHadamards(n - 1, current)
  }

  case class ZXAngleData(isGreen: Boolean, angle: Double)

  // ASSUME EVERYTHING HAS ANGLE DATA
  implicit def pullOutAngleData(composite: CompositeExpression): PhaseExpression = {
    composite.firstOrError(ValueType.AngleExpr)
  }

  def interpretZXSpider(zxAngleData: ZXAngleData, inputs: Int, outputs: Int): Tensor = {
    // Converts spider to tensor. If green==false then it is a red spider

    val colour = if(zxAngleData.isGreen){"green"}else{"red"}
    val angle = zxAngleData.angle
    val toString = s"ZX:$colour:$angle:$inputs:$outputs"


    if (cached.contains(toString)) cached(toString) else {
      def gen(i: Int, j: Int): Complex = {
        Complex.zero +
          (if (i == 0 && j == 0) Complex.one else Complex.zero) +
          (if (i == math.pow(2, outputs) - 1 && j == math.pow(2, inputs) - 1)
            Complex(math.cos(angle), math.sin(angle)) else Complex.zero)
      }

      val mid = Tensor(math.pow(2, outputs).toInt, math.pow(2, inputs).toInt, gen)
      val spider = if (zxAngleData.isGreen) mid else makeHadamards(outputs) o mid o makeHadamards(inputs)
      cached += (toString -> spider)
      spider
    }
  }

  def interpretZWSpider(black: Boolean, outputs: Int): Tensor = {
    require(outputs >= 0)
    val toString = "ZW:" + black.toString + ":" + outputs
    val spider = if (cached.contains(toString)) cached(toString) else {
      if (black) {
        outputs match {
          case 0 => Tensor(Array(Array(0)))
          case 1 => Tensor(Array(Array(0, 1))).transpose
          case 2 => Tensor(Array(Array(0, 1, 1, 0))).transpose
          case 3 => Tensor(Array(Array(0, 1, 1, 0, 1, 0, 0, 0))).transpose
          case _ =>
            val bY = Tensor(Array(Array(0, 1, 1, 0), Array(1, 0, 0, 0))).transpose
            val base = interpretZWSpider(black, outputs - 1)
            (Tensor.idWires(outputs - 2) x bY) o base
        }
      } else {
        outputs match {
          case 0 => Tensor(Array(Array(0)))
          case _ =>
            Tensor(1,
              Math.pow(2, outputs).toInt,
              (_, j) => (if (j == 0) Complex.one else Complex.zero) -
                (if (j == Math.pow(2, outputs) - 1) Complex.one else Complex.zero)).transpose
        }
      }
    }

    cached += (toString -> spider)
    spider
  }

  def interpretZXAdjMat(adjMat: AdjMat, greenAM: Vector[NodeV], redAM: Vector[NodeV]): Tensor = {
    interpretZXAdjMatSpidersFirst(adjMat, greenAM, redAM)
  }

  def interpretZWAdjMat(adjMat: AdjMat): Tensor = interpretZWAdjMatSpidersFirst(adjMat)

  def interpretZXGraph(graph: Graph): Tensor = {

    // remove any wire vertices etc
    val minGraph = graph.minimise

    def spiderInterpreter(vdata: NodeV, inputs: Int, outputs: Int): Tensor = {

      val zxData: ZXAngleData = {
        val isGreen = vdata.typ == "Z"
        val angle =  vdata.value.toDouble
        ZXAngleData(isGreen, angle)
      }

      interpretZXSpider(zxData, inputs, outputs)
    }


    minGraph.vdata.count(nd => !nd._2.isWireVertex) match {
      case 0 =>
        stringGraph(minGraph, interpretZXSpider(ZXAngleData(isGreen = true,0), 2, 0))
      case _ =>
        pullOutVertexGraph(minGraph, interpretZXGraph, spiderInterpreter)
    }
  }

  /**
    * Given a graph of just boundaries and edges,
    * return the tensor treating those edges as caps
    *
    * @param graph : Graph
    * @return
    */
  private def stringGraph(graph: Graph, cap: Tensor): Tensor = {
    if (graph.verts.size % 2 != 0) {
      throw new Error("String graph should have an even number of boundaries")
    }
    val numVerts = graph.verts.size
    val caps = cap.power(numVerts / 2)
    val nameVector = graph.verts.toVector.sorted

    def toNeighbour(place: Int): Int = {
      val name = nameVector(place)
      val neighbour = graph.adjacentVerts(name).head
      if (name < neighbour) {
        2 * place
      } else {
        2 * nameVector.indexOf(neighbour) + 1
      }
    }

    val sigma = if (numVerts > 0) {
      Tensor.swap(numVerts, toNeighbour)
    } else {
      Tensor(Array(Array(Complex(1,0))))
    }

    caps o sigma
  }

  private def pullOutVertexGraph(startingGraph: Graph,
                                 graphInterpreter: Graph => Tensor,
                                 spiderInterpreter: (NodeV, Int, Int) => Tensor): Tensor = {
    def ratioDanglingWires(name: VName): Double = {
      // A measure of how many boundary vs non-boundary neighbours the node has
      val numBoundary = startingGraph.adjacentVerts(name).count(vn => startingGraph.isTerminalWire(vn))
      (1 + numBoundary).toDouble / (1 + startingGraph.adjacentVerts(name).size - numBoundary)
    }

    def boundaries(g: Graph) : Set[VName] = g.verts.filter(g.isTerminalWire)

    // Pick a vertex to shift from graph to tensor
    val cutVertex = startingGraph.verts.filterNot(vn => startingGraph.vdata(vn).isWireVertex).maxBy(ratioDanglingWires)
    val numCutSpiderInOuts = startingGraph.adjacentVerts(cutVertex).size

    // cut out that vertex, as well as any boundaries it was attached to
    val (reducedGraph, cutSpiderOutputJoins) = startingGraph.cutVertex(cutVertex)
    val startingGraphBoundaries = boundaries(startingGraph)
    val reducedGraphBoundaries = boundaries(reducedGraph)

    val spiderTensor = spiderInterpreter(
      startingGraph.vdata(cutVertex).asInstanceOf[NodeV],
      numCutSpiderInOuts - cutSpiderOutputJoins.size,
      cutSpiderOutputJoins.size)

    val reducedGraphBoundariesVector: Vector[VName] = reducedGraphBoundaries.toVector.sorted
    val uncutBoundariesVector: Vector[VName] = (reducedGraphBoundaries -- cutSpiderOutputJoins).toVector.sorted
    val allInputsVector: Vector[VName] = startingGraphBoundaries.toVector.sorted

    val bottomSigma: Tensor = {
      val swapList :List[Int] = {
        var leftCount = 0
        var rightCount = (reducedGraphBoundaries -- cutSpiderOutputJoins).size
        allInputsVector.indices.map(i =>
          if (reducedGraphBoundaries.contains(allInputsVector(i))) {
              leftCount += 1
              leftCount - 1
            } else {
              rightCount += 1
              rightCount - 1
            }
        ).toList
      }
      Tensor.swap(allInputsVector.size, swapList)
    }

    val topSigma: Tensor = {
      val inputs: Vector[VName] = uncutBoundariesVector ++ cutSpiderOutputJoins.toVector
      Tensor.swap(reducedGraphBoundaries.size,
        i => reducedGraphBoundariesVector.indexOf(inputs(i)))
    }

    /**
      * Starting with the graph G, pulling out the spider S
      * You want:
      *
      * [     G'    ]
      * [ topSigma ]
      * [id]  x [S]
      * [ botSigma ]
      *
      * Where G' is the reduced graph, with vector of inputs: reducedGraphBoundariesVector
      * S is the cut spider,
      * the identity is on uncutBoundariesVector
      * And the bottom sigma acts on allInputsVector
      */

    {
      reducedGraphBoundariesVector.length == (uncutBoundariesVector.length + cutSpiderOutputJoins.size)
      (uncutBoundariesVector.length + numCutSpiderInOuts - cutSpiderOutputJoins.size) == allInputsVector.length
    }

    graphInterpreter(reducedGraph) o
      topSigma o
      (Tensor.idWires(uncutBoundariesVector.size) x spiderTensor) o
      bottomSigma
  }

  private def interpretAdjMat(adj: AdjMat, join: Tensor, vertexToTensor: (Int) => Tensor): Tensor = {
    // Interpret the graph as (caps) o (crossings) o (vertices)
    if (adj.size == 0) {
      Tensor.id(1)
    } else {
      val numVertices = adj.mat(0).length
      var vertexNextEdge = Map[Int, Int]()
      var edgesFilled = 0

      // For every vertex register which inputs/outputs of the tensor it connects to
      def registerLegs(vertexID: Int): Unit = {
        vertexNextEdge = vertexNextEdge + (vertexID -> edgesFilled)
        edgesFilled += numConnections(vertexID)
      }

      def numConnections(vertexID: Int): Int = {
        adj.mat(vertexID).map(x => if (x) 1 else 0).sum
      }

      def claimLeg(vID: Int): Int = {
        val leg = vertexNextEdge(vID)
        vertexNextEdge = vertexNextEdge updated(vID, leg + 1)
        leg
      }

      for (v <- 0 until numVertices) registerLegs(v)
      val listTensors = for (v <- 0 until numVertices) yield vertexToTensor(v)
      val allSpidersTensors = listTensors.foldLeft(Tensor.id(1))((a, b) => a x b)
      var connectingCaps: Tensor = Tensor.id(1)
      var connectionList: List[Int] = List()
      for (i <- 0 until numVertices; j <- i until numVertices) {
        if (adj.mat(i)(j)) {
          connectingCaps = connectingCaps x join
          connectionList = connectionList ++ List(claimLeg(i), claimLeg(j))
        }
      }
      connectingCaps.plugAbove(allSpidersTensors, connectionList)
    }
  }

  private def interpretZXAdjMatSpidersFirst(adj: AdjMat, greenAM: Vector[NodeV], redAM: Vector[NodeV]): Tensor = {
    // Interpret the graph as (caps) o (crossings) o (vertices)
    if (adj.size == 0) {
      Tensor.id(1)
    } else {
      // Tensor representation of a spider
      def vertexToSpider(v: Int): Tensor = {
        def pullOutAngle(nv: NodeV) = if (!nv.value.isEmpty) {
          try {
            nv.value.toDouble
          } catch {
            case _: Error => nv.phaseData.evaluate(Map("pi" -> 1)) * math.Pi
          }
        } else {
          nv.phaseData.evaluate(Map("pi" -> 1)) * math.Pi
        }

        val (colour, nodeType) = adj.vertexColoursAndTypes(v)
        val numLegs = adj.mat(v).count(p => p)
        colour match {
          case VertexColour.Boundary => Tensor.id(2)
          case VertexColour.Green => interpretZXSpider(
            ZXAngleData(isGreen = true, pullOutAngle(greenAM(nodeType))), 0, numLegs)
          case VertexColour.Red => interpretZXSpider(
            ZXAngleData(isGreen = false, pullOutAngle(redAM(nodeType))), 0, numLegs)
        }

      }

      val cap = interpretZXSpider(ZXAngleData(isGreen = true,0), 2, 0)

      interpretAdjMat(adj, cap, vertexToSpider)
    }
  }

  private def interpretZWAdjMatSpidersFirst(adj: AdjMat): Tensor = {
    val cup = Tensor(Array(Array(1, 0, 0, 1)))

    def vertexToSpider(v: Int): Tensor = {
      val numLegs = adj.mat(v).count(p => p)
      val (colour, _) = adj.vertexColoursAndTypes(v)
      val black = true
      // Using ZX colours for now
      colour match {
        case VertexColour.Boundary => Tensor.id(2)
        case VertexColour.Green => interpretZWSpider(!black, numLegs)
        case VertexColour.Red => interpretZWSpider(black, numLegs)
      }
    }

    interpretAdjMat(adj, cup, vertexToSpider)
  }
}
