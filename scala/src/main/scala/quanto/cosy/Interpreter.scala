package quanto.cosy

import quanto.data.Theory.ValueType
import quanto.data._
import quanto.util.json.JsonObject

/**
  * An interpreter is given a diagram (as an adjMat and variable assignment) and returns a tensor
  */

object Interpreter {
  // Converts a given graph or spider into tensor form
  private type cachedSpiders = collection.mutable.Map[String, Tensor]
  val cached: cachedSpiders = collection.mutable.Map.empty[String, Tensor]

  def makeHadamards(n: Int, current: Tensor = Tensor.id(1)): Tensor = n match {
    // generates the n-fold tensor products of Hadamard maps
    case 0 => Tensor.id(1)
    case _ => Tensor.hadamard x makeHadamards(n - 1, current)
  }

  def interpretZXSpider(zxAngleData: ZXAngleData, inputs: Int, outputs: Int): Tensor = {
    // Converts spider to tensor. If green==false then it is a red spider

    val colour = if (zxAngleData.isGreen) {
      "green"
    } else {
      "red"
    }
    val angle = zxAngleData.angle.constant
    val toString = s"ZX:$colour:$angle:$inputs:$outputs"


    if (cached.contains(toString)) cached(toString) else {
      def gen(i: Int, j: Int): Complex = {
        Complex.zero +
          (if (i == 0 && j == 0) Complex.one else Complex.zero) +
          (if (i == math.pow(2, outputs) - 1 && j == math.pow(2, inputs) - 1)
            Complex(math.cos(angle * math.Pi), math.sin(angle * math.Pi)) else Complex.zero)
      }

      val mid = Tensor(math.pow(2, outputs).toInt, math.pow(2, inputs).toInt, gen)
      val spider = if (zxAngleData.isGreen) mid else makeHadamards(outputs) o mid o makeHadamards(inputs)
      cached += (toString -> spider)
      spider
    }
  }


  implicit def stringToPhase(s: String): PhaseExpression = {
    PhaseExpression.parse(s, ValueType.AngleExpr)
  }

  // ASSUME EVERYTHING HAS ANGLE DATA
  implicit def pullOutAngleData(composite: CompositeExpression): PhaseExpression = {
    composite.firstOrError(ValueType.AngleExpr)
  }

  def interpretZWSpiderNoInputs(black: Boolean, outputs: Int): Tensor = {
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
            val base = interpretZWSpiderNoInputs(black, outputs - 1)
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

  def zwSpiderInterpreter(nodeV: NodeV, inputs: Int, outputs: Int) : Tensor = {
    (inputs, outputs) match {
      case (0, 0) => Tensor.id(1)
      case (0, n) => interpretZWSpiderNoInputs( nodeV.typ.toLowerCase == "w",n)
      case (m, n) =>
        (Tensor.idWires(n) x Tensor(Array(Array(1, 0, 0, 1)))) o
          (zwSpiderInterpreter(nodeV, m-1, n+1) x Tensor.idWires(1))
    }
  }

  def interpretZWAdjMat(adjMat: AdjMat, inputs: List[VName], outputs: List[VName]): Tensor = {
    val blackNodes = Vector(
      NodeV(
        JsonObject(
          "type" -> "w",
          "value" -> ""
        )
      )
    )
    val whiteNodes = Vector(
      NodeV(
        JsonObject(
          "type" -> "z",
          "value" -> "1"
        )
      )
    )
    val graph = Graph.fromAdjMat(adjMat, blackNodes, whiteNodes)
    interpretSpiderGraph(zwSpiderInterpreter)(graph, inputs, outputs)
  }

  def interpretSpiderGraph(spiderInterpreter: (NodeV, Int, Int) => Tensor)(graph: Graph,  inputList: List[VName], outputList: List[VName]): Tensor = {

    // remove any wire vertices etc
    val minGraph = graph.minimise

    minGraph.vdata.count(nd => !nd._2.isWireVertex) match {
      case 0 =>
        stringGraph(minGraph, interpretZXSpider(
          ZXAngleData(isGreen = true, PhaseExpression.zero(ValueType.AngleExpr)), 2, 0),
          inputList,
          outputList
        )
      case _ =>
        pullOutVertexGraph(minGraph, inputList, outputList, interpretSpiderGraph(spiderInterpreter), spiderInterpreter)
    }
  }

  def zxSpiderInterpreter(vdata: NodeV, inputs: Int, outputs: Int): Tensor = {

    val zxData: ZXAngleData = {
      val isGreen = vdata.typ == "Z"
      val angle = PhaseExpression.parse(vdata.value, ValueType.AngleExpr)
      ZXAngleData(isGreen, angle)
    }

    interpretZXSpider(zxData, inputs, outputs)
  }

  val interpretZXGraph : (Graph, List[VName], List[VName]) => Tensor = interpretSpiderGraph(zxSpiderInterpreter)

  /**
    * Given a graph of just boundaries and edges,
    * return the tensor treating those edges as caps
    *
    * @param graph : Graph
    * @return
    */
  def stringGraph(graph: Graph, cap: Tensor, inputList: List[VName], outputList: List[VName]): Tensor = {
    if (graph.verts.size % 2 != 0) {
      throw new Error("String graph should have an even number of boundaries")
    }

    // GRAPHS ARE READ BOTTOM TO TOP HERE.


    // Errors here are from node names appearing in the input or output list but not in the graph
    val numInternalInputs = inputList.count(vn => {
      inputList.contains(graph.adjacentVerts(vn).head)
    })
    val numInternalOutputs = outputList.count(vn => {
      outputList.contains(graph.adjacentVerts(vn).head)
    })

    val joinLowerList = inputList.filterNot(vn => inputList.contains(graph.adjacentVerts(vn).head))
    val joinUpperList = outputList.filterNot(vn => outputList.contains(graph.adjacentVerts(vn).head))

    var claimedInternalCaps: List[VName] = List()
    var claimedInternalCups: List[VName] = List()

    val numJoin = (inputList.size + outputList.size - numInternalInputs - numInternalOutputs) / 2

    def toLowerNeighbour(place: Int): Int = {
      val name: VName = inputList(place)
      val neighbour: VName = graph.adjacentVerts(name).head
      if (inputList.contains(neighbour) && inputList.contains(name)) {
        val index = claimedInternalCaps.indexOf(name)
        if (index < 0) {
          claimedInternalCaps = claimedInternalCaps :+ name
          claimedInternalCaps = claimedInternalCaps :+ neighbour
          toLowerNeighbour(place)
        } else {
          index
        }
      } else {
        numInternalInputs + joinLowerList.indexOf(name)
      }
    }


    def toUpperNeighbour(place: Int): Int = {
      val name: VName = outputList(place)
      val neighbour: VName = graph.adjacentVerts(name).head
      if (outputList.contains(neighbour) && outputList.contains(name)) {
        val index = claimedInternalCups.indexOf(name)
        if (index < 0) {
          claimedInternalCups = claimedInternalCups :+ name
          claimedInternalCups = claimedInternalCups :+ neighbour
          toUpperNeighbour(place)
        } else {
          index
        }
      } else {
        numInternalOutputs + joinUpperList.indexOf(name)
      }
    }

    def toJoin(place: Int) : Int = {
      val name: VName = joinLowerList(place)
      val neighbour: VName = graph.adjacentVerts(name).head
      joinUpperList.indexOf(neighbour)
    }

    val caps = cap.power(numInternalInputs / 2)
    val cups = cap.transpose.power(numInternalOutputs / 2)

    val sigmaLower = if (inputList.nonEmpty) {
      Tensor.swap(inputList.length, toLowerNeighbour)
    } else {
      Tensor(Array(Array(Complex(1, 0))))
    }

    val sigmaUpper = if (outputList.nonEmpty) {
      Tensor.swap(outputList.length, toUpperNeighbour).transpose
    } else {
      Tensor(Array(Array(Complex(1, 0))))
    }

    val sigmaMid = if (joinLowerList.nonEmpty) {
      Tensor.swap(joinLowerList.length, toJoin)
    } else {
      Tensor(Array(Array(Complex(1, 0))))
    }

    sigmaUpper o (cups x Tensor.idWires(numJoin)) o sigmaMid o (caps x Tensor.idWires(numJoin)) o sigmaLower
  }

  private def pullOutVertexGraph(startingGraph: Graph,
                                 inputList: List[VName],
                                 outputList: List[VName],
                                 graphInterpreter: (Graph, List[VName], List[VName]) => Tensor,
                                 spiderInterpreter: (NodeV, Int, Int) => Tensor): Tensor = {
    def ratioDanglingWires(name: VName): Double = {
      // A measure of how many boundary vs non-boundary neighbours the node has
      val numBoundary = startingGraph.adjacentVerts(name).count(vn => inputList.contains(vn))
      (1 + numBoundary).toDouble / (1 + startingGraph.adjacentVerts(name).size - numBoundary)
    }

    // def boundaries(g: Graph): Set[VName] = g.verts.filter(g.isTerminalWire)

    // Pick a vertex to shift from graph to tensor
    val cutVertex = startingGraph.verts
      .filterNot(vn => startingGraph.vdata(vn).isWireVertex)
      .maxBy(ratioDanglingWires)
    val numCutSpiderInOuts = startingGraph.adjacentVerts(cutVertex).size

    // cut out that vertex, as well as any boundaries it was attached to
    val (reducedGraph, freshMadeBoundaries, removedBoundaries) = startingGraph.cutVertex(cutVertex, inputList.toSet)
    val uncutBoundariesVector: Vector[VName] =
      inputList.filter(b => !removedBoundaries.contains(b)).toVector
    val reducedGraphBoundaries = uncutBoundariesVector.toList ++ freshMadeBoundaries.toList

    val spiderTensor = spiderInterpreter(
      startingGraph.vdata(cutVertex).asInstanceOf[NodeV],
      numCutSpiderInOuts - freshMadeBoundaries.size,
      freshMadeBoundaries.size)

    val reducedGraphBoundariesVector: Vector[VName] = reducedGraphBoundaries.toVector

    val bottomSigma: Tensor = {
      val swapList: List[Int] = {
        var leftCount = 0
        var rightCount = (reducedGraphBoundaries.toSet -- freshMadeBoundaries).size
        inputList.indices.map(i =>
          if (reducedGraphBoundaries.contains(inputList(i))) {
            leftCount += 1
            leftCount - 1
          } else {
            rightCount += 1
            rightCount - 1
          }
        ).toList
      }
      Tensor.swap(inputList.size, swapList)
    }

    /*
    val topSigma: Tensor = {
      val inputs: Vector[VName] = uncutBoundariesVector ++ freshMadeBoundaries.toVector
      Tensor.swap(reducedGraphBoundaries.size,
        i => reducedGraphBoundariesVector.indexOf(inputs(i)))
    }
    */

    /**
      * Starting with the graph G, pulling out the spider S
      * You want:
      *
      * [     G'    ]
      * [id]  x [S]
      * [ botSigma ]
      *
      * Where G' is the reduced graph, with vector of inputs: reducedGraphBoundariesVector
      * S is the cut spider,
      * the identity is on uncutBoundariesVector
      * And the bottom sigma acts on allInputsVector
      * (Rather than having topSigma now just have different lis tof inputs for G')
      */

    graphInterpreter(reducedGraph, reducedGraphBoundaries, outputList) o
      // topSigma o
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
      connectingCaps o Tensor.swap(connectionList).transpose o allSpidersTensors
    }
  }

  private def interpretZXAdjMatSpidersFirst(adj: AdjMat, greenAM: Vector[NodeV], redAM: Vector[NodeV]): Tensor = {
    // Interpret the graph as (caps) o (crossings) o (vertices)
    if (adj.size == 0) {
      Tensor.id(1)
    } else {
      // Tensor representation of a spider
      def vertexToSpider(v: Int): Tensor = {
        def pullOutAngle(nv: NodeV): PhaseExpression = if (!nv.value.isEmpty) {
          nv.value
        } else {
          "0"
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

      val cap = interpretZXSpider(ZXAngleData(isGreen = true, "0"), 2, 0)

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
        case VertexColour.Green => interpretZWSpiderNoInputs(!black, numLegs)
        case VertexColour.Red => interpretZWSpiderNoInputs(black, numLegs)
      }
    }

    interpretAdjMat(adj, cup, vertexToSpider)
  }

  case class ZXAngleData(isGreen: Boolean, angle: PhaseExpression)

}
