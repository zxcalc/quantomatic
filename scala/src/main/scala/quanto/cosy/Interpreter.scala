package quanto.cosy

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

  def interpretSpider(green: Boolean, angle: Double, inputs: Int, outputs: Int): Tensor = {
    // Converts spider to tensor. If green==false then it is a red spider
    val toString = green.toString + ":" + angle + ":" + inputs + ":" + outputs
    if (cached.contains(toString)) cached(toString) else {
      def gen(i: Int, j: Int): Complex =
        if (i == 0 && j == 0) Complex.one //TODO: Scalars
        else if (i == math.pow(2, outputs) - 1 && j == math.pow(2, inputs) - 1) Complex(math.cos(angle), math.sin(angle))
        else Complex.zero

      val mid = Tensor(math.pow(2, outputs).toInt, math.pow(2, inputs).toInt, gen)
      val spider = if (green) mid else makeHadamards(outputs) o mid o makeHadamards(inputs)
      cached += (toString -> spider)
      spider
    }
  }

  def interpretGraph(graph: Graph, greenAM: AngleMap, redAM: AngleMap): Tensor = {
    // Converts a graph to a tensor. The angle maps convert the angleType of a vertex to a Double
    interpretGraphSpidersFirst(graph, greenAM, redAM)
  }

  private def interpretGraphSpidersFirst(graph: Graph, greenAM: AngleMap, redAM: AngleMap): Tensor = {
    // Interpret the graph as (caps) o (crossings) o (vertices)
    val vertices = graph.vertices
    val allSpidersVectors = vertices.values.toList
    var vertexNextEdge = Map[Int, Int]()
    var edgesFilled = 0

    // For every vertex register which inputs/outputs of the tensor it connects to
    def registerLegs(vertexID: Int): Unit = {
      vertexNextEdge = vertexNextEdge + (vertexID -> edgesFilled)
      edgesFilled += vertices(vertexID).connections.size
    }

    // Claims an output leg of a spider, next claim will give next leg
    def claimLeg(vID: Int): Int = {
      val leg = vertexNextEdge(vID)
      vertexNextEdge = vertexNextEdge updated(vID, leg + 1)
      leg
    }

    // Tensor representation of a spider
    def vecToSpider(v: Vertex): Tensor = {
      v.vertexType match {
        case VertexType.Boundary => Tensor.id(2)
        case VertexType.Green => interpretSpider(true, greenAM(v.angleType), 0, v.connections.size)
        case VertexType.Red => interpretSpider(false, redAM(v.angleType), 0, v.connections.size)
      }
    }

    var legCount = 0
    vertices.foreach(x => registerLegs(x._1))
    val allSpidersTensors = allSpidersVectors.map(vecToSpider).foldLeft(Tensor.id(1))((a, b) => a x b)
    val cap = interpretSpider(true, 0, 2, 0)
    var connectingCaps: Tensor = Tensor.id(1)
    var connectionList: List[Int] = List()
    for (vecWithID <- graph.vertices) {
      val sourceID = vecWithID._1
      val vec = vecWithID._2
      for (targetID <- vec.connections) {
        if (targetID >= sourceID) {
          connectingCaps = connectingCaps x cap
          connectionList = connectionList ++ List(claimLeg(sourceID), claimLeg(targetID))
        }
      }
    }
    allSpidersTensors.plugBeneath(connectingCaps, connectionList)
  }
}
