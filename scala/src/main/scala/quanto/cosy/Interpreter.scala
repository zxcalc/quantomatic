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
      def gen(i: Int, j: Int): Complex = {
        Complex.zero +
          (if (i == 0 && j == 0) Complex.one else Complex.zero) +
          (if (i == math.pow(2, outputs) - 1 && j == math.pow(2, inputs) - 1)
            Complex(math.cos(angle), math.sin(angle)) else Complex.zero)
      }

      val mid = Tensor(math.pow(2, outputs).toInt, math.pow(2, inputs).toInt, gen)
      val spider = if (green) mid else makeHadamards(outputs) o mid o makeHadamards(inputs)
      cached += (toString -> spider)
      spider
    }
  }

  def interpretAdjMat(adjMat: AdjMat, greenAM: AngleMap, redAM: AngleMap): Tensor = {
    interpretAdjMatSpidersFirst(adjMat: AdjMat, greenAM: AngleMap, redAM: AngleMap)
  }

  private def interpretAdjMatSpidersFirst(adj: AdjMat, greenAM: AngleMap, redAM: AngleMap): Tensor = {
    // Interpret the graph as (caps) o (crossings) o (vertices)
    if (adj.size == 0) {
      Tensor.id(1)
    } else {
      val vertices = adj.mat(0)
      val numVertices = vertices.length
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

      // Tensor representation of a spider
      def vecToSpider(v: Int): Tensor = {
        val (colour, angle) = adj.vertexColoursAndTypes(v)
        val green = true
        colour match {
          case VertexColour.Boundary => Tensor.id(2)
          case VertexColour.Green => interpretSpider(green, greenAM(angle), 0, numConnections(v))
          case VertexColour.Red => interpretSpider(!green, redAM(angle), 0, numConnections(v))
        }
      }

      var legCount = 0
      for (v <- 0 until numVertices) registerLegs(v)
      val allSpidersTensors = (for (v <- 0 until numVertices) yield vecToSpider(v)).
        foldLeft(Tensor.id(1))((a, b) => a x b)
      val cap = interpretSpider(green = true, 0, 2, 0)
      var connectingCaps: Tensor = Tensor.id(1)
      var connectionList: List[Int] = List()
      for (i <- 0 until numVertices; j <- i until numVertices) {
        if (adj.mat(i)(j)) {
          connectingCaps = connectingCaps x cap
          connectionList = connectionList ++ List(claimLeg(i), claimLeg(j))
        }
      }


      allSpidersTensors.plugBeneath(connectingCaps, connectionList)
    }
  }
}
