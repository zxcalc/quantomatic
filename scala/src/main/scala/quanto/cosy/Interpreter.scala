package quanto.cosy

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

  def interpretZXSpider(green: Boolean, angle: Double, inputs: Int, outputs: Int): Tensor = {
    // Converts spider to tensor. If green==false then it is a red spider
    val toString = "ZX:" + green.toString + ":" + angle + ":" + inputs + ":" + outputs
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

  def interpretZWSpider(black: Boolean, outputs: Int): Tensor = {
    require(outputs >= 0)
    val toString = "ZW:" + black.toString + ":" + outputs
    val spider = if (cached.contains(toString)) cached(toString) else {
      black match {
        case true =>
          // Black spider
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

        case false =>
          // White spider
          outputs match {
            case 0 => Tensor(Array(Array(0)))
            case _ =>
              Tensor(1,
                Math.pow(2, outputs).toInt,
                (i, j) => (if (j == 0) Complex.one else Complex.zero) -
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
            case e: Error => nv.angle.evaluate(Map()) * math.Pi
          }
        } else {
          // AK: note, I replaced "pi -> math.Pi" with (). "PI" should never appear as a variable
          // name in an AngleExpression
          nv.angle.evaluate(Map()) * math.Pi
        }

        val (colour, nodeType) = adj.vertexColoursAndTypes(v)
        val numLegs = adj.mat(v).count(p => p)
        val green = true
        colour match {
          case VertexColour.Boundary => Tensor.id(2)
          case VertexColour.Green => interpretZXSpider(green, pullOutAngle(greenAM(nodeType)), 0, numLegs)
          case VertexColour.Red => interpretZXSpider(!green, pullOutAngle(redAM(nodeType)), 0, numLegs)
        }
      }

      val cap = interpretZXSpider(green = true, 0, 2, 0)

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
