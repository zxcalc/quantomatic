package quanto.cosy

import quanto.data.Theory.{ValueType, VertexDesc}
import quanto.data._
import quanto.rewrite.Matcher
import quanto.util.Rational
import quanto.util.json.{Json, JsonObject}

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object GraphAnalysis {

  def zxCircuitCompare(left: Graph, right: Graph): Int = {

    // returns x where
    // x < 0 iff this < that
    // x == 0 iff this == that
    // x > 0 iff this > that

    // Circuit comparison of graphs
    // Cares about T-count

    val Angle = ValueType.AngleExpr

    def phase(vdata: VData): PhaseExpression =
      vdata match {
        case NodeV(d, a, t) => vdata.asInstanceOf[NodeV].phaseData.first[PhaseExpression](ValueType.AngleExpr) match {
          case Some(p) => p
          case None => PhaseExpression.zero(ValueType.AngleExpr)
        }
        case _ => PhaseExpression.zero(ValueType.AngleExpr)
      }


    // Number of T-gates
    def countT(graph: Graph): Int = graph.vdata.count(nd => {
      val const = phase(nd._2).constant
      // T-gate if an odd multiple of \pi/4
      ((const.n * (const.d / 4)) % 2) == 1
    })

    val tDiff = countT(left) - countT(right)
    if (tDiff != 0) return tDiff

    // Number of nodes
    def nodes(graph: Graph): Int = graph.vdata.size

    val nodeDiff = nodes(left) - nodes(right)
    if (nodeDiff != 0) return nodeDiff

    // Number of edges
    def edges(graph: Graph): Int = graph.edata.size

    val edgeDiff = edges(left) - edges(right)
    if (edgeDiff != 0) return edgeDiff

    // Number of "Z" nodes
    // We favour these!
    // Purely for aesthetic reasons
    def countZ(graph: Graph): Int = graph.vdata.count(nd => nd._2.typ == "Z")

    val zDiff = countZ(left) - countZ(right)
    if (zDiff != 0) return zDiff

    // sum of the phases
    def phaseSum(graph: Graph): Rational = graph.vdata.map(nd => phase(nd._2)).
      foldLeft(Rational(0, 1)) { (s, a) => s + a.constant }

    val phaseDiff = phaseSum(left) - phaseSum(right)
    if (phaseDiff > 0) return 1
    if (phaseDiff < 0) return -1

    // Weighting by row
    // example node name: r-2-bl-1-h-1
    def positionWeighting(graph: Graph): Double = {
      def nodeWeighting(node: NodeV): Double = {
        (node.typ match {
          case "X" =>
            2 * (1 + node.phaseData.values.head.constant)
          case "Z" =>
            1 + node.phaseData.values.head.constant
          case "hadamard" =>
            1
        }) / 3 // Scale so any given node has weight at most 1, bust still > 0
      }

      graph.vdata.toList.map(nameData => {
        nameData._2 match {
          case v: NodeV =>
            // Non-wire nodes are weighted biased towards the bottom left
            val placement = CircuitPlacementParser.p(nameData._1.toString)
            placement._1 + placement._2 + nodeWeighting(v)
          case _ =>
            // wires have no weight
            0
        }
      }).sum
    }

    val circuitWeightLeft = positionWeighting(left)
    val circuitWeightRight = positionWeighting(right)
    if (circuitWeightLeft > circuitWeightRight) {
      return 1
    }
    if (circuitWeightLeft < circuitWeightRight) {
      return -1
    }

    0
  }

  case class CircuitPlacementParseException(input: String) extends Error

  object CircuitPlacementParser extends RegexParsers {

    override def skipWhitespace = true

    def INT: Parser[Int] =
      """[0-9]+""".r ^^ {
        _.toInt
      }

    def IDENT: Parser[String] =
      """[\\a-zA-Z_][a-zA-Z0-9_]*""".r ^^ {
        _.toString
      }

    // example node name: r-2-bl-1-h-1
    def expr: Parser[(Int, Int, String)] =
      "r-" ~ INT ~ "-bl-" ~ INT ~ "-" ~ IDENT ~ "-" ~ INT ^^ { case _ ~ r ~ _ ~ bl ~ _ ~ s ~ _ ~ _ => (r, bl, s) }

    def p(s: String): (Int, Int, String) = parseAll(expr, s) match {
      case Success(e, _) => e
      case Failure(msg, _) => throw CircuitPlacementParseException(msg)
      case Error(msg, _) => throw CircuitPlacementParseException(msg)
    }
  }


  def zxGraphCompare(left: Graph, right: Graph): Int = {

    // returns x where
    // x < 0 iff this < that
    // x == 0 iff this == that
    // x > 0 iff this > that


    // Graph comparison for ZX diagrams
    // Cares about node count, then phases

    implicit def stringToPhase(s: String): PhaseExpression = {
      PhaseExpression.parse(s, ValueType.AngleExpr)
    }


    // First count number of nodes
    def nodes(graph: Graph): Int = graph.vdata.size

    val node = nodes(left) - nodes(right)
    if (node != 0) return node

    // Number of edges
    def edges(graph: Graph): Int = graph.edata.size

    val edge = edges(left) - edges(right)
    if (edge != 0) return edge

    // Number of "Z" nodes
    def countZ(graph: Graph): Int = graph.vdata.count(nd => nd._2.typ == "Z")

    val zDiff = countZ(left) - countZ(right)
    if (zDiff != 0) return zDiff

    // Sum of Z angles
    val Pi = math.Pi

    def sumAngles(graph: Graph, filterType: String): Rational = graph.vdata.
      filter(nd => nd._2.typ == filterType).
      foldLeft(Rational(0, 1)) {
        (angle, nd) => angle + stringToPhase(nd._2.asInstanceOf[NodeV].value).constant
      }

    // sumAngles returns a rational that is probably bigger than 2 (remember that the pi is left out)

    val ZAngles: Rational = sumAngles(left, "Z") - sumAngles(right, "Z")
    if (ZAngles > 0) return 1
    if (ZAngles < 0) return -1

    // Sum of X angles

    val XAngles: Rational = sumAngles(left, "X") - sumAngles(right, "X")
    if (XAngles > 0) return 1
    if (XAngles < 0) return -1


    0
  }

  def connectionClasses(adjMat: AdjMat): Vector[Int] = {
    val initialVector = (0 until adjMat.size).toVector

    def join(v: Vector[Int], i: Int, j: Int): Vector[Int] = {
      v.map { a => if (a == v(i) || a == v(j)) {
        math.min(v(i), v(j))
      } else a
      }
    }

    adjMat.mat.zipWithIndex.flatMap(rowWithIndex => {

      val rci = rowWithIndex._1.zipWithIndex
      rci.map(bi => (bi._1, bi._2, rowWithIndex._2))
    }).filter(_._1).map(bii => (bii._2, bii._3)).foldLeft(initialVector) {
      (v, ii) => join(v, ii._1, ii._2)
    }
  }

  def containsScalars(adjMat: AdjMat): Boolean = {
    // Boundaries are at the front of the adjmat, and are given labels first
    // So if any class still has labels higher than the number of boundaries
    // it can't be connected to any of the boundaries
    val cc = connectionClasses(adjMat)
    !cc.forall(_ < adjMat.numBoundaries)
  }


  type BMatrix = Vector[Vector[Boolean]]

  def tensorToBooleanMatrix(tensor: Tensor): BMatrix = {
    def bool(a: Array[Complex]): Vector[Boolean] = a.foldLeft(Vector[Boolean]())((vs, b) => vs :+ (b != Complex(0, 0)))

    def bool2(a: Array[Array[Complex]]): BMatrix = a.foldLeft(Vector[Vector[Boolean]]())((vs, b) => vs :+ bool(b))

    bool2(tensor.contents)
  }

  def distanceOfErrorsFromEnds(ends: List[VName])(graph: Graph): Option[Double] = {
    val vertexList = graph.verts.toList

    def namesToIndex(name: VName) = vertexList.indexOf(name)

    val targets = ends.map(namesToIndex).toSet
    val errorNames = detectPiNodes(graph)


    val errors = errorNames.map(namesToIndex)
    val rawAdjacencyMatrix = adjacencyMatrix(graph)
    val bypassedAdjacencyMatrix = bypassSpecial(detectPiNodes)(graph, rawAdjacencyMatrix)
    pathDistanceSet(bypassedAdjacencyMatrix, errors, targets) match {
      case None => None
      case Some(d) => Some(d - 1) //account for errors being over-counted in standard distance
    }

  }


  def distanceOfSingleErrorFromEnd(ends: Set[VName])(graph: Graph, vNames: Set[VName]): Option[Double] = {
    val adjacencyMatrix = GraphAnalysis.adjacencyMatrix(graph)
    val vertexList = graph.verts.toList

    def namesToIndex(name: VName) = vertexList.indexOf(name)

    val targets = ends.map(namesToIndex)
    // val errorNames = detectPiNodes(graph)


    // val errors = errorNames.map(namesToIndex)
    // val bypassedAdjacencyMatrix = bypassSpecial(detectPiNodes)(graph, adjacencyMatrix)

    implicit def nameToInt(name: VName): Int = {
      adjacencyMatrix._1.indexOf(name)
    }

    GraphAnalysis.pathDistanceSet(adjacencyMatrix, vNames.map(nameToInt), targets) match {
      case None => None
      case Some(d) => Some(d - 1)
    }
  }

  def bypassSpecial(detection: Graph => Set[VName])
                   (graph: Graph, matrixWithNames: (List[VName], BMatrix)): (List[VName], BMatrix) = {
    val vNames = detection(graph)
    vNames.foldLeft(matrixWithNames)((mat, _) => bypassSpecialOnce(vNames, mat))
  }

  def bypassSpecialOnce(special: Set[VName], matrixWithNames: (List[VName], BMatrix)): (List[VName], BMatrix) = {
    def getIndex(name: VName): Int = matrixWithNames._1.indexOf(name)

    def getNeighbours(name: VName): List[Int] = {
      matrixWithNames._2(getIndex(name)).zipWithIndex.filter(p => p._1).map(p => p._2).toList
    }

    val neighbourList = special.map(getNeighbours)

    def isConnected(i: Int, j: Int): Boolean = {
      matrixWithNames._2(i)(j) || neighbourList.exists(l => l.contains(i) && l.contains(j))
    }

    val bypassedMatrix = matrixWithNames._1.indices.foldLeft(Vector[Vector[Boolean]]())(
      (vs, i) => vs :+ matrixWithNames._1.indices.foldLeft(Vector[Boolean]())((vs, j) => vs :+ isConnected(i, j))
    )
    (matrixWithNames._1, bypassedMatrix)
  }

  def detectPiNodes(graph: Graph): Set[VName] = {
    graph.verts.
      filterNot(name => graph.vdata(name).isBoundary || graph.vdata(name).isWireVertex).
      filter(name => graph.vdata(name).asInstanceOf[NodeV].phaseData.
        firstOrError(ValueType.AngleExpr).
        equals(PhaseExpression(Rational(1), ValueType.AngleExpr))) // Pull out those angle expressions with value \pi
  }

  def adjacencyMatrix(graph: Graph): (List[VName], BMatrix) = {
    val vertexNames = graph.verts.toList

    def setToVector(vertexSet: Set[VName]): Vector[Boolean] = {
      vertexNames.foldLeft(Vector[Boolean]())((vs, v) => vs :+ vertexSet.contains(v))
    }

    (vertexNames, vertexNames.foldLeft(Vector[Vector[Boolean]]())((vs, v) => vs :+ setToVector(graph.adjacentVerts(v))))
  }

  def pathDistanceSet(matrixWithNames: (List[VName], BMatrix),
                      distancesToFind: Set[Int],
                      measuredFrom: Set[Int]):
  Option[Double] = {

    val names = matrixWithNames._1
    // val matrix = matrixWithNames._2

    val distances = distancesFromInitial(matrixWithNames, Set(), measuredFrom.map(i => names(i)))

    val importantDistances = distances.filter(nd => distancesToFind.contains(names.indexOf(nd._1)))

    if (importantDistances.nonEmpty) {
      val d = intsWithCount(importantDistances.values.toList)
      if (d < 0) None else Some(d)
    } else {
      None
    }
  }

  def distancesFromInitial(matrixWithNames: (List[VName], BMatrix), ignoring: Set[VName], initials: Set[VName]):
  Map[VName, Int] = {

    // Initials have distance 0, unreached have distance -1

    val names = matrixWithNames._1
    val matrix = matrixWithNames._2

    val vertexIndex: Map[VName, Int] = names.zipWithIndex.map(ni => ni._1 -> ni._2).toMap
    val initialDistances: Map[VName, Int] = names.map(n => if (initials.contains(n)) n -> 0 else n -> -1).toMap


    def requestDistance(name: VName, currentDistances: Vector[Int]): Int = {
      val index = vertexIndex(name)
      // Find the distance if not already found
      val neighbours = matrix(index).zipWithIndex.filter(_._1).map(_._2)
      val evaluatedNeighbours = (Vector(currentDistances(index)) ++ neighbours.map(neighbour => {
        val d = currentDistances(neighbour)
        if (d > -1 && !ignoring.contains(name)) {
          d + 1
        } else d
      }
      )).filter(d => d > -1)
      if (evaluatedNeighbours.nonEmpty) evaluatedNeighbours.min else -1
    }

    def updateDistances(current: Vector[Int]): Vector[Int] = {
      names.map(v => requestDistance(v, current)).toVector
    }

    val finalDistances = names.indices.foldLeft(names.map(initialDistances).toVector)((vs, _) => updateDistances(vs))
    names.zipWithIndex.map(name_index => name_index._1 -> finalDistances(name_index._2)).toMap
  }

  private def intsWithCount(ints: List[Int]): Double = {
    val max = ints.max
    val count = max match {
      case 0 => 1
      case _ => ints.count(c => c == max)
    }

    max + ((count - 1).toDouble / count.toDouble)
  }

  def distanceSpecialFromEnds(specials: List[VName])(ends: List[VName])(graph: Graph): Option[Double] = {

    val vertexList = graph.verts.toList

    def namesToIndex(name: VName) = vertexList.indexOf(name)

    val targets = ends.map(namesToIndex).toSet
    // Called errors for historical reasons
    val errors = specials.map(namesToIndex).toSet
    val aMatrix = adjacencyMatrix(graph)
    pathDistanceSet(aMatrix, errors, targets)
  }

  def pathConnectionMatrices(graph: Graph): List[(Int, Tensor)] = {
    val matrixWithNames = adjacencyMatrix(graph)
    pathConnectionMatrices(matrixWithNames)
  }

  def pathConnectionMatrices(matrixWithNames: (List[VName], BMatrix)): List[(Int, Tensor)] = {
    val adjTensor = booleanMatrixToTensor(matrixWithNames._2)

    var rollingPower = Tensor.id(adjTensor.width)
    (for (i <- matrixWithNames._1.indices) yield {
      (i, {
        rollingPower = rollingPower.compose(adjTensor)
        rollingPower
      })
    }).toList
  }

  def booleanMatrixToTensor(bMatrix: BMatrix): Tensor = {
    def complexify(v: Vector[Boolean]): Array[Complex] = v.map(b => if (b) Complex(1, 0) else Complex(0, 0)).toArray

    def complexify2(v: Vector[Vector[Boolean]]): Array[Array[Complex]] = v.map(b => complexify(b)).toArray

    Tensor(complexify2(bMatrix))
  }

  def neighbours(matrixWithNames: (List[VName], BMatrix), target: VName): Set[VName] = {
    val names = matrixWithNames._1
    val matrix = matrixWithNames._2
    val index = names.indexOf(target)
    matrix(index).zipWithIndex.filter(_._1).map(_._2).map(names(_)).toSet
  }


  def checkIsomorphic(theory: Theory = Theory.DefaultTheory, boundaryByRegex: Option[Regex] = None)
                     (g1: Graph, g2: Graph): Boolean = {
    // Check whether graphs are isomorphic, after constraining their boundaries

    if(g1.verts.size != g2.verts.size) return false

    // Currently the .isBoundary on wires is set by JSON, not programmatically, and as such I don't trust it.
    def borderNodes(g: Graph): Set[VName] = boundaryByRegex match {
      case Some(r) => g.verts.filter(vn => vn.s.matches(r.regex))
      case None => g.verts.filter(vn => g.vdata(vn).isBoundary)
    }

    val overlappingNodes = borderNodes(g1).intersect(borderNodes(g2))

    //First check they have the same number of boundaries, and the same names between them.

    if(overlappingNodes != borderNodes(g1) || overlappingNodes != borderNodes(g2)){
      return false
    }

    val dummyVertexDesc = VertexDesc.fromJson(
      Json.parse("""{
                   |            "value": {
                   |                "type": "empty",
                   |                "latex_constants": true,
                   |                "validate_with_core": false
                   |            },
                   |            "style": {
                   |                "label": {
                   |                    "position": "center",
                   |                    "fg_color": [
                   |                        0.0,
                   |                        0.0,
                   |                        0.0
                   |                    ]
                   |                },
                   |                "stroke_color": [
                   |                    0.0,
                   |                    0.0,
                   |                    0.0
                   |                ],
                   |                "fill_color": [
                   |                    0.0,
                   |                    1.0,
                   |                    1.0
                   |                ],
                   |                "shape": "rectangle"
                   |            },
                   |            "default_data": {
                   |                "type": "dummyBoundary",
                   |                "value": ""
                   |            }
                   |        } """.stripMargin))

    def enforceUnique(s: String) : String = if(theory.vertexTypes.keys.exists(_ == s)){
      enforceUnique(s + "1")
    } else {
      s
    }
    val dummyVertexName: String = enforceUnique("dummyBoundary")
    val dummyTheory = theory.mixin(newVertexTypes = Map(dummyVertexName -> dummyVertexDesc))
    val boundaryData = NodeV(JsonObject(
      "type" -> dummyVertexName,
      "value" -> ""
    ),
      JsonObject(),
      dummyTheory)

    def makeSolidBoundaries(graph: Graph): Graph = {
      overlappingNodes.foldLeft(graph.copy(data = graph.data.copy(theory = dummyTheory))) { (g, vn) =>
          g.updateVData(vn) { _ => boundaryData }
        }
    }

    val solid1 = makeSolidBoundaries(g1)
    val solid2 = makeSolidBoundaries(g2)

    // The graphs are isomorphic if there are matches in both directions that are the identity on boundaries

    val matches12 = Matcher.findMatches(solid1, solid2).filter(
      m => overlappingNodes.forall(
        vn => m.map.v.dom.toList.contains(vn) && m.map.v.domf(vn).contains(vn)
      )
    )

    val matches21 = Matcher.findMatches(solid2, solid1).filter(
      m => overlappingNodes.forall(
        vn => m.map.v.dom.toList.contains(vn) && m.map.v.domf(vn).contains(vn)
      )
    )

    matches21.nonEmpty && matches12.nonEmpty
  }

}