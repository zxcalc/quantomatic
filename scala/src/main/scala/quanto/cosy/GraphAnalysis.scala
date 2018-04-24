package quanto.cosy

import quanto.data.Theory.ValueType
import quanto.data._
import quanto.util.Rational

object GraphAnalysis {
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
}