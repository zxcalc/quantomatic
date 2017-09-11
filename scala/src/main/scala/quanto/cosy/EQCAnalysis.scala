package quanto.cosy

import quanto.data._

/**
  * Analyse the properties of equivalence classes
  * e.g. Find AdjMats with different numbers of connected components inside the same class
  */
object GraphAnalysis {
  type BMatrix = Vector[Vector[Boolean]]

  def tensorToBooleanMatrix(tensor: Tensor): BMatrix = {
    def bool(a: Array[Complex]): Vector[Boolean] = a.foldLeft(Vector[Boolean]())((vs, b) => vs :+ (b != Complex(0, 0)))

    def bool2(a: Array[Array[Complex]]): BMatrix = a.foldLeft(Vector[Vector[Boolean]]())((vs, b) => vs :+ bool(b))

    bool2(tensor.contents)
  }

  def distanceOfErrorsFromEnds(ends: List[VName])(graph: Graph): Option[Int] = {
    val vertexList = graph.verts.toList

    def namesToIndex(name: VName) = vertexList.indexOf(name)

    val targets = ends.map(namesToIndex).toSet
    val errorNames = graph.verts.
      filter(name => !graph.vdata(name).isBoundary && !graph.vdata(name).isWireVertex).
      filter(name => graph.vdata(name).asInstanceOf[NodeV].angle != AngleExpression(0))
    val errors = errorNames.map(namesToIndex)
    val distanceMatrices = pathConnectionMatrices(graph)
    pathDistanceSet(distanceMatrices, errors, targets)
  }

  def pathConnectionMatrices(graph: Graph): List[(Int, Tensor)] = {
    val adjMat = adjacencyMatrix(graph)
    val adjTensor = booleanMatrixToTensor(adjMat._2)

    var rollingPower = Tensor.id(adjTensor.width)
    (for (i <- adjMat._1.indices) yield {
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

  def adjacencyMatrix(graph: Graph): (List[VName], BMatrix) = {
    val vertexNames = graph.verts.toList

    def setToVector(vertexSet: Set[VName]): Vector[Boolean] = {
      vertexNames.foldLeft(Vector[Boolean]())((vs, v) => vs :+ vertexSet.contains(v))
    }

    (vertexNames, vertexNames.foldLeft(Vector[Vector[Boolean]]())((vs, v) => vs :+ setToVector(graph.adjacentVerts(v))))
  }

  def pathDistanceSet(matrices: List[(Int, Tensor)], vertexSet1: Set[Int], vertexSet2: Set[Int]): Option[Int] = {

    val distances = (for (a <- vertexSet1) yield {
      val shortest = (for (b <- vertexSet2) yield {
        pathDistance(matrices, a, b)
      }).filter(p => p.nonEmpty).map(p => p.get)
      if (shortest.nonEmpty) Some(shortest.min) else None
    }
      ).filter(p => p.nonEmpty).map(p => p.get)

    if (distances.isEmpty) None else Some(distances.max)
  }

  def pathDistance(matrices: List[(Int, Tensor)], v1Index: Int, v2Index: Int): Option[Int] = {
    if (v1Index == v2Index) {
      Some(0)
    } else {
      val first = matrices.find(it => it._2(v1Index, v2Index) != Complex(0, 0))
      if (first.nonEmpty) Some(first.get._1) else None
    }
  }
}

object EQCAnalysis {

  def AdjMatConnectedComponents(eqc: EquivalenceClassByAdjMat): Map[Int, AdjMat] = {
    var connectivityExamples: Map[Int, AdjMat] = Map()
    eqc.members.foreach(adjMatHash => {
      val adjMat = AdjMat.fromHash(adjMatHash)
      val numColours = AdjMatConnectedComponents(adjMat)
      if (!connectivityExamples.isDefinedAt(numColours)) {
        connectivityExamples += (numColours -> adjMat)
      } else {
        val smaller = if (adjMat.size < connectivityExamples(numColours).size) adjMat else connectivityExamples(numColours)
        connectivityExamples += (numColours -> smaller)
      }
    })
    connectivityExamples
  }

  def AdjMatConnectedComponents(adjMat: AdjMat): Int = {
    if (adjMat.size > 0) {
      def sqMat(mat: Vector[Vector[Boolean]]): Vector[Vector[Boolean]] = {
        var entries: List[Boolean] = List()
        for (i <- mat.indices; j <- mat.indices) {
          def lkup(i: Int, j: Int): Boolean = mat(i)(j) || i == j

          entries = mat.indices.foldLeft(false)((a, k) => a || (lkup(i, k) && lkup(k, j))) :: entries
        }
        entries.reverse.toVector.grouped(mat.length).toVector
      }

      var m = adjMat.mat
      while (m != sqMat(m)) {
        m = sqMat(m)
      }
      // Initialise with every vertex its own colour
      var colours = m.indices.foldLeft(Map[Int, Int]())((a, i) => a + (i -> i))
      for (i <- m.indices; j <- 0 until i) {
        if (m(i)(j)) {
          val sharedColour = math.min(colours(j), colours(i))
          colours += (i -> sharedColour)
          colours += (j -> sharedColour)
        }
      }
      colours.values.toSet.size
    } else {
      // adjmat of size zero
      0
    }
  }
}


