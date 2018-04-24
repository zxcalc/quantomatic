package quanto.cosy


/**
  * Analyse the properties of equivalence classes
  * e.g. Find AdjMats with different numbers of connected components inside the same class
  */

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


