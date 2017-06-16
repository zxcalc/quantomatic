package quanto.cosy

/**
  * An iso-free graph enumeration procedure, based on the one described in Colbourn and Read (1979)
  *
  * This variation enumerates undirected, typed, bipartite (red/green) graphs with fixed boundary
  */


object VertexColour {

  // Red, Green or Boundary
  sealed trait EnumVal

  case object Boundary extends EnumVal

  case object Green extends EnumVal

  case object Red extends EnumVal

}

class GraphEnumException(msg: String) extends Exception(msg)


// store an undirected graph as a symmetric adjacency matrix
case class AdjMat(numRedTypes: Int,
                  numGreenTypes: Int,
                  numBoundaries: Int = 0,
                  red: Vector[Int] = Vector(),
                  green: Vector[Int] = Vector(),
                  mat: Vector[Vector[Boolean]] = Vector())
  extends Ordered[AdjMat] {
  lazy val size: Int = mat.length
  lazy val numRed : Int = red.sum
  lazy val numGreen : Int = green.sum
  lazy val hash : String = makeHash()
  lazy val vertexColoursAndTypes: List[(VertexColour.EnumVal, Int)] = {
    var _vertexColoursAndTypes: List[(VertexColour.EnumVal, Int)] = List()
    var colCount = 0
    var angleTypeCount = 0
    for (i <- 0 until numBoundaries) {
      _vertexColoursAndTypes = (VertexColour.Boundary, 0) :: _vertexColoursAndTypes
      colCount += 1
    }

    for (j <- red) {
      for (i <- 0 until j) {
        _vertexColoursAndTypes = (VertexColour.Red, angleTypeCount) :: _vertexColoursAndTypes
        colCount += 1
      }
      angleTypeCount += 1
    }

    angleTypeCount = 0
    for (j <- green) {
      for (i <- 0 until j) {
        _vertexColoursAndTypes = (VertexColour.Green, angleTypeCount) :: _vertexColoursAndTypes
        colCount += 1
      }
      angleTypeCount += 1
    }
    _vertexColoursAndTypes.reverse
  }

  // advance to the next type of vertex added by the addVertex method. The order is boundaries,
  // then each red type, then each green type.
  def nextType: Option[AdjMat] = {
    if (red.length < numRedTypes) Some(copy(red = red :+ 0))
    else if (green.length < numGreenTypes) Some(copy(green = green :+ 0))
    else None
  }

  // This method grows the adjacency matrix by adding a new boundary, red node, or green node, with the given
  // vector of edges.
  def addVertex(connection: Vector[Boolean]) : AdjMat = {
    if (red.isEmpty && green.isEmpty) { // new vertex is a boundary
      copy(numBoundaries = numBoundaries + 1, mat = growMatrix(connection))
    } else if (red.nonEmpty && green.isEmpty) { // new vertex is a red node
      copy(red = red.updated(red.length - 1, red(red.length - 1) + 1), mat = growMatrix(connection))
    } else { // new vertex is a green node
      copy(green = green.updated(green.length - 1, green(green.length - 1) + 1), mat = growMatrix(connection))
    }
  }

  // add the given vector as the bottom row and rightmost column, with false in bottom-right.
  private def growMatrix(vec: Vector[Boolean]) = {
    mat.indices.toVector.map { r => mat(r) :+ vec(r) } :+ (vec :+ false)
  }

  // compare the lower triangular part of this matrix, lexicographically
  def compare(that: AdjMat): Int = {
    for (i <- 0 until size)
      for (j <- 0 to i)
        if (mat(i)(j) < that.mat(i)(j)) return -1
        else if (mat(i)(j) > that.mat(i)(j)) return 1
    0
  }

  // a matrix is canonical if it is lexicographically smaller than any vertex permutation
  def isCanonical(permuteBoundary: Boolean = false): Boolean = validPerms(permuteBoundary).forall { p => compareWithPerm(p) <= 0 }

  // compare this matrix with itself, but with the rows and columns permuted according to "perm"
  def compareWithPerm(perm: Vector[Int]): Int = {
    for (i <- 0 until size)
      for (j <- 0 to i)
        if (mat(i)(j) < mat(perm(i))(perm(j))) return -1
        else if (mat(i)(j) > mat(perm(i))(perm(j))) return 1
    0
  }

  // return all the vertex-permutations which respect type and keep boundary fixed
  def validPerms(permuteBoundary: Boolean): Vector[Vector[Int]] = {
    var idx = numBoundaries
    val bVec = (0 until idx).toVector
    var vecs = Vector[Vector[Int]]()
    for (i <- red) {
      val r = idx until idx + i
      idx += i
      vecs = vecs :+ r.toVector
    }

    for (i <- green) {
      val r = idx until idx + i
      idx += i
      vecs = vecs :+ r.toVector
    }

    if (permuteBoundary) AdjMat.productPerms(bVec +: vecs)
    else AdjMat.productPerms(vecs).map(bVec ++ _)
  }

  // return a list of all the valid ways to connect a new node to the graph, which respect the
  // bipartite structure, and maintain boundaries with arity at most 1
  def validConnections(bipartite: Boolean): Vector[Vector[Boolean]] = {
    val bnd = red.isEmpty && green.isEmpty
    val notRed = red.isEmpty || green.nonEmpty
    val notGreen = green.isEmpty

    def validConnectionsFrom(i: Int): Vector[Vector[Boolean]] =
      if (i >= size) Vector(Vector())
      else {
        val rest = validConnectionsFrom(i + 1)
        if ((i < numBoundaries && !mat(i).contains(true)) ||
          (i >= numBoundaries && i < numBoundaries + numRed && (notRed || !bipartite)) ||
          (i >= numBoundaries + numRed && (notGreen || !bipartite))
        )
          rest.map(false +: _) ++
            (if (bnd) Vector(true +: Vector.fill(size - i - 1)(false))
            else rest.map(true +: _))
        else
          rest.map(false +: _)
      }

    validConnectionsFrom(0)
  }

  def componentOf(v: Int, seen: Set[Int] = Set()): Set[Int] = {
    (0 until size).foldRight(seen + v) { (w, seen1) =>
      if (seen1.contains(w) || !mat(v)(w)) seen1
      else seen1.union(componentOf(w, seen1))
    }
  }

  def isConnected: Boolean = size == 0 || componentOf(0).size == size

  // check if there exists an automorphism which interchanges red and green nodes. Note types and boundary
  // order are ignored.
  // (mainly used for testing)
  def isColorSymmetric: Boolean = if (numRed != numGreen) false else {
    val fix = (numBoundaries until numBoundaries + numRed).toVector

    for (p1 <- AdjMat.perms((0 until numBoundaries).toVector);
         p2 <- AdjMat.perms((numBoundaries + numRed until size).toVector))
      if (compareWithPerm(p1 ++ p2 ++ fix) == 0) return true
    false
  }

  // returns true if all boundaries are connected to something
  def isComplete: Boolean = (0 until numBoundaries).forall(i => mat(i).contains(true))

  override def toString: String = {
    val pipes = Array.fill(size + 1)(0)
    var idx = numBoundaries
    for (r <- red) {
      pipes(idx) += 1
      idx += r
    }
    for (g <- green) {
      pipes(idx) += 1
      idx += g
    }

    val sep = pipes.take(size).foldRight("") { (p, rest) => "+" * p + "---" + rest } + "+" * pipes(size) + "\n"


    "\n" + mat.indices.foldRight("") { (i, str) =>
      sep * pipes(i) +
        mat(i).indices.foldRight("") { (j, rowStr) =>
          "|" * pipes(j) + (if (mat(i)(j)) " 1 " else " 0 ") + rowStr
        } + "|" * pipes(size) + "\n" + str
    } + sep * pipes(size)
  }

  private def makeHash(): String = {
    def toChars(intAsString: String, base: Int): String = {
      java.lang.Long.toString(java.lang.Long.parseLong(intAsString
        , base), 36)
    }

    "" + List(size - numGreen - numRed).mkString("-") +
      "." +
      red.mkString("-") +
      "." +
      green.mkString("-") +
      "." +
      toChars("0" +
        mat.flatten.foldLeft("")((a, b) => if (b) a + "1" else a + "0")
        , 2)
  }
}

object AdjMat {
  def perms(vec: Vector[Int]): Vector[Vector[Int]] =
    if (vec.isEmpty) Vector(Vector())
    else vec.indices.toVector.flatMap { i =>
      perms(vec.take(i) ++ vec.drop(i + 1)).map {
        vec(i) +: _
      }
    }

  def productPerms(vecs: Vector[Vector[Int]]): Vector[Vector[Int]] =
    vecs match {
      case vec +: vecs1 =>
        val pVec = perms(vec)
        val pRest = productPerms(vecs1)
        pVec.flatMap { p1 => pRest.map { p2 => p1 ++ p2 } }
      case _ => Vector(Vector())
    }

  def fromHash(hash: String): AdjMat = {
    // "boundaries.red1-red2.green1-green2.matBase36"
    val dotChunk = hash.split("\\.")
    val numBoundaries = dotChunk(0).toInt
    val red: Vector[Int] = dotChunk(1).split("-").map(a => a.toInt).toVector
    val green: Vector[Int] = dotChunk(2).split("-").map(a => a.toInt).toVector
    val size = numBoundaries + red.sum + green.sum
    val longMatStringUnpadded = java.lang.Long.toString(java.lang.Long.parseLong(dotChunk(3), 36), 2)
    val longMatString = (1 to size * size - longMatStringUnpadded.length).foldLeft("") {(a,b) => "0" + a} +
      longMatStringUnpadded
    val longMatVec = longMatString.map(x => x == '1').toVector
    val mat = longMatVec.grouped(size).toVector
    new AdjMat(red.length, green.length, numBoundaries, red, green, mat)
  }
}

object ColbournReadEnum {
  def enumerate(numRedTypes: Int, numGreenTypes: Int, maxBoundaries: Int, maxVertices: Int,
                bipartite: Boolean = true, permuteBoundary: Boolean = false): Stream[AdjMat] = {
    if (numRedTypes == 0 && numGreenTypes == 0) throw new GraphEnumException("must have at least one node type")

    def enum1(bnd: Int, verts: Int, amat: AdjMat): Stream[AdjMat] =
      if (amat.isCanonical(permuteBoundary)) {
        (
          // advancing to the next type of node
          amat.nextType match {
            case Some(amat1) =>
              enum1(0, verts, amat1)
            case None =>
              if (amat.isComplete) Stream(amat)
              else Stream()
          }
          ) #::: (
          // add boundaries in all possible ways
          if (bnd > 0) {
            amat.validConnections(bipartite).foldRight(Stream[AdjMat]()) { (c, rest) =>
              enum1(bnd - 1, verts, amat.addVertex(c)) #::: rest
            }
            // add current node type in all possible ways
          } else if (verts > 0 && (amat.red.nonEmpty || amat.green.nonEmpty)) {
            amat.validConnections(bipartite).foldRight(Stream[AdjMat]()) { (c, rest) =>
              enum1(0, verts - 1, amat.addVertex(c)) #::: rest
            }
          } else Stream()
          )
      } else Stream()

    enum1(maxBoundaries, maxVertices, AdjMat(numRedTypes, numGreenTypes))
  }
}
