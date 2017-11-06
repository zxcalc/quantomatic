package quanto.cosy

/**
  * An iso-free graph enumeration procedure, based on the one described in Colbourn and Read (1979)
  *
  * This variation enumerates undirected, typed, bipartite (red/green) graphs with fixed boundary
  */


class GraphEnumException(msg: String) extends Exception(msg)


// store an undirected graph as a symmetric adjacency matrix
case class AdjMat(numRedTypes: Int,
                  numGreenTypes: Int,
                  numBoundaries: Int = 0,
                  red: Vector[Int] = Vector(),
                  green: Vector[Int] = Vector(),
                  arities: Vector[Int] = Vector(),
                  mat: Vector[Vector[Boolean]] = Vector())
extends Ordered[AdjMat]
{
  lazy val size: Int = mat.length
  lazy val numRed = red.sum
  lazy val numGreen = green.sum

  // advance to the next type of vertex added by the addVertex method. The order is boundaries,
  // then each red type, then each green type.
  def nextType: Option[AdjMat] = {
    if (red.length < numRedTypes) Some(copy(red = red :+ 0))
    else if (green.length < numGreenTypes) Some(copy(green = green :+ 0))
    else None
  }

  // add the given vector as the bottom row and rightmost column, with false in bottom-right.
  private def growMatrix(vec: Vector[Boolean]) = {
    mat.indices.toVector.map { r => mat(r) :+ vec(r) } :+ (vec :+ false)
  }

  // This method grows the adjacency matrix by adding a new boundary, red node, or green node, with the given
  // vector of edges.
  //
  // Optionally, force the given matrix to (ultimately) have the given arity. Boundaries are always forced
  // to have arity 1.
  def addVertex(connection: Vector[Boolean], arity: Int = -1) = {
    if (red.isEmpty && green.isEmpty) { // new vertex is a boundary
      copy(numBoundaries = numBoundaries + 1,
           arities = arities :+ 1,
           mat = growMatrix(connection))
    } else if (red.nonEmpty && green.isEmpty) { // new vertex is a red node
      copy(red = red.updated(red.length-1, red(red.length-1)+1),
           arities = arities :+ arity,
           mat = growMatrix(connection))
    } else { // new vertex is a green node
      copy(green = green.updated(green.length-1, green(green.length-1)+1),
           arities = arities :+ arity,
           mat = growMatrix(connection))
    }
  }

  // compare the lower triangular part of this matrix, lexicographically
  def compare(that: AdjMat): Int = {
    for (i <- 0 until size)
      for (j <- 0 to i)
        if (mat(i)(j) < that.mat(i)(j)) return -1
        else if (mat(i)(j) > that.mat(i)(j)) return 1
    0
  }

  // compare this matrix with itself, but with the rows and columns permuted according to "perm"
  def compareWithPerm(perm: Vector[Int]): Int = {
    for (i <- 0 until size)
      for (j <- 0 to i)
        if (mat(i)(j) < mat(perm(i))(perm(j))) return -1
        else if (mat(i)(j) > mat(perm(i))(perm(j))) return 1
    0
  }

  // return all the vertex-permutations which respect type and (optionally) keep boundary fixed
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

  // a matrix is canonical if it is lexicographically smaller than any vertex permutation
  def isCanonical(permuteBoundary: Boolean = false): Boolean = validPerms(permuteBoundary).forall { p => compareWithPerm(p) <= 0 }

  // return a list of all the valid ways to connect a new node to the graph, which respect the
  // bipartite structure, and maintain boundaries with arity at most 1
  //
  // Optionally, give a number of remaining vertices. This will force connections to be made if
  // necessary to fulfill arity constraints
  def validConnections(bipartite: Boolean, remaining: Int = -1, newArity: Int = -1): Vector[Vector[Boolean]] = {
    val bnd = red.isEmpty && green.isEmpty
    val notRed = red.isEmpty || green.nonEmpty
    val notGreen = green.isEmpty

    def validConnectionsFrom(i: Int, a: Int): Vector[Vector[Boolean]] =
      if (newArity != -1 && a > newArity) Vector()
      else if (i >= size && (newArity == -1 || remaining == -1 || newArity <= a + remaining)) Vector(Vector())
      else {
        lazy val currentArity = mat(i).foldRight(0)((b,a1) => if (b) a1+1 else a1)

        (
          // determine if an EDGE is allowed from vertex i to the new node
          if ((arities(i) == -1 || currentArity < arities(i)) &&
              (i < numBoundaries ||
               (i >= numBoundaries && i < numBoundaries + numRed && (notRed || !bipartite)) ||
               (i >= numBoundaries + numRed && (notGreen || !bipartite)))
          ) {
              if (bnd) Vector(true +: Vector.fill(size - i - 1)(false))
              else validConnectionsFrom(i + 1, a + 1).map(true +: _)
          } else Vector()
        ) ++ (
          // determine if a NON-EDGE is allowed from vertex i to the new node
          if (remaining == -1 || arities(i) <= currentArity + remaining) {
            validConnectionsFrom(i + 1, a).map(false +: _)
          } else Vector()
        )
      }

    validConnectionsFrom(0, newArity)
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

  // returns true if all vertices have the correct arities
  def isComplete: Boolean = (0 until size).forall {i =>
    arities(i) == -1 || arities(i) == mat(i).map { b => if (b) 1 else 0 }.sum
  }

  // returns true if this graph can be completed with 'n' more vertices
//  def isCompletable(n: Int): Boolean = (0 until size).forall {i =>
//    arities(i) == -1 || arities(i) <= mat(i).map { b => if (b) 1 else 0 }.sum + n
//  }

  override def toString: String = {
    val pipes = Array.fill(size+1)(0)
    var idx = numBoundaries
    for (r <- red) {
      pipes(idx) += 1
      idx += r
    }
    for (g <- green) {
      pipes(idx) += 1
      idx += g
    }

    val sep = pipes.take(size).foldRight("") { (p, rest) => "+"*p + "---" + rest } + "+"*pipes(size) + "\n"


    "\n" + mat.indices.foldRight("") { (i,str) =>
      sep * pipes(i) +
      mat(i).indices.foldRight("") { (j, rowStr) =>
        "|" * pipes(j) + (if (mat(i)(j)) " 1 " else " 0 ") + rowStr
      } + "|" * pipes(size) + "\n" + str
    } + sep * pipes(size)
  }
}

object AdjMat {
  def perms(vec: Vector[Int]): Vector[Vector[Int]] =
    if (vec.isEmpty) Vector(Vector())
    else vec.indices.toVector.flatMap { i => perms(vec.take(i) ++ vec.drop(i+1)).map { vec(i) +: _ } }

  def productPerms(vecs: Vector[Vector[Int]]): Vector[Vector[Int]] =
    vecs match {
      case vec +: vecs1 =>
        val pVec = perms(vec)
        val pRest = productPerms(vecs1)
        pVec.flatMap { p1 => pRest.map { p2 => p1 ++ p2 } }
      case _ => Vector(Vector())
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
            amat.validConnections(bipartite, verts + bnd).foldRight(Stream[AdjMat]()){ (c, rest) =>
              enum1(bnd - 1, verts, amat.addVertex(c)) #::: rest
            }
          // add current node type in all possible ways
          } else if (verts > 0 && (amat.red.nonEmpty || amat.green.nonEmpty)) {
            (for (a <- 0 until maxVertices; c <- amat.validConnections(bipartite, a)) yield (c,a))
            .foldRight(Stream[AdjMat]()){ case ((c,a), rest) =>
              enum1(0, verts - 1, amat.addVertex(c,a)) #::: rest
            }
          } else Stream()
        )
      } else Stream()

    enum1(maxBoundaries, maxVertices, AdjMat(numRedTypes, numGreenTypes))
  }
}
