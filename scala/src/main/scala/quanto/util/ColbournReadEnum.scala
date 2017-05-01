package quanto.util

/**
  * An iso-free graph enumeration procedure, based on the one described in Colbourn and Read (1979)
  *
  * This variation enumerates undirected, typed, bipartite (red/green) graphs with fixed boundary
  */


// store an undirected graph as a symmetric adjacency matrix
class AdjMat(val numBoundaries: Int = 0,
             val red: Vector[Int] = Vector(),
             val green: Vector[Int] = Vector(),
             val mat: Vector[Vector[Boolean]] = Vector())
extends Ordered[AdjMat]
{
  def size: Int = mat.length

  // add the given vector as the bottom row and rightmost column, with false in bottom-right.
  private def growMatrix(vec: Vector[Boolean]) = {
    mat.indices.toVector.map { r => mat(r) :+ vec(r) } :+ (vec :+ false)
  }

  // These functions grow the adjacency matrix by adding a new boundary, red node, or green node, with the given
  // vector of edges.
  def addVertex(edges: Vector[Boolean]) = {
    if (red.isEmpty && green.isEmpty) { // new vertex is a boundary
      new AdjMat(numBoundaries + 1, red, green, growMatrix(edges))
    } else if (red.nonEmpty && green.isEmpty) { // new vertex is a red node
      new AdjMat(numBoundaries, red.updated(red.length-1, red(red.length-1)+1), green, growMatrix(edges))
    } else { // new vertex is a green node
      new AdjMat(numBoundaries, red, green.updated(green.length-1, green(green.length-1)+1), growMatrix(edges))
    }
  }

  // compare the upper triangular part of this matrix, lexicographically
  def compare(that: AdjMat): Int = {
    for (i <- 0 to size)
      for (j <- i to size)
        if (mat(i)(j) < that.mat(i)(j)) return -1
        else if (mat(i)(j) > that.mat(i)(j)) return 1
    0
  }

  // compare this matrix with itself, but with the rows and columns permuted according to "perm"
  def compareWithPerm(perm: Vector[Int]): Int = {
    for (i <- 0 to size)
      for (j <- i to size)
        if (mat(i)(j) < mat(perm(i))(perm(j))) return -1
        else if (mat(i)(j) > mat(perm(i))(perm(j))) return 1
    0
  }

  // return all the vertex-permutations which respect type and keep boundary fixed
  def validPerms: Vector[Vector[Int]] = {
    var idx = numBoundaries
    val bVec = (0 until idx).toVector
    var vecs = Vector[Vector[Int]]()
    for (i <- red) {
      val r = idx until idx + i
      idx += i
      vecs = vecs :+ r.toVector
    }

    AdjMat.productPerms(vecs).map { p => bVec ++ p }
  }

  // a matrix is canonical if it is lexicographically smaller than any vertex permutation
  def isCanonical: Boolean = validPerms.forall { p => compareWithPerm(p) <= 0 }
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

class ColbournReadEnum {

}
