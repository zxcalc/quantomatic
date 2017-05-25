package quanto.cosy

/**
  * Graphs should take an adjacency matrix and produce a mutable graph that accepts local operations
  */


object VertexType {

  sealed trait EnumVal

  case object Boundary extends EnumVal

  case object Green extends EnumVal

  case object Red extends EnumVal

}

case class Vertex(vertexType: VertexType.EnumVal,
                  angleType: Int,
                  connections: Set[Int]) {

}

case class Edge(connect: List[Int])

class Graph(adjMat: AdjMat) {
  val vertices: scala.collection.mutable.Map[Int, Vertex] = collection.mutable.Map()
  var vIndex = 0
  var colCount = 0
  var typeCount = 0

  def vertexList: List[Vertex] = vertices.map { case (key, value) => value }.toList

  def add(v: Vertex): Unit = {
    vertices += (vIndex -> v)
    vIndex += 1
  }

  for (i <- 0 until adjMat.numBoundaries) {
    add(Vertex(VertexType.Boundary, 0, connectionsFromVector(adjMat.mat(colCount))))
    colCount += 1
  }

  def connectionsFromVector(vec: Vector[Boolean]): Set[Int] = {
    (for (j <- vec.indices) yield (j, vec(j))).foldLeft(Set[Int]()) { (a, b) => if (b._2) a + b._1 else a }
  }

  for (j <- adjMat.red) {
    for (i <- 0 until j) {
      add(Vertex(VertexType.Red, typeCount, connectionsFromVector(adjMat.mat(colCount))))
      colCount += 1
    }
    typeCount += 1
  }

  typeCount = 0
  for (j <- adjMat.green) {
    for (i <- 0 until j) {
      add(Vertex(VertexType.Green, typeCount, connectionsFromVector(adjMat.mat(colCount))))
      colCount += 1
    }
    typeCount += 1
  }
}

object Graph {
  def apply(adjMat: AdjMat): Graph = new Graph(adjMat)
}
