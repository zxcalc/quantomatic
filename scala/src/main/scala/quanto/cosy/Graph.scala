package quanto.cosy

/**
  * Graphs should take an adjacency matrix and produce a mutable graph that accepts local operations
  */


object VertexType {

  // Red, Green or Boundary
  sealed trait EnumVal

  case object Boundary extends EnumVal

  case object Green extends EnumVal

  case object Red extends EnumVal

}

case class Vertex(vertexType: VertexType.EnumVal,
                  angleType: Int,
                  connections: Set[Int]) {
  // Holds colour, angle and edge connections
}

class Graph(adjMat: AdjMat) {
  // Converts an AdjMat into a graph of vertices and edges
  // It steps through one column at a time
  var vertices: Map[Int, Vertex] = Map()
  var vIndex = 0
  var colCount = 0
  var angleTypeCount = 0

  var boundaries: List[Vertex] = List()
  var redVertices: List[Vertex] = List()
  var greenVertices: List[Vertex] = List()

  def vertexList: List[Vertex] = vertices.map { case (key, value) => value }.toList

  def add(v: Vertex): Unit = {
    vertices = vertices + (vIndex -> v)
    v.vertexType match {
      case VertexType.Boundary => boundaries = v :: boundaries
      case VertexType.Green => greenVertices = v :: greenVertices
      case VertexType.Red => redVertices = v :: redVertices
    }
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
      add(Vertex(VertexType.Red, angleTypeCount, connectionsFromVector(adjMat.mat(colCount))))
      colCount += 1
    }
    angleTypeCount += 1
  }

  angleTypeCount = 0
  for (j <- adjMat.green) {
    for (i <- 0 until j) {
      add(Vertex(VertexType.Green, angleTypeCount, connectionsFromVector(adjMat.mat(colCount))))
      colCount += 1
    }
    angleTypeCount += 1
  }

  override def toString: String = adjMat.toString
}

object Graph {
  def apply(adjMat: AdjMat): Graph = new Graph(adjMat)
}
