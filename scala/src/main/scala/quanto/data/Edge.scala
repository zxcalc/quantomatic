package quanto.data

case class DuplicateEdgeNameException(name: String)
extends DuplicateNameException("edge", name)

class Edge[D](val name: String) extends HasName {
  var data: D = _
}
object Edge {
  def apply[D](name: String, data: D) = {
    val e = new Edge[D](name)
    e.data = data
    e
  }

  def apply[D](name: String): Edge[D] = new Edge(name)
}
