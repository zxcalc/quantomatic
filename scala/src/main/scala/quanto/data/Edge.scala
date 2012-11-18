package quanto.data

case class DuplicateEdgeNameException(name: String)
extends DuplicateNameException("edge", name)

class Edge[D](val name: String, var data: D) extends HasName
object Edge {
  def apply[D](name: String, data: D = ()) =
    new Edge(name, data)
}
