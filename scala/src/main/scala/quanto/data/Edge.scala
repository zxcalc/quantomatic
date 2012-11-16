package quanto.data

case class DuplicateEdgeNameException(name: String) extends
Exception("Duplicate edge name: '" + name +"'")

class Edge[D](val name: String, var data: D)
object Edge {
  def apply[D](name: String, data: D = ()) =
    new Edge(name, data)
}
