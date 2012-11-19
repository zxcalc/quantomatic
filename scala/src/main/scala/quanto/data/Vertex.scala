package quanto.data

case class DuplicateVertexNameException(name: String)
extends DuplicateNameException("vertex", name)

class Vertex[D](val name: String) extends HasName {
  var data : D = _
}

object Vertex {
  def apply[D](
      name: String): Vertex[D] =
    new Vertex(name)
  def apply[D](
      name: String,
      data : D): Vertex[D] = {
    val v = new Vertex[D](name)
    v.data = data
    v
  }
}