package quanto.data

case class DuplicateVertexNameException(name: VName)
extends DuplicateNameException("vertex", name)

case class Vertex[D](name: VName, data: D = ()) extends NameAndData[VName,D]

//object Vertex {
//  def apply[D](
//      name: String): Vertex[D] =
//    new Vertex(name)
//  def apply[D](
//      name: String,
//      data : D): Vertex[D] = {
//    val v = new Vertex[D](name)
//    v.data = data
//    v
//  }
//}