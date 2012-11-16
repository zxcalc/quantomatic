package quanto.data

case class DuplicateVertexNameException(name: String) extends
Exception("Duplicate vertex name: '" + name +"'")

class Vertex[D](
  val name: String,
  var coord : Tuple2[Float,Float],
  var data: D
) {
  def x = coord._1
  def x_=(x1 : Float) { coord = (x1, coord._2) }
  def y = coord._2
  def y_=(y1 : Float) { coord = (coord._1, y1) }
}

object Vertex {
  def apply[D](
      name: String,
      coord: Tuple2[Float,Float] = (0.0f, 0.0f),
      data: D = ()) =
    new Vertex(name, coord, data)
}