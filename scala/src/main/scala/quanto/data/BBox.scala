package quanto.data

case class DuplicateBBoxNameException(name: String)
extends DuplicateNameException("bang box", name)

class BBox[D](val name: String)
extends HasName {
  var data: D = _
}

object BBox {
  def apply[D](name: String): BBox[D] = new BBox(name)
  def apply[D](name: String, data: D): BBox[D] = {
    val b = new BBox[D](name)
    b.data = data
    b
  }
}