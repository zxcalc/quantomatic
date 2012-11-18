package quanto.data

case class DuplicateBBoxNameException(name: String)
extends DuplicateNameException("bang box", name)

class BBox[B](val name: String, var data: B = ())
extends HasName

object BBox {
  def apply[B](name: String, data: B = ()) = new BBox(name)
}