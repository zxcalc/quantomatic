package quanto.data

case class DuplicateBBoxNameException(name: BBName)
extends DuplicateNameException("bang box", name)

case class BBox[D](name: BBName, data: D) extends NameAndData[BBName, D]
