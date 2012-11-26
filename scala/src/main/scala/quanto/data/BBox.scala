package quanto.data

case class BBox[D](name: BBName, data: D) extends NameAndData[BBName, D]
