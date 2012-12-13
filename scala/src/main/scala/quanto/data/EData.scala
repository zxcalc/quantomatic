package quanto.data

abstract class EData(val directed: Boolean)

case class DirEdge() extends EData(true)
case class UndirEdge() extends EData(false)