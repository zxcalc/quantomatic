package quanto.data

abstract class VData

case class NodeV(coord: (Float,Float)=(0,0)) extends VData {
  def withCoord(c: (Float,Float)) = copy(coord=c)
}
case class WireV(coord: (Float,Float)=(0,0)) extends VData {
  def withCoord(c: (Float,Float)) = copy(coord=c)
}
