package quanto.data

abstract class VData {
  def coord: (Double, Double)
}

case class NodeV(coord: (Double,Double)=(0,0)) extends VData {
  def withCoord(c: (Double,Double)) = copy(coord=c)
}
case class WireV(coord: (Double,Double)=(0,0)) extends VData {
  def withCoord(c: (Double,Double)) = copy(coord=c)
}
