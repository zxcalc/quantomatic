package quanto.data

import quanto.util.json._

abstract class VData(val coord: (Double, Double)) extends GraphElementData {
  def this(data: JsonObject, annotation: JsonObject) =
    this(annotation.get("coord") match {
          case Some(JsonArray(Vector(x,y))) => (x.doubleValue, y.doubleValue)
          case Some(json) => throw new JsonAccessException("Expected: array with 2 elements", json)
          case None => (0,0) })
  def withCoord(c: (Double,Double)): VData
}

case class NodeV(data: JsonObject, annotation: JsonObject) extends VData(data, annotation)
{
  def withCoord(c: (Double,Double)) =
    copy(annotation = annotation + ("coord" -> JsonArray(JsonDouble(c._1), JsonDouble(c._2))))
}

object NodeV {
  def apply(): NodeV = NodeV(JsonObject(),JsonObject())
  def apply(coord: (Double,Double)): NodeV = NodeV().withCoord(coord)
}

case class WireV(data: JsonObject, annotation: JsonObject) extends VData(data,annotation) {
  def withCoord(c: (Double,Double)) =
    copy(annotation = annotation + ("coord" -> JsonArray(JsonDouble(c._1), JsonDouble(c._2))))
}

object WireV {
  def apply(): WireV = WireV(JsonObject(),JsonObject())
  def apply(coord: (Double,Double)): WireV = WireV().withCoord(coord)
}
