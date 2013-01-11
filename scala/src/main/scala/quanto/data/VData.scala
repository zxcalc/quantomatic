package quanto.data

import quanto.util.json._

abstract class VData extends GraphElementData {
  def coord: (Double, Double)
  def withCoord(c: (Double,Double)): VData
  def isWireVertex: Boolean
}

object VData {
  def getCoord(annotation: Json): (Double,Double) = annotation.get("coord") match {
    case Some(JsonArray(Vector(x,y))) => (x.doubleValue, y.doubleValue)
    case Some(otherJson) => throw new JsonAccessException("Expected: array with 2 elements", otherJson)
    case None => (0,0)
  }
}

case class NodeV(
  coord: (Double,Double) = (0.0,0.0),
  typ: String = "",
  value: String = "",
  data: JsonObject = JsonObject(),
  annotation: JsonObject = JsonObject()) extends VData
{
  def isWireVertex = false
  def withCoord(c: (Double,Double)) = copy(coord = c)
  def withValue(s: String) = copy(value = s)
}

object NodeV {
  def fromJson(json: Json, thy: Theory = Theory.defaultTheory): NodeV = {
    val data = json.getOrElse("data", thy.defaultVertexData).asObject
    val annotation = json ?# "annotation"
    val coord = VData.getCoord(annotation)
    val typ = (data / "type").stringValue
    val value = data.getPath(thy.vertexTypes(typ).value.path).stringValue

    NodeV(coord, typ, value, data, annotation)
  }

  def toJson(n: NodeV, thy: Theory = Theory.defaultTheory) = JsonObject(
    "data" -> (n.data.setPath(thy.vertexTypes(n.typ).value.path, n.value)),
    "annotation" -> (n.annotation + ("coord" -> JsonArray(JsonDouble(n.coord._1), JsonDouble(n.coord._2))))
  )
}

case class WireV(
  coord: (Double, Double) = (0.0,0.0),
  data: JsonObject = JsonObject(),
  annotation: JsonObject = JsonObject()) extends VData
{
  def isWireVertex = true
  def withCoord(c: (Double,Double)) = copy(coord = c)
}

object WireV {
  def fromJson(json: Json, thy: Theory = Theory.defaultTheory): WireV = {
    val data = json ?# "data"
    val annotation = json ?# "annotation"
    val coord = VData.getCoord(annotation)

    WireV(coord, data, annotation)
  }

  def toJson(n: WireV , thy: Theory = Theory.defaultTheory) = JsonObject(
    "data" -> n.data,
    "annotation" -> (n.annotation + ("coord" -> JsonArray(JsonDouble(n.coord._1), JsonDouble(n.coord._2))))
  )
}
