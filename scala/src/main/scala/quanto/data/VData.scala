package quanto.data

import quanto.util.json._

abstract class VData extends GraphElementData {
  def coord: (Double, Double) = annotation.get("coord") match {
    case Some(JsonArray(Vector(x,y))) => (x.doubleValue, y.doubleValue)
    case Some(otherJson) => throw new JsonAccessException("Expected: array with 2 elements", otherJson)
    case None => (0,0)
  }

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
  data: JsonObject = Theory.DefaultTheory.defaultVertexData,
  annotation: JsonObject = JsonObject(),
  theory: Theory = Theory.DefaultTheory) extends VData
{
  def typ = (data / "type").stringValue
  def value = (data.getPath(theory.vertexTypes(typ).value.path)).stringValue
  def typeInfo = theory.vertexTypes(typ)

  def withCoord(c: (Double,Double)) =
    copy(annotation = (annotation + ("coord" -> JsonArray(c._1, c._2))))
  def withValue(s: String) =
    copy(data = data.setPath(theory.vertexTypes(typ).value.path, s).asObject)

  def isWireVertex = false

  override def toJson = JsonObject(
    "data" -> (if (data == theory.vertexTypes(typ).defaultData) JsonNull() else data),
    "annotation" -> annotation).noEmpty
}

object NodeV {
  def apply(coord: (Double,Double)): NodeV = NodeV(annotation = JsonObject("coord" -> JsonArray(coord._1,coord._2)))

  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory): NodeV = {
    val data = json.getOrElse("data", thy.defaultVertexData).asObject
    val annotation = json ?# "annotation"

    VData.getCoord(annotation)
    val typ = (data / "type").stringValue
    if (!thy.vertexTypes.keySet.contains(typ)) throw new GraphLoadException("Unrecognized vertex type: " + typ)
    data.getPath(thy.vertexTypes(typ).value.path).stringValue

    NodeV(data, annotation, thy)
  }
}

case class WireV(
  data: JsonObject = JsonObject(),
  annotation: JsonObject = JsonObject(),
  theory: Theory = Theory.DefaultTheory) extends VData
{
  def isWireVertex = true
  def withCoord(c: (Double,Double)) =
    copy(annotation = (annotation + ("coord" -> JsonArray(c._1, c._2))))
}

object WireV {
  def apply(c: (Double,Double)): WireV = WireV(annotation = JsonObject("coord" -> JsonArray(c._1,c._2)))

  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory): WireV =
    WireV(json ?# "data", json ?# "annotation", thy)
}
