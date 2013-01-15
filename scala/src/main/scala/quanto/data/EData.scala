package quanto.data

import quanto.util.json._

abstract class EData extends GraphElementData {
  def isDirected: Boolean

  def typ = (data / "type").stringValue
  def typeInfo = theory.edgeTypes(typ)
  def value = data.getPath(theory.edgeTypes(typ).value.path).stringValue
  def withValue(v: String): EData
}

case class DirEdge(
  data: JsonObject = Theory.DefaultTheory.defaultEdgeData,
  annotation: JsonObject = JsonObject(),
  theory: Theory = Theory.DefaultTheory
) extends EData {
  def isDirected = true
  def withValue(v: String): DirEdge = copy(data = data.setPath(typeInfo.value.path, v).asObject)
  override def toJson = DirEdge.toJson(this, theory)
}
case class UndirEdge(
  data: JsonObject = Theory.DefaultTheory.defaultEdgeData,
  annotation: JsonObject = JsonObject(),
  theory: Theory = Theory.DefaultTheory
) extends EData {
  def isDirected = false
  def withValue(v: String): UndirEdge = copy(data = data.setPath(typeInfo.value.path, v).asObject)
  override def toJson = UndirEdge.toJson(this, theory)
}

object DirEdge {
  def toJson(d: EData, theory: Theory) = JsonObject(
    "data" -> (if (d.data == theory.edgeTypes(d.typ).defaultData) JsonNull() else d.data),
    "annotation" -> d.annotation).noEmpty
  def fromJson(json: Json, theory: Theory) : DirEdge = {
    val data = json.getOrElse("data", theory.defaultEdgeData).asObject
    val annotation = json ?# "annotation"
    DirEdge(data, annotation, theory)
  }
}

object UndirEdge {
  def toJson(d: EData, theory: Theory) = JsonObject(
    "data" -> (if (d.data == theory.edgeTypes(d.typ).defaultData) JsonNull() else d.data),
    "annotation" -> d.annotation).noEmpty
  def fromJson(json: Json, theory: Theory) : UndirEdge = {
    val data = json.getOrElse("data", theory.defaultEdgeData).asObject
    val annotation = json ?# "annotation"
    UndirEdge(data, annotation, theory)
  }
}