package quanto.data

import quanto.util.json._

abstract class EData extends GraphElementData {
  def isDirected: Boolean

  def typ = (data / "type").stringValue
  def value = data.getPath(theory.edgeTypes(typ).value.path).stringValue
  def withValue(v: String): EData

  override def toJson = JsonObject(
    "data" -> (if (data == theory.edgeTypes(typ).defaultData) JsonNull() else data),
    "annotation" -> annotation).noEmpty
}

case class DirEdge(
  data: JsonObject = Theory.DefaultTheory.defaultEdgeData,
  annotation: JsonObject = JsonObject(),
  theory: Theory = Theory.DefaultTheory
) extends EData {
  def isDirected = true

  def withValue(v: String): DirEdge = copy(data = data.setPath(theory.edgeTypes(typ).value.path, v).asObject)
}
case class UndirEdge(
  data: JsonObject = Theory.DefaultTheory.defaultEdgeData,
  annotation: JsonObject = JsonObject(),
  theory: Theory = Theory.DefaultTheory
) extends EData {
  def isDirected = false

  def withValue(v: String): UndirEdge = copy(data = data.setPath(theory.edgeTypes(typ).value.path, v).asObject)
}