package quanto.data

import quanto.util.json._

/**
  * An abstract class providing an interface for accessing edge data
  *
  * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/EData.scala Source code]]
  * @see [[https://github.com/Quantomatic/quantomatic/blob/integration/docs/json_formats.txt json_formats.txt]]
  * @author Aleks Kissinger
  */
abstract class EData extends GraphElementData {
  def isDirected: Boolean

  def typeInfo = theory.edgeTypes(typ)

  /** type of the edge */
  def typ: String = (data / "type").stringValue

  def label: String = data.getOrElse("label", "").stringValue

  def value: Json = data ? "value"

  /** Create a copy of the current edge data, but with the new value */
  def withValue(v: String): EData

  def toDirEdge = DirEdge(data, annotation, theory)

  def toUndirEdge = UndirEdge(data, annotation, theory)
}

/**
  * A class which represents directed edge data.
  *
  * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/EData.scala Source code]]
  * @see [[https://github.com/Quantomatic/quantomatic/blob/integration/docs/json_formats.txt json_formats.txt]]
  */
case class DirEdge(
                    data: JsonObject = Theory.DefaultTheory.defaultEdgeData,
                    annotation: JsonObject = JsonObject(),
                    theory: Theory = Theory.DefaultTheory
                  ) extends EData {
  def isDirected = true

  def withValue(v: String): DirEdge = copy(data = data.setPath("$.value", v).setPath("$.label", v).asObject)

  override def toJson: JsonObject = DirEdge.toJson(this, theory)
}

/**
  * A class which represents undirected edge data.
  *
  * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/EData.scala Source code]]
  * @see [[https://github.com/Quantomatic/quantomatic/blob/integration/docs/json_formats.txt json_formats.txt]]
  */
case class UndirEdge(
                      data: JsonObject = Theory.DefaultTheory.defaultEdgeData,
                      annotation: JsonObject = JsonObject(),
                      theory: Theory = Theory.DefaultTheory
                    ) extends EData {
  def isDirected = false

  def withValue(v: String): UndirEdge = copy(data = data.setPath("$.value", v).setPath("$.label", v).asObject)

  override def toJson: JsonObject = UndirEdge.toJson(this, theory)
}

/**
  * Companion object for the DirEdge class. Contains methods to convert to/from
  * JSON
  *
  * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/EData.scala Source code]]
  * @see [[https://github.com/Quantomatic/quantomatic/blob/integration/docs/json_formats.txt json_formats.txt]]
  */
object DirEdge {
  def toJson(d: EData, theory: Theory): JsonObject = JsonObject(
    "data" -> (if (d.typ == theory.defaultEdgeType && d.data == theory.edgeTypes(d.typ).defaultData) {
      JsonNull
    } else {
      d.data
    }),
    "annotation" -> d.annotation).noEmpty

  def fromJson(json: Json, theory: Theory): DirEdge = {
    val data = json.getOrElse("data", theory.defaultEdgeData).asObject
    val annotation = (json ? "annotation").asObject
    DirEdge(data, annotation, theory)
  }
}

/**
  * Companion object for the UndirEdge class. Contains methods to convert
  * to/from JSON
  *
  * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/EData.scala Source code]]
  * @see [[https://github.com/Quantomatic/quantomatic/blob/integration/docs/json_formats.txt json_formats.txt]]
  */
object UndirEdge {
  def toJson(d: EData, theory: Theory): JsonObject = JsonObject(
    "data" -> (if (d.typ == theory.defaultEdgeType && d.data == theory.edgeTypes(d.typ).defaultData) {
      JsonNull
    } else {
      d.data
    }),
    "annotation" -> d.annotation).noEmpty

  def fromJson(json: Json, theory: Theory): UndirEdge = {
    val data = json.getOrElse("data", theory.defaultEdgeData).asObject
    val annotation = (json ? "annotation").asObject
    UndirEdge(data, annotation, theory)
  }
}
