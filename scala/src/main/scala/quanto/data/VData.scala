package quanto.data

import quanto.data.Theory.ValueType
import quanto.util.json._
import quanto.data.AngleExpression

/**
 * An abstract class which provides a general interface for accessing
 * vertex data 
 *
 * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/VData.scala Source code]]
 * @see [[https://github.com/Quantomatic/quantomatic/blob/integration/docs/json_formats.txt json_formats.txt]]
 * @author Aleks Kissinger
 */
abstract class VData extends GraphElementData {
  def annotation : JsonObject

  /**
   * Get coordinates of vertex
   * @throws JsonAccessException
   * @return actual coordinates of vertex or (0,0) if none are specified
   */
  def coord: (Double, Double) = annotation.get("coord") match {
    case Some(JsonArray(Vector(x,y))) => (x.doubleValue, y.doubleValue)
    case Some(otherJson) => throw new JsonAccessException("Expected: array with 2 elements", otherJson)
    case None => (0,0)
  }

  /** Create a copy of the current vertex with the new coordinates  */
  def withCoord(c: (Double,Double)): VData

  def isWireVertex: Boolean
  def isBoundary : Boolean
}

/**
 * Companion object for the VData class. Contains a method getCoord which has
 * the same behaviour as VData.coord, but is static.
 * 
 * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/VData.scala Source code]]
 * @see [[https://github.com/Quantomatic/quantomatic/blob/integration/docs/json_formats.txt json_formats.txt]]
 * @author Aleks Kissinger
 */
object VData {
  def getCoord(annotation: Json): (Double,Double) = annotation.get("coord") match {
    case Some(JsonArray(Vector(x,y))) => (x.doubleValue, y.doubleValue)
    case Some(otherJson) => throw new JsonAccessException("Expected: array with 2 elements", otherJson)
    case None => (0,0)
  }
}

/**
 * A class which represents node vertex data.
 * 
 * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/VData.scala Source code]]
 * @see [[https://github.com/Quantomatic/quantomatic/blob/integration/docs/json_formats.txt json_formats.txt]]
 */
case class NodeV(
  data: JsonObject = Theory.DefaultTheory.defaultVertexData,
  annotation: JsonObject = JsonObject(),
  theory: Theory = Theory.DefaultTheory) extends VData
{
  /** Type of the vertex */
  val typ = (data / "type").stringValue

//  def label = data.getOrElse("label","").stringValue
  def typeInfo = theory.vertexTypes(typ)

  // support input of old-style graphs, where data may be stored at value/pretty
  val value: Json = data ? "value" match {
    case str : JsonString => str
    case obj : JsonObject => obj.getOrElse("pretty", JsonString(""))
    case _ => JsonString("")
  }


  // if the theory says this node should have an angle, try to parse it from value,
  // and store it in "angle". If it should have an angle, but parsing fails, set
  // angle to "0".
  lazy val (angle: AngleExpression, hasAngle: Boolean) =
    if (theory.vertexTypes(typ).value.typ == ValueType.AngleExpr)
     try { (AngleExpression.parse(value.stringValue), true) }
     catch { case _: AngleParseException => (AngleExpression(), true) }
    else (AngleExpression(), false)

    def withCoord(c: (Double,Double)) =
      copy(annotation = annotation + ("coord" -> JsonArray(c._1, c._2)))

    /** Create a copy of the current vertex with the new value */
    def withValue(s: String) =
      copy(data = data.setPath("$.value", s).asObject)

    def withTyp(s: String) =
      copy(data = data.setPath("$.type", s).asObject)

    def isWireVertex = false
    def isBoundary = false

    override def toJson =
    if (data == theory.defaultVertexData)
      JsonObject("annotation" -> annotation).noEmpty
    else
      JsonObject(
        "data" -> data,
        "annotation" -> annotation).noEmpty
 }

/**
 * Companion object for the NodeV class. Contains methods to convert to/from 
 * JSON and a factory method to create instances of NodeV from a pair of 
 * coordinates.
 *
 * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/VData.scala Source code]]
 * @see [[https://github.com/Quantomatic/quantomatic/blob/integration/docs/json_formats.txt json_formats.txt]]
 * @author Aleks Kissinger
 */
object NodeV {
  def apply(coord: (Double,Double)): NodeV = NodeV(annotation = JsonObject("coord" -> JsonArray(coord._1,coord._2)))

  def toJson(d: NodeV, theory: Theory) = JsonObject(
    "data" -> (if (d.data == theory.vertexTypes(d.typ).defaultData) JsonNull else d.data),
    "annotation" -> d.annotation).noEmpty
  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory): NodeV = {
    val data = json.getOrElse("data", thy.defaultVertexData).asObject
    val annotation = (json ? "annotation").asObject

    val n = NodeV(data, annotation, thy)
    n.coord // make sure coord is accessible
    if (!thy.vertexTypes.keySet.contains(n.typ)) throw new GraphLoadException("Unrecognized vertex type: " + n.typ)

    n
  }
}

/**
 * A class which represents wire vertex data
 *
 * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/VData.scala Source code]]
 * @see [[https://github.com/Quantomatic/quantomatic/blob/integration/docs/json_formats.txt json_formats.txt]]
 */
case class WireV(
  data: JsonObject = JsonObject(),
  annotation: JsonObject = JsonObject(),
  theory: Theory = Theory.DefaultTheory) extends VData
{
  def isWireVertex = true
  def isBoundary = annotation.get("boundary") match { case Some(JsonBool(b)) => b; case _ => false }
  def withCoord(c: (Double,Double)) =
    copy(annotation = annotation + ("coord" -> JsonArray(c._1, c._2)))
}

/**
 * A companion object for the WireV class. Contains methods to convert to/from
 * JSON and a factory method to create instances of WireV from a pair of 
 * coordinates
 * 
 * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/VData.scala Source code]]
 * @see [[https://github.com/Quantomatic/quantomatic/blob/integration/docs/json_formats.txt json_formats.txt]]
 */
object WireV {
  def apply(c: (Double,Double)): WireV = WireV(annotation = JsonObject("coord" -> JsonArray(c._1,c._2)))

  def toJson(d: NodeV, theory: Theory) = JsonObject(
    "data" -> d.data, "annotation" -> d.annotation).noEmpty
  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory): WireV =
    WireV((json ? "data").asObject, (json ? "annotation").asObject, thy)
}
