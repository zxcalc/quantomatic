package quanto.data

import quanto.util.json._
import JsonValues._
import java.awt.{Color, Shape}

/**
 * Exception thrown when theory cannot be created for some reason
 *
 * @author Aleks Kissinger
 * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/Theory.scala Source code]]
 */
class TheoryLoadException(message: String, cause: Throwable = null)
  extends Exception(message, cause)

/**
 * A class which represents a theory
 *
 * @author Aleks Kissinger
 * @see [[https://github.com/Quantomatic/quantomatic/blob/integration/docs/json_formats.txt json_formats.txt]]
 * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/Theory.scala Source code]]
 */
case class Theory(
  name: String,
  coreName: String,
  vertexTypes: Map[String, Theory.VertexDesc],
  edgeTypes: Map[String, Theory.EdgeDesc] = Map("plain" -> Theory.PlainEdgeDesc),
  defaultVertexType: String,
  defaultEdgeType: String = "plain")
{
  def defaultVertexData = vertexTypes(defaultVertexType).defaultData
  def defaultEdgeData = edgeTypes(defaultEdgeType).defaultData
  override def toString = coreName
}


/**
 * Companion object for the Theory class. Contains useful methods for 
 * converting a Theory to/from JSON object
 *
 * @author Aleks Kissinger
 * @see [[https://github.com/Quantomatic/quantomatic/blob/integration/docs/json_formats.txt json_formats.txt]]
 * @see [[https://github.com/Quantomatic/quantomatic/blob/scala-frontend/scala/src/main/scala/quanto/data/Theory.scala Source code]]
 */
object Theory {
  private implicit def jsonToColor(json: Json) = json match {
    case JsonArray(Vector(r,g,b,a)) => new Color(r.floatValue,g.floatValue,b.floatValue,a.floatValue)
    case JsonArray(Vector(r,g,b))   => new Color(r.floatValue,g.floatValue,b.floatValue)
      case _ => throw new JsonParseException("Expected array of 3 or 4 doubles, got: " + json)
    }
  private implicit def jsonOptToColorOpt(jopt: Option[Json]) = jopt.map(jsonToColor(_))
  private implicit def colorToJson(c: Color) =
    c.getRGBComponents(null) match {
      case Array(r,g,b,1.0) => JsonArray(r,g,b)
      case Array(r,g,b,a) => JsonArray(r,g,b,a)
    }

  object ValueType extends Enumeration with JsonEnumConversions {
    val String = Value("string")
    val LongString = Value("long_string")
    val Enum = Value("enum")
    val Empty = Value("empty")
  }
  type ValueType = ValueType.Value

  object VertexShape extends Enumeration with JsonEnumConversions {
    val Circle = Value("circle")
    val Rectangle = Value("rectangle")
    val Custom = Value("custom")
  }
  type VertexShape = VertexShape.Value

  object VertexLabelPosition extends Enumeration with JsonEnumConversions {
    val Center = Value("center")
    val Inside = Value("inside")
    val Below = Value("below")
  }
  type VertexLabelPosition = VertexLabelPosition.Value

  object EdgeLabelPosition extends Enumeration with JsonEnumConversions {
    val Center = Value("center")
    val Auto = Value("auto")
  }
  type EdgeLabelPosition = EdgeLabelPosition.Value

  case class ValueDesc(
    //by LYH: restore the feature of path
    path: JsonPath = JsonPath("$"),
    typ: ValueType = ValueType.Empty,
    enumOptions: Vector[String] = Vector[String](),
    latexConstants: Boolean = false,
    validateWithCore: Boolean = false
  )
  object ValueDesc {
    implicit def fromJson(json: Json): ValueDesc =
      ValueDesc(
        //by LYH: restore the feature of path
        path = JsonPath(json / "path"),
        typ  = json / "type",
        enumOptions = (json ? "enum_options").vectorValue.map(_.stringValue),
        latexConstants = json.getOrElse("latex_constants", false),
        validateWithCore = json.getOrElse("validate_with_core", false)
      )

    implicit def toJson(v: ValueDesc) =
      JsonObject(
      //by LYH: restore the feature of path
        "path" -> v.path.toString,
        "type" -> v.typ,
        "enum_options" -> v.enumOptions,
        "latex_constants" -> v.latexConstants,
        "validate_with_core" -> v.validateWithCore
      ).noEmpty
  }

  case class VertexStyleDesc(
    shape: VertexShape,
    customShape: Option[Shape] = None,
    strokeColor: Color = Color.BLACK,
    fillColor: Color = Color.WHITE,
    labelPosition: VertexLabelPosition = VertexLabelPosition.Center,
    labelForegroundColor: Color = Color.BLACK,
    labelBackgroundColor: Option[Color] = None
  )
  // TODO: implement custom shapes
  object VertexStyleDesc {
    implicit def fromJson(json: Json) = VertexStyleDesc(
      shape = (json / "shape"),
      customShape = None,
      strokeColor = json.getOrElse("stroke_color", Color.BLACK),
      fillColor = json.getOrElse("fill_color", Color.WHITE),
      labelPosition = (json ? "label").getOrElse("position", VertexLabelPosition.Center),
      labelForegroundColor = (json ? "label").getOrElse("fg_color", Color.BLACK),
      labelBackgroundColor = (json ? "label").get("bg_color")
    )

    implicit def toJson(v: VertexStyleDesc) =
      JsonObject(
        "shape" -> v.shape,
        "custom_shape" -> JsonNull,
        "stroke_color" -> v.strokeColor,
        "fill_color" -> v.fillColor,
        "label" -> JsonObject(
          "position" -> v.labelPosition,
          "fg_color" -> v.labelForegroundColor,
          "bg_color" -> v.labelBackgroundColor.map(x=>x:Json).getOrElse(JsonNull)
        ).noEmpty
      ).noEmpty
  }

  case class EdgeStyleDesc(
    strokeColor: Color = Color.BLACK,
    strokeWidth: Int = 1,
    labelPosition: EdgeLabelPosition = EdgeLabelPosition.Auto,
    labelForegroundColor: Color = Color.BLACK,
    labelBackgroundColor: Option[Color] = None
  )

  object EdgeStyleDesc {
    implicit def fromJson(json: Json) = EdgeStyleDesc(
      strokeColor = json.getOrElse("stroke_color", Color.BLACK),
      strokeWidth = json.getOrElse("stroke_width", 1),
      labelPosition = (json ? "label").getOrElse("position", EdgeLabelPosition.Auto),
      labelForegroundColor = (json ? "label").getOrElse("fg_color", Color.BLACK),
      labelBackgroundColor = (json ? "label").get("bg_color")
    )

    implicit def toJson(v: EdgeStyleDesc) =
      JsonObject(
        "stroke_color" -> v.strokeColor,
        "stroke_width" -> v.strokeWidth,
        "label" -> JsonObject(
          "position" -> v.labelPosition,
          "fg_color" -> v.labelForegroundColor,
          "bg_color" -> v.labelBackgroundColor.map(x=>x:Json).getOrElse(JsonNull)
        ).noEmpty
      ).noEmpty
  }

  case class VertexDesc(
    value: ValueDesc,
    style: VertexStyleDesc,
    defaultData: JsonObject
  )
  object VertexDesc {
    implicit def fromJson(json: Json) = VertexDesc(
      value = json / "value",
      style = json / "style",
      defaultData = (json / "default_data").asObject
    )
    implicit def toJson(v: VertexDesc) = JsonObject(
      "value" -> v.value,
      "style" -> v.style,
      "default_data" -> v.defaultData
    )
  }

  case class EdgeDesc(
    value: ValueDesc,
    style: EdgeStyleDesc,
    defaultData: JsonObject
  )
  object EdgeDesc {
    implicit def fromJson(json: Json) = EdgeDesc(
      value = (json / "value"),
      style = (json / "style"),
      defaultData = (json / "default_data").asObject
    )
    implicit def toJson(v: EdgeDesc) = JsonObject(
      "value" -> v.value,
      "style" -> v.style,
      "default_data" -> v.defaultData
    )
  }

  /** 
   * Same as '''fromJson(json : Json)''', but tries to parse a string to a Json object first 
   * @throws TheoryLoadException
   */
  def fromJson(s: String): Theory =
    try   { fromJson(Json.parse(s)) }
    catch { case e:JsonParseException => throw new TheoryLoadException("Error parsing JSON", e) }

  /**
   * Create a theory instance from a Json object
   * @throws TheoryLoadException 
   */
  def fromJson(json: Json): Theory = {
    try {
      val name = (json / "name").stringValue
      val coreName = (json / "core_name").stringValue
      val vertexTypes = (json / "vertex_types").asObject.mapValues(x => x:VertexDesc)
      val defaultVertexType = (json / "default_vertex_type")
      if (!vertexTypes.contains(defaultVertexType))
        throw new TheoryLoadException("Default vertex type: " + defaultVertexType + " not in list.")

      val edgeTypes = json.get("edge_types") match {
        case Some(et) => et.asObject.mapValues(x => x:EdgeDesc)
        case None => Map("plain"->PlainEdgeDesc)
      }
      val defaultEdgeType = json.getOrElse("default_edge_type", "plain").stringValue

      if (!edgeTypes.contains(defaultEdgeType))
        throw new TheoryLoadException("Default edge type: " + defaultEdgeType + " not in list.")

      Theory(
        name,
        coreName,
        vertexTypes,
        edgeTypes,
        defaultVertexType,
        defaultEdgeType
      )
    } catch {
      case e: JsonAccessException => throw new TheoryLoadException("Error reading JSON", e)
    }
  }

  /** Convert the theory to a JSON object  */
  def toJson(thy: Theory): Json = JsonObject(
    "name" -> thy.name,
    "core_name" -> thy.coreName,
    "vertex_types" -> JsonObject(thy.vertexTypes.mapValues(x => x:Json)),
    "edge_types" -> JsonObject(thy.edgeTypes.mapValues(x => x:Json)),
    "default_vertex_type" -> thy.defaultVertexType,
    "default_edge_type" -> thy.defaultEdgeType
  ).noEmpty

  val PlainEdgeDesc = EdgeDesc(
    value = ValueDesc(typ = ValueType.Empty),
    style = EdgeStyleDesc(),
    defaultData = JsonObject("type" -> "plain")
  )

  val DefaultTheory = Theory(
    name = "String theory",
    coreName = "string_theory",
    vertexTypes = Map(
      "string" -> VertexDesc(
        value = ValueDesc(
          typ = ValueType.String
        ),
        style = VertexStyleDesc(
          shape = VertexShape.Rectangle,
          labelPosition = VertexLabelPosition.Inside
        ),
        defaultData = JsonObject("type" -> "string", "value" -> "")
      )
    ),
    defaultVertexType = "string"
  )
}
