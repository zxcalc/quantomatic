package quanto.data

import quanto.util.json._
import JsonValues._
import java.awt.{Color, Shape}

case class Theory(
  name: String,
  coreName: String,
  vertexTypes: Map[String, Theory.VertexDesc] = Map(),
  edgeTypes: Map[String, Theory.EdgeDesc] = Map(),
  defaultVertexData: JsonObject = JsonObject(),
  defaultEdgeData: JsonObject = JsonObject()
)



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
    path: JsonPath = JsonPath("$"),
    typ: ValueType = ValueType.Empty,
    enumOptions: List[String] = List[String](),
    default: String = "",
    latexConstants: Boolean = false,
    validateWithCore: Boolean = false
  )
  object ValueDesc {
    implicit def fromJson(json: Json): ValueDesc =
      ValueDesc(
        path = JsonPath(json / "path"),
        typ  = json / "type",
        enumOptions = (json ?@ "enum_options").toList.map(_.stringValue),
        default = json / "default",
        latexConstants = json.getOrElse("latex_constants", false),
        validateWithCore = json.getOrElse("validate_with_core", false)
      )

    implicit def toJson(v: ValueDesc) =
      JsonObject(
        "path" -> v.path.toString,
        "type" -> v.typ,
        "enum_options" -> v.enumOptions,
        "default" -> v.default,
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
      labelPosition = (json ?# "label").getOrElse("position", VertexLabelPosition.Center),
      labelForegroundColor = (json ?# "label").getOrElse("fg_color", Color.BLACK),
      labelBackgroundColor = (json ?# "label").get("bg_color")
    )

    implicit def toJson(v: VertexStyleDesc) =
      JsonObject(
        "shape" -> v.shape,
        "custom_shape" -> JsonNull(),
        "stroke_color" -> v.strokeColor,
        "fill_color" -> v.fillColor,
        "label" -> JsonObject(
          "position" -> v.labelPosition,
          "fg_color" -> v.labelForegroundColor,
          "bg_color" -> v.labelBackgroundColor.map(x=>x:Json).getOrElse(JsonNull())
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
      labelPosition = (json ?# "label").getOrElse("position", EdgeLabelPosition.Auto),
      labelForegroundColor = (json ?# "label").getOrElse("fg_color", Color.BLACK),
      labelBackgroundColor = (json ?# "label").get("bg_color")
    )

    implicit def toJson(v: EdgeStyleDesc) =
      JsonObject(
        "stroke_color" -> v.strokeColor,
        "stroke_width" -> v.strokeWidth,
        "label" -> JsonObject(
          "position" -> v.labelPosition,
          "fg_color" -> v.labelForegroundColor,
          "bg_color" -> v.labelBackgroundColor.map(x=>x:Json).getOrElse(JsonNull())
        ).noEmpty
      ).noEmpty
  }

  case class VertexDesc(
    value: ValueDesc,
    style: VertexStyleDesc
  )
  object VertexDesc {
    implicit def fromJson(json: Json) = VertexDesc(
      value = json / "value",
      style = json / "style"
    )
    implicit def toJson(v: VertexDesc) = JsonObject(
      "value" -> v.value,
      "style" -> v.style
    )
  }

  case class EdgeDesc(
    value: ValueDesc,
    style: VertexStyleDesc
  )
  object EdgeDesc {
    implicit def fromJson(json: Json) = EdgeDesc(
      value = (json / "value"),
      style = (json / "style")
    )
    implicit def toJson(v: EdgeDesc) = JsonObject(
      "value" -> v.value,
      "style" -> v.style
    )
  }

  def fromJson(s: String): Theory = fromJson(Json.parse(s))
  def fromJson(json: Json): Theory =
    Theory(
      name = (json / "name"),
      coreName = (json / "core_name"),
      vertexTypes = (json ?# "vertex_types").mapValues(x => x:VertexDesc),
      edgeTypes = (json ?# "edge_types").mapValues(x => x:EdgeDesc),
      defaultVertexData = (json ?# "default_vertex_data"),
      defaultEdgeData = (json ?# "default_edge_data")
    )

  def toJson(thy: Theory): Json = JsonObject(
    "name" -> thy.name,
    "core_name" -> thy.coreName,
    "vertex_types" -> JsonObject(thy.vertexTypes.mapValues(x => x:Json)),
    "edge_types" -> JsonObject(thy.edgeTypes.mapValues(x => x:Json)),
    "default_vertex_data" -> thy.defaultVertexData,
    "default_edge_data" -> thy.defaultEdgeData
  ).noEmpty


  def defaultTheory = Theory(
    name = "String theory",
    coreName = "string_theory",
    vertexTypes = Map(
      "string" -> VertexDesc(
        value = ValueDesc(
          path = JsonPath("$.value"),
          typ = ValueType.String
        ),
        style = VertexStyleDesc(
          shape = VertexShape.Rectangle,
          labelPosition = VertexLabelPosition.Inside
        )
      )
    ),
    defaultVertexData = JsonObject("type"->"string", "value"->"")
  )
}
