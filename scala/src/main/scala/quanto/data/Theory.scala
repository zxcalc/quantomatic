package quanto.data

import quanto.util.json._
import JsonValues._

case class Theory(
  name: String,
  coreName: String
)



object Theory {


  case class ValueDesc(path: String, typ: Symbol, enumOptions: List[String])
  object ValueDesc {
    def fromJson(json: Json): ValueDesc =
      ValueDesc(
        path = (json / "path"),
        typ  = (json / "type").stringValue match {
          case "string" => 'string
          case "long_string" => 'longString
          case "enum" => 'enum
          case s => throw new JsonParseException("Expected \"string\", \"long_string\", or \"enum\", got: " + s)
        },
        enumOptions = (json ?@ "enum_options").foldRight(List[String]()) { _.stringValue :: _ }
      )
  }

  def fromJson(s: String): Theory = fromJson(Json.parse(s))
  def fromJson(json: Json): Theory =
    Theory(
      name = (json / "name"),
      coreName = (json / "core_name")
    )
}
