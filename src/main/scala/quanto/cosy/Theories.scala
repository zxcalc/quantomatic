package quanto.cosy

import quanto.data.Theory
import quanto.data.Theory.{ValueDesc, ValueType, VertexDesc, VertexShape, VertexStyleDesc}
import quanto.util.json.{Json, JsonObject}

object Theories {

  val ZH: Theory = new Theory("zh", "zh",
    vertexTypes = Map(
      "Z" -> VertexDesc(
        value = ValueDesc(
          typ = Vector(ValueType.Empty)),
        style = VertexStyleDesc(shape = VertexShape.Circle),
        defaultData = JsonObject(
          "type" -> Json.stringToJson("Z"),
          "value" -> Json.stringToJson("0")
        )),
      "H" -> VertexDesc(
        value = ValueDesc(
          typ = Vector(ValueType.Rational, ValueType.Rational)),
        style = VertexStyleDesc(shape = VertexShape.Rectangle),
        defaultData = JsonObject(
          "type" -> Json.stringToJson("H"),
          "value" -> Json.stringToJson("-1,0")
        )),
    ),
    defaultVertexType = "Z")

}
