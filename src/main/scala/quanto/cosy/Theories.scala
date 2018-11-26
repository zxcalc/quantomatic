package quanto.cosy

import quanto.cosy.Interpreter.{ZHSpiderData, ZHSpiderType}
import quanto.data.{NodeV, Theory}
import quanto.data.Theory.{ValueDesc, ValueType, VertexDesc, VertexShape, VertexStyleDesc}
import quanto.util.Rational
import quanto.util.json.JsonObject

object Theories {

  val ZH: Theory = new Theory("zh", "zh",
    vertexTypes = Map(
      "Z" -> VertexDesc(
        value = ValueDesc(
          typ = Vector(ValueType.Empty)),
        style = VertexStyleDesc(shape = VertexShape.Circle),
        defaultData = JsonObject()),
      "H" -> VertexDesc(
        value = ValueDesc(
          typ = Vector(ValueType.Rational, ValueType.Rational)),
        style = VertexStyleDesc(shape = VertexShape.Rectangle),
        defaultData = JsonObject()),
    ),
    defaultVertexType = "Z")

}
