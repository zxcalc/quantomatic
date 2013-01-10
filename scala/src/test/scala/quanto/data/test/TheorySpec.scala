package quanto.data.test

import quanto.data._
import quanto.util.json._
import org.scalatest._
import java.awt.Color

class TheorySpec extends FlatSpec {
  behavior of "A theory"

  val rgValueDesc = Theory.ValueDesc(
    path = JsonPath("$.angle.pretty"),
    typ = Theory.ValueType.String,
    default = "0",
    latexConstants = true,
    validateWithCore = true
  )

  val hValueDesc = Theory.ValueDesc(
    path = JsonPath("$"),
    typ = Theory.ValueType.Empty,
    default = "",
    latexConstants = false,
    validateWithCore = false
  )

  val rStyleDesc = Theory.VertexStyleDesc(
    shape = Theory.VertexShape.Circle,
    strokeColor = Color.BLACK,
    fillColor = Color.RED,
    labelPosition = Theory.VertexLabelPosition.Below,
    labelForegroundColor = Color.BLACK,
    labelBackgroundColor = None
  )

  val gStyleDesc = rStyleDesc.copy(fillColor = Color.GREEN)

  val hStyleDesc = rStyleDesc.copy(
    shape = Theory.VertexShape.Rectangle,
    fillColor = Color.YELLOW,
    labelPosition = Theory.VertexLabelPosition.Center
  )

  val thy = Theory(
    name = "Test Theory",
    coreName = "test_theory",
    vertexTypes = Map(
      "red" -> Theory.VertexDesc(
        value = rgValueDesc,
        style = rStyleDesc
      ),
      "green" -> Theory.VertexDesc(
        value = rgValueDesc,
        style = gStyleDesc
      ),
      "hadamard" -> Theory.VertexDesc(
        value = hValueDesc,
        style = hStyleDesc
      )
    )
  )

  val thyJson = Json.parse(
    """
      |{
      |  "name": "Test Theory",
      |  "core_name": "test_theory",
      |  "vertex_types": {
      |
      |    "red": {
      |      "value": {
      |        "path": "$.angle.pretty",
      |        "type": "string",
      |        "default": "0",
      |        "latex_constants": true,
      |        "validate_with_core": true
      |      },
      |      "style": {
      |        "shape": "circle",
      |        "stroke_color": [0.0,0.0,0.0],
      |        "fill_color": [1.0,0.0,0.0],
      |        "label": {
      |          "position": "below",
      |          "fg_color": [0.0,0.0,0.0]
      |        }
      |      }
      |    },
      |
      |    "green": {
      |      "value": {
      |        "path": "$.angle.pretty",
      |        "type": "string",
      |        "default": "0",
      |        "latex_constants": true,
      |        "validate_with_core": true
      |      },
      |      "style": {
      |        "shape": "circle",
      |        "stroke_color": [0.0,0.0,0.0],
      |        "fill_color": [0.0,1.0,0.0],
      |        "label": {
      |          "position": "below",
      |          "fg_color": [0.0,0.0,0.0]
      |        }
      |      }
      |    },
      |
      |    "hadamard": {
      |      "value": {
      |        "path": "$",
      |        "type": "empty",
      |        "default": "",
      |        "latex_constants": false,
      |        "validate_with_core": false
      |      },
      |      "style": {
      |        "shape": "rectangle",
      |        "stroke_color": [0.0,0.0,0.0],
      |        "fill_color": [1.0,1.0,0.0],
      |        "label": {
      |          "position": "center",
      |          "fg_color": [0.0,0.0,0.0]
      |        }
      |      }
      |    }
      |
      |  }
      |}
    """.stripMargin)

  it should "save to JSON" in {
    assert(Theory.toJson(thy) === thyJson)
  }

  it should "load from JSON" in {
    assert(Theory.fromJson(thyJson) === thy)
  }
}
