package quanto.cosy.test

import org.scalatest.FlatSpec
import quanto.cosy.CoSyRuns._
import quanto.cosy._

import scala.concurrent.duration.Duration
import java.io.File

import quanto.data.Theory.VertexDesc
import quanto.data._
import quanto.rewrite.Matcher
import quanto.util.FileHelper
import quanto.util.json.{Json, JsonObject}

import scala.util.Random

/**
  * Created by hector on 24/05/17.
  */
class CoSyRunSpec extends FlatSpec {

  behavior of "ZX"

  it should "do a small run" in {
    var theory = Theory.fromFile("red_green")
    var CR = new CoSyRuns.CoSyZX(duration = Duration.Inf,
      numBoundaries = List(0, 1, 2),
      outputDir = None,
      scalars = false,
      numVertices = 2,
      rulesDir = new File("./cosy/"), theory = theory,
      numAngles = 4)

    def interpret(g: Graph) = Interpreter.interpretZXGraph(g, g.verts.filter(g.isBoundary).toList, List())
    // Don't test this here
    // It isn't a standard feature
    // And pollutes the filesystem
    // Ask hmillerbakewell@gmail.com for more information

    // CR.begin()
    /*
    val reduced = RuleSynthesis.minimiseRuleset(CR.reductionRules, theory, new Random(1))
    //reduced.foreach(r => FileHelper.printJson(s"./cosy/${r.lhs.hashCode}-${r.rhs.hashCode}.qrule", Rule.toJson(r, theory)))
    reduced.foreach(r =>
      assert(interpret(r.lhs).isRoughly(interpret(r.rhs))))
      */

  }


  behavior of "ZX with bool"

  it should "do a small run" in {
    val theory = Theory.fromJson(
      """
        |{
        |    "name": "ZXH",
        |    "core_name": "zxh",
        |    "vertex_types": {
        |        "Z": {
        |            "value": {
        |                "type": "angle_expr, bool",
        |                "latex_constants": true,
        |                "validate_with_core": false
        |            },
        |            "style": {
        |                "label": {
        |                    "position": "center",
        |                    "fg_color": [
        |                        0.0,
        |                        0.0,
        |                        0.0
        |                    ]
        |                },
        |                "stroke_color": [
        |                    0.0,
        |                    0.0,
        |                    0.0
        |                ],
        |                "fill_color": [
        |                    0.0,
        |                    1.0,
        |                    0.0
        |                ],
        |                "shape": "circle"
        |            },
        |            "default_data": {
        |                "type": "Z",
        |                "value": "0, 0"
        |            }
        |        },
        |        "X": {
        |            "value": {
        |                "type": "angle_expr, bool",
        |                "latex_constants": true,
        |                "validate_with_core": false
        |            },
        |            "style": {
        |                "label": {
        |                    "position": "center",
        |                    "fg_color": [
        |                        0.0,
        |                        0.0,
        |                        0.0
        |                    ]
        |                },
        |                "stroke_color": [
        |                    0.0,
        |                    0.0,
        |                    0.0
        |                ],
        |                "fill_color": [
        |                    1.0,
        |                    0.0,
        |                    0.0
        |                ],
        |                "shape": "circle"
        |            },
        |            "default_data": {
        |                "type": "X",
        |                "value": "0, 0"
        |            }
        |        },
        |        "hadamard": {
        |            "value": {
        |                "type": "empty",
        |                "latex_constants": true,
        |                "validate_with_core": false
        |            },
        |            "style": {
        |                "label": {
        |                    "position": "center",
        |                    "fg_color": [
        |                        0.0,
        |                        0.0,
        |                        0.0
        |                    ]
        |                },
        |                "stroke_color": [
        |                    0.0,
        |                    0.0,
        |                    0.0
        |                ],
        |                "fill_color": [
        |                    1.0,
        |                    1.0,
        |                    0.0
        |                ],
        |                "shape": "rectangle"
        |            },
        |            "default_data": {
        |                "type": "hadamard",
        |                "value": ""
        |            }
        |        },
        |        "dummyBoundary": {
        |            "value": {
        |                "type": "empty",
        |                "latex_constants": true,
        |                "validate_with_core": false
        |            },
        |            "style": {
        |                "label": {
        |                    "position": "center",
        |                    "fg_color": [
        |                        0.0,
        |                        0.0,
        |                        0.0
        |                    ]
        |                },
        |                "stroke_color": [
        |                    0.0,
        |                    0.0,
        |                    0.0
        |                ],
        |                "fill_color": [
        |                    0.0,
        |                    1.0,
        |                    1.0
        |                ],
        |                "shape": "rectangle"
        |            },
        |            "default_data": {
        |                "type": "dummyBoundary",
        |                "value": ""
        |            }
        |        }
        |    },
        |    "default_vertex_type": "Z",
        |    "default_edge_type": "plain",
        |    "edge_types": {
        |        "plain": {
        |            "value": {
        |                "type": "empty",
        |                "latex_constants": false,
        |                "validate_with_core": false
        |            },
        |            "style": {
        |                "stroke_color": [
        |                    0.0,
        |                    0.0,
        |                    0.0
        |                ],
        |                "stroke_width": 1,
        |                "label": {
        |                    "position": "auto",
        |                    "fg_color": [
        |                        0.0,
        |                        0.0,
        |                        0.0
        |                    ]
        |                }
        |            },
        |            "default_data": {
        |                "type": "plain"
        |            }
        |        }
        |    }
        |}
      """.stripMargin)
    var CR = new CoSyRuns.CoSyZXBool(duration = Duration.Inf,
      numBoundaries = List(0, 1, 2),
      outputDir = None,
      scalars = false,
      numVertices = 3,
      rulesDir = new File("./cosy/"), theory = theory,
      numAngles = 4)

    // Don't test this here
    // It isn't a standard feature
    // And pollutes the filesystem
    // Ask hmillerbakewell@gmail.com for more information

    //CR.begin()
    assert(1 == 1)
    /*
    val reduced = RuleSynthesis.minimiseRuleset(CR.reductionRules, theory, new Random(1))
    //reduced.foreach(r => FileHelper.printJson(s"./cosy/${r.lhs.hashCode}-${r.rhs.hashCode}.qrule", Rule.toJson(r, theory)))
    reduced.foreach(r =>
      assert(interpret(r.lhs).isRoughly(interpret(r.rhs))))
      */

  }

  behavior of "ZX Circuit"

  it should "do a small run" in {
    val ZXRails = Theory.fromFile("ZXRails")

    // THIS WILL RUN UNTIL THE TIME RUNS OUT.
    var CR = new CoSyRuns.CoSyCircuit(duration = Duration(4, "minutes"), numBoundaries = 3,
      outputDir = Some(new File("./cosy_synth/")),
      rulesDir = new File("./cosy_synth/"), theory = ZXRails)
    //var rules = CR.begin()
    /*
    val reduced = RuleSynthesis.minimiseRuleset(CR.reductionRules, theory, new Random(1))
    reduced.foreach(r => FileHelper.printJson(s"./cosy/${r.lhs.hashCode}-${r.rhs.hashCode}.qrule", Rule.toJson(r, theory)))
    */
    assert(1 == 1)
  }

}
