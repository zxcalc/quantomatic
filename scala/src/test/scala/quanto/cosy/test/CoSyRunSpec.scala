package quanto.cosy.test

import org.scalatest.FlatSpec
import quanto.cosy.CoSyRuns._
import quanto.cosy._

import scala.concurrent.duration.Duration
import java.io.File

import quanto.data._
import quanto.rewrite.Matcher
import quanto.util.FileHelper
import quanto.util.json.Json

import scala.util.Random

/**
  * Created by hector on 24/05/17.
  */
class CoSyRunSpec extends FlatSpec {

  behavior of "ZX"

  it should "do a small run" in {
    var theory = Theory.fromFile("red_green")
    var CR = new CoSyRuns.CoSyZX(duration = Duration.Inf,
      numBoundaries = List(0, 1, 2, 3),
      outputDir = None,
      scalars = true,
      numVertices = 2,
      rulesDir = new File(""), theory = theory,
      numAngles = 2)
    // Don't test this here
    // It isn't a standard feature
    // And pollutes the filesystem
    // Ask hmillerbakewell@gmail.com for more information
    //CR.begin()
    assert(1 == 1)
    //var reduced = RuleSynthesis.minimiseRuleset(CR.reductionRules, theory, new Random(1))
    //reduced.foreach(r => FileHelper.printJson(s"./cosy/${r.lhs.hashCode}-${r.rhs.hashCode}.qrule", Rule.toJson(r, theory)))
  }

  behavior of "ZX Circuit"

  it should "do a small run" in {
    val theory = Theory.fromJson("""
                                   |{
                                   |    "name": "ZXH",
                                   |    "core_name": "zxh",
                                   |    "vertex_types": {
                                   |        "Z": {
                                   |            "value": {
                                   |                "type": "angle_expr",
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
                                   |                "value": "0"
                                   |            }
                                   |        },
                                   |        "X": {
                                   |            "value": {
                                   |                "type": "angle_expr",
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
                                   |                "value": "0"
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
    var CR = new CoSyRuns.CoSyCircuit(duration = Duration(10,"minutes"), numBoundaries = 2, outputDir = None,
      rulesDir = new File("./cosyrun/"), theory = theory)
   //var rules = CR.begin()
    assert(1 == 1)
  }

}
