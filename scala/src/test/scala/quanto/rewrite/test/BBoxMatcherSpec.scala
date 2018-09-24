package quanto.rewrite.test

import org.scalatest._
import quanto.data._
import quanto.rewrite._
import quanto.util.json.Json

class BBoxMatcherSpec extends FlatSpec {
  //MatchState.smashSymmetries = false
  val rg = Theory.fromFile("red_green")

  def loadGraph(name: String) =
    Graph.fromJson(Json.parse(new Json.Input(Matcher.getClass.getResourceAsStream(name + ".qgraph"))), rg)

  behavior of "the !-box matcher"

  it should "match a graph with 1 bbox on a graph with no bboxes" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "X"}}
        |  },
        |  "bang_boxes": {
        |    "bb0": {"contents": ["v0"]}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "X"}}
        |  }
        |}
      """.stripMargin), thy = rg)

    val matches = Matcher.findMatches(g1, g2)


    // should match the empty graph, then one copy of the !-box
    assert(matches.size === 2)
  }

  it should "match an empty (wild) bbox twice" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |  },
        |  "bang_boxes": {
        |    "bb0": {"contents": []}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "X"}},
        |    "v1": {"data": {"type": "Z"}}
        |  }
        |}
      """.stripMargin), thy = rg)

    val matches = Matcher.findMatches(g1, g2)

    // This is a wild !-box. So, it should be matched twice, once as a drop and
    // once as a kill.
    assert(matches.size === 2)
  }

  it should "match a graph with 1 bbox on itself" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "X"}}
        |  },
        |  "bang_boxes": {
        |    "bb0": {"contents": ["v0"]}
        |  }
        |}
      """.stripMargin), thy = rg)

    val matches = Matcher.findMatches(g1, g1)

    // should match the empty graph, then one copy of the contents
    assert(matches.size === 2)
  }


  it should "match all vertices, including isolated ones" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": [
        |    "b0"
        |  ],
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "b0"}
        |  },
        |  "bang_boxes": {
        |    "bb0": {"contents": ["b0"]}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "Z"}},
        |    "v2": {"data": {"type": "Z"}},
        |    "v3": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "v1"},
        |    "e1": {"src": "v0", "tgt": "v2"}
        |  }
        |}
      """.stripMargin), thy = rg)

    val matches = Matcher.findMatches(g1, g2)

    // v3 has no neighbours
    // v1 and v2 are connected to v0

    // 2 + 1 + 1 + 1 options (the 2 is from symmetry)

    assert(matches.size === 5)
  }


  it should "match all neighbourless vertices" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |  },
        |  "bang_boxes": {
        |    "bb0": {"contents": ["v0"]}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "Z"}},
        |    "v2": {"data": {"type": "Z"}},
        |    "v3": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "v1"},
        |    "e1": {"src": "v0", "tgt": "v2"}
        |  }
        |}
      """.stripMargin), thy = rg)

    val matches = Matcher.findMatches(g1, g2)

    // v3 has no neighbours
    // the empty match

    assert(matches.size === 2)
  }

  it should "match powerset of neighbourless vertices" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "bang_boxes" : {
        |    "bx0" : {
        |      "contents" : [ "v0" ]
        |    },
        |    "bx1" : {
        |      "contents" : [ "v0" ],
        |      "parent" : "bx0"
        |    }
        |  },
        |  "node_vertices" : {
        |    "v0" : {
        |      "data" : {
        |        "type" : "Z",
        |        "value" : ""
        |      },
        |      "annotation" : {
        |        "coord" : [ -4.75, 2.25 ]
        |      }
        |    }
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "Z"}},
        |    "v3": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "v1"}
        |  }
        |}
      """.stripMargin), thy = rg)

    val matches = Matcher.findMatches(g1, g2)

    // Note bx0 is a wild !-box, since it doesn't contain any vertices which are
    // not also in its children. Hence, it should only be dropped or killed.

    // Should find 3 matches, corresponding to:
    //   1. killing bx0
    //   2. dropping bx0 and killing bx1
    //   3. dropping bx0, expanding bx1, killing bx1

    assert(matches.size === 3)
  }


  it should "match a graph with a bbox on a star graph" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "X"}},
        |    "v1": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "v1"}
        |  },
        |  "bang_boxes": {
        |    "bb0": {"contents": ["v1"]}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "X"}},
        |    "v1": {"data": {"type": "Z"}},
        |    "v2": {"data": {"type": "Z"}},
        |    "v3": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "v1"},
        |    "e1": {"src": "v0", "tgt": "v2"},
        |    "e2": {"src": "v0", "tgt": "v3"}
        |  }
        |}
      """.stripMargin), thy = rg)

    val matches = Matcher.findMatches(g1, g2)

    // with symmetry smashing = 1
    assert(matches.size === 6)
  }

  it should "match the center of a star graph using two boxes" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": [
        |    "b0"
        |  ],
        |  "node_vertices": {
        |    "v0": {"data": {"type": "X"}},
        |    "v1": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "b0"},
        |    "e1": {"src": "v0", "tgt": "v1"}
        |  },
        |  "bang_boxes": {
        |    "bb0": {"contents": ["b0"]},
        |    "bb1": {"contents": ["v1"]}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "X"}},
        |    "v1": {"data": {"type": "Z"}},
        |    "v2": {"data": {"type": "Z"}},
        |    "v3": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "v1"},
        |    "e1": {"src": "v0", "tgt": "v2"},
        |    "e2": {"src": "v0", "tgt": "v3"}
        |  }
        |}
      """.stripMargin), thy = rg)

    val matches = Matcher.findMatches(g1, g2)

    // effectively match on all choices from three things; including (0 matches from one box | 3 from the other)!
    // 1 x 0|3 = 6
    // 3 x 1|2 = 6
    // 3 x 2|1 = 6
    // 1 x 3|0 = 6
    assert(matches.size === 24)
  }


  it should "match a star of stars" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "bang_boxes" : {
        |    "bx0" : {
        |      "contents" : [ "v1", "v2" ]
        |    },
        |    "bx1" : {
        |      "contents" : [ "v2" ],
        |      "parent" : "bx0"
        |    }
        |  },
        |  "node_vertices" : {
        |    "v0" : {
        |      "data" : {
        |        "type" : "X",
        |        "value" : ""
        |      },
        |      "annotation" : {
        |        "coord" : [ -5.0, 2.0 ]
        |      }
        |    },
        |    "v1" : {
        |      "annotation" : {
        |        "coord" : [ -5.0, 5.25 ]
        |      }
        |    },
        |    "v2" : {
        |      "data" : {
        |        "type" : "X",
        |        "value" : ""
        |      },
        |      "annotation" : {
        |        "coord" : [ -5.0, 7.0 ]
        |      }
        |    }
        |  },
        |  "undir_edges" : {
        |    "e0" : {
        |      "src" : "v2",
        |      "tgt" : "v1"
        |    },
        |    "e1" : {
        |      "src" : "v1",
        |      "tgt" : "v0"
        |    }
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices" : {
        |    "v7" : {
        |      "annotation" : {
        |        "coord" : [ -9.0, 1.0 ]
        |      }
        |    },
        |    "v2" : {
        |      "data" : {
        |        "type" : "X",
        |        "value" : ""
        |      },
        |      "annotation" : {
        |        "coord" : [ 4.0, -1.0 ]
        |      }
        |    },
        |    "v6" : {
        |      "annotation" : {
        |        "coord" : [ -3.0, 7.0 ]
        |      }
        |    },
        |    "v5" : {
        |      "annotation" : {
        |        "coord" : [ 3.0, 1.0 ]
        |      }
        |    },
        |    "v1" : {
        |      "data" : {
        |        "type" : "X",
        |        "value" : ""
        |      },
        |      "annotation" : {
        |        "coord" : [ 4.0, 2.0 ]
        |      }
        |    },
        |    "v0" : {
        |      "data" : {
        |        "type" : "X",
        |        "value" : ""
        |      },
        |      "annotation" : {
        |        "coord" : [ -3.0, 1.0 ]
        |      }
        |    },
        |    "v3" : {
        |      "data" : {
        |        "type" : "X",
        |        "value" : ""
        |      },
        |      "annotation" : {
        |        "coord" : [ -3.0, 8.0 ]
        |      }
        |    }
        |  },
        |  "undir_edges" : {
        |    "e1" : {
        |      "src" : "v6",
        |      "tgt" : "v0"
        |    },
        |    "e5" : {
        |      "src" : "v0",
        |      "tgt" : "v7"
        |    },
        |    "e0" : {
        |      "src" : "v3",
        |      "tgt" : "v6"
        |    },
        |    "e4" : {
        |      "src" : "v5",
        |      "tgt" : "v2"
        |    },
        |    "e3" : {
        |      "src" : "v5",
        |      "tgt" : "v1"
        |    },
        |    "e2" : {
        |      "src" : "v0",
        |      "tgt" : "v5"
        |    }
        |  }
        |}
      """.stripMargin), thy = rg)

    val matches = Matcher.findMatches(g1, g2)

    // v0 is the center
    // v5, v6 and v7 are next layer out
    // they connect to a further 2, 1 and 0 more nodes respectively
    // 6 orderings on the first layer, 2 x 1 x 1 orderings on the second layer

    assert(matches.size === 12)
  }



  it should "match an instance of the spider law" in {
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["b0a", "b0b", "b0c", "b1a", "b1b"],
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0a": {"src": "b0a", "tgt": "v0"},
        |    "e0b": {"src": "b0b", "tgt": "v0"},
        |    "e0c": {"src": "b0c", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "v1"},
        |    "e2a": {"src": "v1", "tgt": "b1a"},
        |    "e2b": {"src": "v1", "tgt": "b1b"}
        |  }
        |}
      """.stripMargin), thy = rg)


    val matches = Matcher.findMatches(g2, g2)
    assert(matches.forall(_.isHomomorphism))
    assert(matches.forall(_.isInjective))
    assert(matches.size === 6*2)
  }

  it should "match the LHS of the spider law" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["b0", "b1"],
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "b0", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "v1"},
        |    "e2": {"src": "v1", "tgt": "b1"}
        |  },
        |  "bang_boxes": {
        |    "bb0": {"contents": ["b0"]},
        |    "bb1": {"contents": ["b1"]}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["b0a", "b0b", "b0c", "b1a", "b1b"],
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0a": {"src": "b0a", "tgt": "v0"},
        |    "e0b": {"src": "b0b", "tgt": "v0"},
        |    "e0c": {"src": "b0c", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "v1"},
        |    "e2a": {"src": "v1", "tgt": "b1a"},
        |    "e2b": {"src": "v1", "tgt": "b1b"}
        |  }
        |}
      """.stripMargin), thy = rg)

    val matches = Matcher.findMatches(g1, g2)


    // with symmetry smashing = 2
    assert(matches.size === 24)
  }


  it should "match the spider law on itself" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["b0", "b1"],
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "b0", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "v1"},
        |    "e2": {"src": "v1", "tgt": "b1"}
        |  },
        |  "bang_boxes": {
        |    "bb0": {"contents": ["b0"]},
        |    "bb1": {"contents": ["b1"]}
        |  }
        |}
      """.stripMargin), thy = rg)

    val matches = Matcher.findMatches(g1, g1)

    // Symmetry accounts for 2x

    assert(matches.size === 2)
  }

  it should "match a partially-instantiated spider law" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["b0", "b1"],
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "b0", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "v1"},
        |    "e2": {"src": "v1", "tgt": "b1"}
        |  },
        |  "bang_boxes": {
        |    "bb0": {"contents": ["b0"]},
        |    "bb1": {"contents": ["b1"]}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["b0", "b0a", "b0b", "b1a", "b1b"],
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "b0", "tgt": "v0"},
        |    "e0a": {"src": "b0a", "tgt": "v0"},
        |    "e0b": {"src": "b0b", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "v1"},
        |    "e2a": {"src": "v1", "tgt": "b1a"},
        |    "e2b": {"src": "v1", "tgt": "b1b"}
        |  },
        |  "bang_boxes": {
        |    "bb0": {"contents": ["b0"]}
        |  }
        |}
      """.stripMargin), thy = rg)

    val matches = Matcher.findMatches(g1, g2)

    //matches.foreach(m => println(m.bbops.map(_.shortName)))

    // with symmetry smashing = 2
    // 2x for symmetry of the Z nodes
    // 2x for symmetry at one end
    // 2x for symmetry at the other
    // NOTE assumes the bbox is instantiated at 0!
    assert(matches.size === 8)
  }

  it should "match the LHS of the spider law with angles" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["b0", "b1"],
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z", "value": "x"}},
        |    "v1": {"data": {"type": "Z", "value": "y"}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "b0", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "v1"},
        |    "e2": {"src": "v1", "tgt": "b1"}
        |  },
        |  "bang_boxes": {
        |    "bb0": {"contents": ["b0"]},
        |    "bb1": {"contents": ["b1"]}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["b0a", "b0b", "b0c", "b1a", "b1b"],
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z", "value": ""}},
        |    "v1": {"data": {"type": "Z", "value": "pi"}}
        |  },
        |  "undir_edges": {
        |    "e0a": {"src": "b0a", "tgt": "v0"},
        |    "e0b": {"src": "b0b", "tgt": "v0"},
        |    "e0c": {"src": "b0c", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "v1"},
        |    "e2a": {"src": "v1", "tgt": "b1a"},
        |    "e2b": {"src": "v1", "tgt": "b1b"}
        |  }
        |}
      """.stripMargin), thy = rg)

    val matches = Matcher.findMatches(g1, g2)

    // with symmetry smashing = 2
    assert(matches.size === 24)
  }

  it should "instantiate the RHS of the spider law" in {
    val lhs = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["b0", "b1"],
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "b0", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "v1"},
        |    "e2": {"src": "v1", "tgt": "b1"}
        |  },
        |  "bang_boxes": {
        |    "bb0": {"contents": ["b0"]},
        |    "bb1": {"contents": ["b1"]}
        |  }
        |}
      """.stripMargin), thy = rg)
    val rhs = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["b0", "b1"],
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "b0", "tgt": "v0"},
        |    "e2": {"src": "v0", "tgt": "b1"}
        |  },
        |  "bang_boxes": {
        |    "bb0": {"contents": ["b0"]},
        |    "bb1": {"contents": ["b1"]}
        |  }
        |}
      """.stripMargin), thy = rg)

    val g = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["b0a", "b0b", "b0c", "b1a", "b1b"],
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z", "value": ""}},
        |    "v1": {"data": {"type": "Z", "value": ""}}
        |  },
        |  "undir_edges": {
        |    "e0a": {"src": "b0a", "tgt": "v0"},
        |    "e0b": {"src": "b0b", "tgt": "v0"},
        |    "e0c": {"src": "b0c", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "v1"},
        |    "e2a": {"src": "v1", "tgt": "b1a"},
        |    "e2b": {"src": "v1", "tgt": "b1b"}
        |  }
        |}
      """.stripMargin), thy = rg)

    val matches = Matcher.findMatches(lhs, g)
    assert(matches.nonEmpty)

    val m = matches.head
    val rhs1 = Rewriter.expandRhs(m, rhs)

    assert(m.pattern.boundary === rhs1.boundary)
  }

  it should "match the LHS of bialgebra law on itself" in {
    val bialg = Graph.fromJson(Json.parse(
    """
      |{
      |  "bang_boxes": {
      |    "bx0":{"contents":["b0","v0"]},
      |    "bx2":{"contents":["b1","v1"]}
      |  },
      |  "wire_vertices":["b0","b1"],
      |  "node_vertices":{
      |    "v0":{"data":{"type":"X","value":""}},
      |    "v1":{"data":{"type":"Z","value":""}}
      |  },
      |  "undir_edges":{
      |    "e0":{"src":"b0","tgt":"v0"},
      |    "e1":{"src":"v0","tgt":"v1"},
      |    "e2":{"src":"v1","tgt":"b1"}
      |  }
      |}
    """.stripMargin
    ), thy=rg)

    val matches = Matcher.findMatches(bialg, bialg)

    // should match once as empty graph (killing both bboxes), and once as id
    assert(matches.size === 2)
  }

  it should "match the LHS of bialgebra law on two id spiders" in {
    val bialg = Graph.fromJson(Json.parse(
      """
        |{
        |  "bang_boxes": {
        |    "bx0":{"contents":["b0","v0"]},
        |    "bx2":{"contents":["b1","v1"]}
        |  },
        |  "wire_vertices":["b0","b1"],
        |  "node_vertices":{
        |    "v0":{"data":{"type":"X","value":""}},
        |    "v1":{"data":{"type":"Z","value":""}}
        |  },
        |  "undir_edges":{
        |    "e0":{"src":"b0","tgt":"v0"},
        |    "e1":{"src":"v0","tgt":"v1"},
        |    "e2":{"src":"v1","tgt":"b1"}
        |  }
        |}
      """.stripMargin
    ), thy=rg)

    val bialgInst = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices":["b0","b1"],
        |  "node_vertices":{
        |    "v0":{"data":{"type":"X","value":""}},
        |    "v1":{"data":{"type":"Z","value":""}}
        |  },
        |  "undir_edges":{
        |    "e0":{"src":"b0","tgt":"v0"},
        |    "e1":{"src":"v0","tgt":"v1"},
        |    "e2":{"src":"v1","tgt":"b1"}
        |  }
        |}
      """.stripMargin
    ), thy=rg)

    val matches = Matcher.findMatches(bialg, bialgInst)
    matches.size

    // should match once as empty graph (killing both bboxes), and once using one copy of each bbox
    assert(matches.size === 2)
  }

  it should "match the LHS of bialgebra law on a 4-cycle" in {
    val bialg = Graph.fromJson(Json.parse(
      """
        |{
        |  "bang_boxes": {
        |    "bx0":{"contents":["b0","v0"]},
        |    "bx2":{"contents":["b1","v1"]}
        |  },
        |  "wire_vertices":["b0","b1"],
        |  "node_vertices":{
        |    "v0":{"data":{"type":"X","value":""}},
        |    "v1":{"data":{"type":"Z","value":""}}
        |  },
        |  "undir_edges":{
        |    "e0":{"src":"b0","tgt":"v0"},
        |    "e1":{"src":"v0","tgt":"v1"},
        |    "e2":{"src":"v1","tgt":"b1"}
        |  }
        |}
      """.stripMargin
    ), thy=rg)

    val bialgInst = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices":["b0a","b1a","b0b","b1b"],
        |  "node_vertices":{
        |    "v0a":{"data":{"type":"X","value":""}},
        |    "v0b":{"data":{"type":"X","value":""}},
        |    "v1a":{"data":{"type":"Z","value":""}},
        |    "v1b":{"data":{"type":"Z","value":""}}
        |  },
        |  "undir_edges":{
        |    "e0a":{"src":"b0a","tgt":"v0a"},
        |    "e0b":{"src":"b0b","tgt":"v0b"},
        |    "e1aa":{"src":"v0a","tgt":"v1a"},
        |    "e1ab":{"src":"v0a","tgt":"v1b"},
        |    "e1ba":{"src":"v0b","tgt":"v1a"},
        |    "e1bb":{"src":"v0b","tgt":"v1b"},
        |    "e2a":{"src":"v1a","tgt":"b1a"},
        |    "e2b":{"src":"v1b","tgt":"b1b"}
        |  }
        |}
      """.stripMargin
    ), thy=rg)

    val matches = Matcher.findMatches(bialg, bialgInst)
    matches.size

    // should match once as empty graph (killing both bboxes), and once using two copies of each bbox
    // double the non-empty matches for symmetry
    assert(matches.size === 5)
  }

  it should "match spider-law with multiple connecting edges (and no external ones)" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["w1"],
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e3": {"src": "v0", "tgt": "w1"},
        |    "e4": {"src": "v1", "tgt": "w1"}
        |  },
        |  "bang_boxes": {
        |    "bb2": {"contents": ["w1"]}
        |  }
        |}
      """.stripMargin), thy = rg)


    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "Z"}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "v1"},
        |    "e1": {"src": "v0", "tgt": "v1"}
        |  }
        |}
      """.stripMargin), thy = rg)

    val matches = Matcher.findMatches(g1, g1)

    assert(matches.size === 2)
  }
}
