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

    // without symmetry smashing = 6
    assert(matches.size === 1)
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


    // without symmetry smashing = 2*6*2
    assert(matches.size === 2)
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

    // // without symmetry smashing = 2*2*2
    assert(matches.size === 2)
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

    // without symmetry smashing = 2*6*2
    assert(matches.size === 2)
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
}
