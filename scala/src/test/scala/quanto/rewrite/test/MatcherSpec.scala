package quanto.rewrite.test

import quanto.rewrite._
import quanto.data._
import org.scalatest._
import quanto.data.Theory.ValueType
import quanto.util.json.Json

class MatcherSpec extends FlatSpec {
  val rg = Theory.fromFile("red_green")

  def loadGraph(name: String) =
    Graph.fromJson(Json.parse(new Json.Input(Matcher.getClass.getResourceAsStream(name + ".qgraph"))), rg)

  behavior of "The matcher"

  it should "initialise a match search" in {
    val matches = Matcher.findMatches(Graph(), Graph())
  }

  it should "match an empty graph on itself" in {
    val g = Graph(theory = rg)
    val matches = Matcher.findMatches(g, g)
    assert(matches.size === 1)
  }

  it should "match a graph with one node on itself" in {
    val g = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z", "value": ""}}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g, g)
    assert(matches.size === 1)
  }

  it should "match a graph with two nodes on itself twice" in {
    val g = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z", "value": ""}},
        |    "v1": {"data": {"type": "Z", "value": ""}}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g, g)
    assert(matches.size === 2)
  }

  it should "match a graph with four nodes on itself 4! times" in {
    val g = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z", "value": ""}},
        |    "v1": {"data": {"type": "Z", "value": ""}},
        |    "v2": {"data": {"type": "Z", "value": ""}},
        |    "v3": {"data": {"type": "Z", "value": ""}}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g, g)
    assert(matches.size === 4*3*2)
  }

  it should "match a graph with two edges on itself twice" in {
    val g = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z", "value": ""}},
        |    "v1": {"data": {"type": "X", "value": ""}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "v1"},
        |    "e1": {"src": "v0", "tgt": "v1"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g, g)
    assert(matches.size === 2)
  }

  it should "match a graph with two edges and three nodes on itself twice" in {
    val g = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z", "value": ""}},
        |    "v1": {"data": {"type": "X", "value": ""}},
        |    "v2": {"data": {"type": "Z", "value": ""}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "v1"},
        |    "e1": {"src": "v1", "tgt": "v2"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g, g)
    assert(matches.size === 2)
  }

  it should "match a graph with 7 edges and 3 nodes on itself 3! * 2! times" in {
    val g = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["b0", "b1"],
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z", "value": ""}},
        |    "v1": {"data": {"type": "X", "value": ""}},
        |    "v2": {"data": {"type": "Z", "value": ""}}
        |  },
        |  "undir_edges": {
        |    "e0": {"src": "b0", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "v1"},
        |    "e2": {"src": "v0", "tgt": "v1"},
        |    "e3": {"src": "v1", "tgt": "v2"},
        |    "e4": {"src": "v1", "tgt": "v2"},
        |    "e5": {"src": "v1", "tgt": "v2"},
        |    "e6": {"src": "v2", "tgt": "b1"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g, g)
    assert(matches.size === 12)
  }

  it should "not match if the type is wrong" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z", "value": ""}}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "X", "value": ""}}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g2)
    assert(matches.size === 0)
  }

  it should "match a node with an angle" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z", "value": "(1/2) \\pi"}}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z", "value": "(1/2) \\pi"}}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g2)
    assert(matches.size === 1)
  }

  it should "match a node with a free variable" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z", "value": "x"}}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z", "value": "(1/2) \\pi"}}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g2)
    assert(matches.size === 1)
    assert(matches.head.subst(ValueType.AngleExpr).mapValues(_.as(ValueType.AngleExpr)) ===
      Map("x" -> PhaseExpression.parse("(1/2) \\pi", ValueType.AngleExpr)))
  }

  it should "match a graph with one wire on itself" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "X"}}
        |  },
        |  "wire_vertices": ["w0"],
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "w0"},
        |    "e1": {"src": "w0", "tgt": "v1"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g1)
    assert(matches.size === 1)
    assert(matches.forall { _.isTotal })
    assert(matches.forall { _.isHomomorphism })
  }

  it should "match a graph with one wire on itself twice" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "Z"}}
        |  },
        |  "wire_vertices": ["w0"],
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "w0"},
        |    "e1": {"src": "w0", "tgt": "v1"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g1)
    assert(matches.size === 2)

    assert(matches.forall { _.isTotal })
    assert(matches.forall { _.isHomomorphism })
  }

  it should "match a graph with one directed wire on itself" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "Z"}}
        |  },
        |  "wire_vertices": ["w0"],
        |  "dir_edges": {
        |    "e0": {"src": "v0", "tgt": "w0"},
        |    "e1": {"src": "w0", "tgt": "v1"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g1)
    assert(matches.size === 1)

    assert(matches.forall { _.isTotal })
    assert(matches.forall { _.isHomomorphism })
  }

  it should "refuse to match directed wires on undirected, and vice-versa" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "Z"}}
        |  },
        |  "wire_vertices": ["w0"],
        |  "dir_edges": {
        |    "e0": {"src": "v0", "tgt": "w0"},
        |    "e1": {"src": "w0", "tgt": "v1"}
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
        |  "wire_vertices": ["w0"],
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "w0"},
        |    "e1": {"src": "w0", "tgt": "v1"}
        |  }
        |}
      """.stripMargin), thy = rg)
    assert(Matcher.findMatches(g1, g2).size === 0)
    assert(Matcher.findMatches(g2, g1).size === 0)
  }

  it should "match a graph with two wires on itself" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "X"}},
        |    "v2": {"data": {"type": "Z"}}
        |  },
        |  "wire_vertices": ["w0", "w1"],
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "w0"},
        |    "e1": {"src": "w0", "tgt": "v1"},
        |    "e2": {"src": "v1", "tgt": "w1"},
        |    "e3": {"src": "w1", "tgt": "v2"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g1)
    assert(matches.size === 2)
    assert(matches.forall { _.isTotal })
    assert(matches.forall { _.isHomomorphism })
  }

  it should "match a graph with two directed wires on itself" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "X"}},
        |    "v2": {"data": {"type": "Z"}}
        |  },
        |  "wire_vertices": ["w0", "w1"],
        |  "dir_edges": {
        |    "e0": {"src": "v0", "tgt": "w0"},
        |    "e1": {"src": "w0", "tgt": "v1"},
        |    "e2": {"src": "v1", "tgt": "w1"},
        |    "e3": {"src": "w1", "tgt": "v2"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g1)
    assert(matches.size === 1)
    assert(matches.forall { _.isTotal })
    assert(matches.forall { _.isHomomorphism })
  }

  it should "match a node in a triangle" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}}
        |  },
        |  "wire_vertices": ["w0", "w1", "w2"],
        |  "undir_edges": {
        |    "e0": {"src": "w0", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "w1"},
        |    "e2": {"src": "v0", "tgt": "w2"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "X"}},
        |    "v2": {"data": {"type": "X"}},
        |    "v3": {"data": {"type": "X"}}
        |  },
        |  "wire_vertices": ["w0", "w1", "w2"],
        |  "undir_edges": {
        |    "e0": {"src": "w0", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "w1"},
        |    "e2": {"src": "v0", "tgt": "w2"},
        |    "e3": {"src": "v1", "tgt": "w0"},
        |    "e4": {"src": "w1", "tgt": "v2"},
        |    "e5": {"src": "w2", "tgt": "v3"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g2)
    assert(matches.size === 6)
    assert(matches.forall { _.isTotal })
    assert(matches.forall { _.isHomomorphism })
  }

  it should "match a node in a directed triangle" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}}
        |  },
        |  "wire_vertices": ["w0", "w1", "w2"],
        |  "dir_edges": {
        |    "e0": {"src": "w0", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "w1"},
        |    "e2": {"src": "v0", "tgt": "w2"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "X"}},
        |    "v2": {"data": {"type": "X"}},
        |    "v3": {"data": {"type": "X"}}
        |  },
        |  "wire_vertices": ["w0", "w1", "w2"],
        |  "dir_edges": {
        |    "e0": {"src": "w0", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "w1"},
        |    "e2": {"src": "v0", "tgt": "w2"},
        |    "e3": {"src": "v1", "tgt": "w0"},
        |    "e4": {"src": "w1", "tgt": "v2"},
        |    "e5": {"src": "w2", "tgt": "v3"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g2)

    assert(matches.size === 2)
    assert(matches.forall { _.isTotal })
    assert(matches.forall { _.isHomomorphism })
  }

  it should "pull matches lazily" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}}
        |  },
        |  "wire_vertices": ["w0", "w1", "w2"],
        |  "dir_edges": {
        |    "e0": {"src": "w0", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "w1"},
        |    "e2": {"src": "v0", "tgt": "w2"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "X"}},
        |    "v2": {"data": {"type": "X"}},
        |    "v3": {"data": {"type": "X"}}
        |  },
        |  "wire_vertices": ["w0", "w1", "w2"],
        |  "dir_edges": {
        |    "e0": {"src": "w0", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "w1"},
        |    "e2": {"src": "v0", "tgt": "w2"},
        |    "e3": {"src": "v1", "tgt": "w0"},
        |    "e4": {"src": "w1", "tgt": "v2"},
        |    "e5": {"src": "w2", "tgt": "v3"}
        |  }
        |}
      """.stripMargin), thy = rg)

    MatchState.startCountingMatches()
    val matches = Matcher.findMatches(g1, g2)
    matches.head
    matches.tail.head
    val c = MatchState.matchCount()
    assert(c === 2)
  }

  it should "match a graph with circles" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "X"}}
        |  },
        |  "wire_vertices": ["w0", "w1", "w2"],
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "w0"},
        |    "e1": {"src": "w0", "tgt": "v1"},
        |    "e2": {"src": "w1", "tgt": "w1"},
        |    "e3": {"src": "w2", "tgt": "w2"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g1)
    assert(matches.size === 1)
    assert(matches.forall { _.isTotal })
    assert(matches.forall { _.isHomomorphism })
  }

  it should "not match a graph with too many circles" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "X"}}
        |  },
        |  "wire_vertices": ["w0", "w1", "w2"],
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "w0"},
        |    "e1": {"src": "w0", "tgt": "v1"},
        |    "e2": {"src": "w1", "tgt": "w1"},
        |    "e3": {"src": "w2", "tgt": "w2"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "X"}}
        |  },
        |  "wire_vertices": ["w0", "w1"],
        |  "undir_edges": {
        |    "e0": {"src": "v0", "tgt": "w0"},
        |    "e1": {"src": "w0", "tgt": "v1"},
        |    "e2": {"src": "w1", "tgt": "w1"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g2)
    assert(matches.size === 0)
  }

  it should "match non-injectively on the boundary" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}}
        |  },
        |  "wire_vertices": ["w0", "w1", "w2"],
        |  "undir_edges": {
        |    "e0": {"src": "w0", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "w1"},
        |    "e2": {"src": "v0", "tgt": "w2"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}}
        |  },
        |  "wire_vertices": ["w0", "w1"],
        |  "undir_edges": {
        |    "e0": {"src": "w0", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "w1"},
        |    "e2": {"src": "v0", "tgt": "w1"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g2)
    assert(matches.size === 6)
    assert(matches.forall { _.isTotal })
    assert(matches.forall { _.isHomomorphism })
  }

  it should "match a bare wire on itself" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["w0", "w1"],
        |  "undir_edges": {
        |    "e0": {"src": "w0", "tgt": "w1"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g1)
    assert(matches.size === 1)
    assert(matches.forall { _.isTotal })
    assert(matches.forall { _.isHomomorphism })
  }

  it should "match a bare wire in 4 places" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["w0", "w1"],
        |  "undir_edges": {
        |    "e0": {"src": "w0", "tgt": "w1"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": {
        |    "v0": {"data": {"type": "Z"}},
        |    "v1": {"data": {"type": "X"}}
        |  },
        |  "wire_vertices": ["w0", "w1", "w2", "w3", "w4"],
        |  "undir_edges": {
        |    "e0": {"src": "w0", "tgt": "v0"},
        |    "e1": {"src": "v0", "tgt": "w1"},
        |    "e2": {"src": "w1", "tgt": "v1"},
        |    "e3": {"src": "v1", "tgt": "w2"},
        |    "e4": {"src": "w3", "tgt": "w4"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g2)
    assert(matches.size === 4)
    assert(matches.forall { _.isTotal })
    assert(matches.forall { _.isHomomorphism })
  }

  it should "match 3 bare wires in 6 different ways" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["w0", "w1", "w2", "w3", "w4", "w5"],
        |  "undir_edges": {
        |    "e0": {"src": "w0", "tgt": "w1"},
        |    "e1": {"src": "w2", "tgt": "w3"},
        |    "e2": {"src": "w4", "tgt": "w5"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val g2 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices": ["w0", "w1"],
        |  "undir_edges": {
        |    "e0": {"src": "w0", "tgt": "w1"}
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g2)
    assert(matches.size === 6)
    assert(matches.forall { _.isTotal })
    assert(matches.forall { _.isHomomorphism })
  }

  it should "match a graph with 2 components on itself" in {
    val g1 = Graph.fromJson(Json.parse(
    """
      |{
      |  "wire_vertices": ["b0", "w0"],
      |  "node_vertices": {
      |    "v0": {"data": {"type": "X"}},
      |    "v1": {"data": {"type": "Z"}},
      |    "v2": {"data": {"type": "X"}}
      |  },
      |  "undir_edges": {
      |    "e0": {"src": "b0", "tgt": "v0"},
      |    "e6": {"src": "v0", "tgt": "w0"},
      |    "e7": {"src": "w0", "tgt": "v1"}
      |  }
      |}
    """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g1)
    assert(matches.nonEmpty)
  }

  it should "match a bialg LHS on itself" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "bang_boxes" : {
        |    "bx0" : {
        |      "contents" : [ "b0", "v1" ]
        |    },
        |    "bx1" : {
        |      "contents" : [ "b1", "v0" ]
        |    }
        |  },
        |  "wire_vertices" : {
        |    "b0" : {
        |      "annotation" : {
        |        "boundary" : true,
        |        "coord" : [ -1.5, 4.25 ]
        |      }
        |    },
        |    "b1" : {
        |      "annotation" : {
        |        "boundary" : true,
        |        "coord" : [ -1.5, -1.75 ]
        |      }
        |    }
        |  },
        |  "node_vertices" : {
        |    "v0" : {
        |      "data" : {
        |        "type" : "X",
        |        "value" : ""
        |      },
        |      "annotation" : {
        |        "coord" : [ -1.5, 0.25 ]
        |      }
        |    },
        |    "v1" : {
        |      "annotation" : {
        |        "coord" : [ -1.5, 2.5 ]
        |      }
        |    }
        |  },
        |  "undir_edges" : {
        |    "e0" : {
        |      "src" : "b0",
        |      "tgt" : "v1"
        |    },
        |    "e1" : {
        |      "src" : "v1",
        |      "tgt" : "v0"
        |    },
        |    "e2" : {
        |      "src" : "v0",
        |      "tgt" : "b1"
        |    }
        |  }
        |}
      """.stripMargin), thy = rg)
    val matches = Matcher.findMatches(g1, g1)
    assert(matches.nonEmpty)
  }
}
