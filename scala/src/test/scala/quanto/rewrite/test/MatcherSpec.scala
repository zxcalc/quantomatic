package quanto.rewrite.test

import quanto.rewrite._
import quanto.data._
import org.scalatest._
import quanto.util.json.Json

class MatcherSpec extends FlatSpec {
  val rg = Theory.fromFile("red_green")

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
    assert(matches.head.subst === Map("x" -> AngleExpression.parse("(1/2) \\pi")))
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
}
