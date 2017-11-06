package quanto.data.test

import org.scalatest._
import quanto.data._
import quanto.util.json._
import quanto.data.Names._

class GraphBBoxSpec extends FlatSpec {
  behavior of "A graph with bboxes"

  it should "expand a bbox with one node" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": ["v0"],
        |  "bang_boxes": {
        |    "bb0": { "contents": ["v0"] }
        |  }
        |}
      """.stripMargin))
    val (g2,bbe) = g1.expandBBox("bb0")
    assert(g1.verts.size === 1)
    assert(g1.bboxes.size === 1)

    assert(bbe.bb === BBName("bb0"))

    assert(g2.verts.size === 2)
    assert(g2.verts === Set[VName]("v0", bbe.mp.v("v0")))
    assert(g2.bboxes.size === 1)
  }

  it should "expand a bbox with an edge into a node" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": ["v0", "v1"],
        |  "undir_edges": {
        |    "e0": { "src": "v0", "tgt": "v1" }
        |  },
        |  "bang_boxes": {
        |    "bb0": { "contents": ["v0"] }
        |  }
        |}
      """.stripMargin))
    val (g2,bbe) = g1.expandBBox("bb0")
    assert(g1.verts.size === 2)
    assert(g1.bboxes.size === 1)

    assert(bbe.bb === BBName("bb0"))

    assert(g2.verts.size === 3)
    assert(g2.verts === Set[VName]("v0", "v1", bbe.mp.v("v0")))
    assert(g2.vdata("v0").data === g2.vdata(bbe.mp.v("v0")).data)
    assert(g2.edges.size === 2)
    assert(g2.edges === Set[EName]("e0", bbe.mp.e("e0")))
    assert(g2.bboxes.size === 1)
  }

  it should "copy a bbox" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": ["v0", "v1"],
        |  "undir_edges": {
        |    "e0": { "src": "v0", "tgt": "v1" }
        |  },
        |  "bang_boxes": {
        |    "bb0": { "contents": ["v0"] }
        |  }
        |}
      """.stripMargin))
    val (g2,bbc) = g1.copyBBox("bb0")
    assert(g1.verts.size === 2)
    assert(g1.bboxes.size === 1)

    assert(bbc.bb === BBName("bb0"))

    assert(g2.verts.size === 3)
    assert(g2.verts === Set[VName]("v0", "v1", bbc.mp.v("v0")))
    assert(g2.edges.size === 2)
    assert(g2.edges === Set[EName]("e0", bbc.mp.e("e0")))
    assert(g2.bboxes.size === 2)
    assert(g2.bboxes === Set[BBName]("bb0", bbc.mp.bb("bb0")))
  }

  it should "drop a bbox" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": ["v0", "v1"],
        |  "undir_edges": {
        |    "e0": { "src": "v0", "tgt": "v1" }
        |  },
        |  "bang_boxes": {
        |    "bb0": { "contents": ["v0"] }
        |  }
        |}
      """.stripMargin))
    val (g2,bbd) = g1.dropBBox("bb0")
    assert(g1.verts.size === 2)
    assert(g1.edges.size === 1)
    assert(g1.bboxes.size === 1)

    assert(bbd.bb === BBName("bb0"))

    assert(g2.verts.size === 2)
    assert(g2.edges.size === 1)
    assert(g2.bboxes.size === 0)
  }

  it should "kill a bbox" in {
    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "node_vertices": ["v0", "v1"],
        |  "undir_edges": {
        |    "e0": { "src": "v0", "tgt": "v1" }
        |  },
        |  "bang_boxes": {
        |    "bb0": { "contents": ["v0"] }
        |  }
        |}
      """.stripMargin))
    val (g2,bbk) = g1.killBBox("bb0")
    assert(g1.verts.size === 2)
    assert(g1.edges.size === 1)
    assert(g1.bboxes.size === 1)

    assert(bbk.bb === BBName("bb0"))

    assert(g2.verts.size === 1)
    assert(g2.edges.size === 0)
    assert(g2.bboxes.size === 0)
  }
}
