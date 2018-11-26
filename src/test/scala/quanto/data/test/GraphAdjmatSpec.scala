package quanto.data.test

import org.scalatest._
import quanto.cosy.AdjMat
import quanto.data._
import quanto.data.Names._
import quanto.layout.ForceLayout
import quanto.util.json._


class GraphAdjMatSpec extends FlatSpec {
  val rg = Theory.fromFile("red_green")

  behavior of "A graph from adjacency matrix"

  val rdata = Vector(
    NodeV(data = JsonObject("type" -> "X", "value" -> "0"), theory = rg),
    NodeV(data = JsonObject("type" -> "X", "value" -> "pi"), theory = rg),
    NodeV(data = JsonObject("type" -> "X", "value" -> "(1/2) pi"), theory = rg),
    NodeV(data = JsonObject("type" -> "X", "value" -> "(-1/2) pi"), theory = rg)
  )

  val gdata = Vector(
    NodeV(data = JsonObject("type" -> "Z", "value" -> "0"), theory = rg),
    NodeV(data = JsonObject("type" -> "Z", "value" -> "pi"), theory = rg),
    NodeV(data = JsonObject("type" -> "Z", "value" -> "(1/2) pi"), theory = rg),
    NodeV(data = JsonObject("type" -> "Z", "value" -> "(-1/2) pi"), theory = rg)
  )

  it should "be constructible" in {
    var amat = new AdjMat(numRedTypes = 2, numGreenTypes = 2)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(true, false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, true, false))
    amat = amat.addVertex(Vector(false, false, false, true))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, false, false, true, false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, false, false, true, false, true))
    val g = AdjMat.toZXGraph(amat, rdata, gdata)
    println(Graph.toJson(g))

    val g1 = Graph.fromJson(Json.parse(
      """
        |{
        |  "wire_vertices" : ["v0", "v1"],
        |  "node_vertices" : {
        |    "v2" : {"data" : {"type" : "X", "value" : "0" }},
        |    "v3" : {"data" : {"type" : "X", "value" : "pi"}},
        |    "v4" : {"data" : {"type" : "X", "value" : "pi"}},
        |    "v5" : {"data" : {"type" : "Z", "value" : "0"}},
        |    "v6" : {"data" : {"type" : "Z", "value" : "pi"}}
        |  },
        |  "undir_edges" : {
        |    "e0" : {"src" : "v2", "tgt" : "v0"},
        |    "e1" : {"src" : "v3", "tgt" : "v1"},
        |    "e2" : {"src" : "v4", "tgt" : "v3"},
        |    "e3" : {"src" : "v5", "tgt" : "v3"},
        |    "e4" : {"src" : "v6", "tgt" : "v3"},
        |    "e5" : {"src" : "v6", "tgt" : "v5"}
        |  }
        |}
      """.stripMargin), rg)

    assert(g.isBoundary("v0"))
    assert(g.isBoundary("v1"))
    var g2 = g1.copy()
    g1.verts.foreach(vn => g2 = g2.updateVData(vn) { vd => vd.withCoord(0, 0) })
    var g3 = g.copy()
    g.verts.foreach(vn => g3 = g3.updateVData(vn) { vd => vd.withCoord(0, 0) })
    assert(g3 === g2)

    // LAYOUT A GRAPH LIKE THIS:
    // val layoutProc = new ForceLayout
    // val g2 = layoutProc.layout(g)
    //
    // TRANSLATE TO JSON:
    // val json = Graph.toJson(g2, rg)
    //
    // WHICH CAN BE INCLUDED IN A BIGGER FILE (see classes in quanto.util.json), e.g.
    // val bigList = JsonArray(json1, json2, ...)
    //
    // OR OUTPUT AS A STANDALONE GRAPH LIKE THIS:
    // json.writeTo(new java.io.File("/path/to/XXX.qgraph"))
  }
}
