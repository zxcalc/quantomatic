package quanto.data

import Names._
import quanto.util.json._
import math.sqrt
import JsonValues._

class QGraphJsonException(message: String, cause: Throwable = null)
extends Exception(message, cause)

case class QGraph(
  data: GData                     = GData(),
  vdata: Map[VName,VData]         = Map[VName,VData](),
  edata: Map[EName,EData]         = Map[EName,EData](),
  source: PFun[EName,VName]       = PFun[EName,VName](),
  target: PFun[EName,VName]       = PFun[EName,VName](),
  bbdata: Map[BBName,BBData]      = Map[BBName,BBData](),
  inBBox: BinRel[VName,BBName]    = BinRel[VName,BBName](),
  bboxParent: PFun[BBName,BBName] = PFun[BBName,BBName]())
extends GraphLike[GData,VData,EData,BBData,QGraph]
{
  protected val factory = new QGraph(_,_,_,_,_,_,_,_)
}

object QGraph {
  implicit def qGraphAndNameToQGraph[N <: Name[N]](t: (QGraph, Name[N])) : QGraph = t._1

  def fromJson(s: String, thy: Theory): QGraph = fromJson(Json.parse(s), thy)
  def fromJson(s: String): QGraph = fromJson(Json.parse(s), Theory.defaultTheory)

  def fromJson(json: Json, thy: Theory = Theory.defaultTheory): QGraph =
    Function.chain[QGraph](Seq(

      (json ?# "wire_vertices").foldLeft(_) { (g,v) =>
        g.addVertex(v._1, WireV.fromJson(v._2, thy))
      },

      (json ?# "node_vertices").foldLeft(_) { (g,v) =>
        g.addVertex(v._1, NodeV.fromJson(v._2, thy))
      },

      (json ?# "dir_edges").foldLeft(_) { (g,e) =>
        val data = e._2 ?# "data"
        val annotation = e._2 ?# "annotation"
        g.addEdge(e._1, DirEdge(data, annotation),
          (e._2("src").stringValue, e._2("tgt").stringValue))
      },

      (json ?# "undir_edges").foldLeft(_) { (g,e) =>
        val data = e._2 ?# "data"
        val annotation = e._2 ?# "annotation"
        g.addEdge(e._1, UndirEdge(data, annotation), (e._2("src").stringValue, e._2("tgt").stringValue))
      },

      (json ?# "bang_boxes").foldLeft(_) { (g,bb) =>
        val data = bb._2 ?# "data"
        val annotation = bb._2 ?# "annotation"
        val contains = (bb._2 ?@ "contains") map { VName(_) }
        val parent = bb._2.get("parent") map { BBName(_) }
        g.addBBox(bb._1, BBData(data, annotation), contains.toSet, parent)
      }

    ))({
      val data = json ?# "data"
      val annotation = json ?# "annotation"
      QGraph(GData(data, annotation))
    })

  def toJson(graph: QGraph, thy: Theory = Theory.defaultTheory): Json = {
    val (wireVertices, nodeVertices) = graph.vdata.foldLeft((JsonObject(), JsonObject()))
    { case ((objW,objN), (v,d)) =>
      val entry = v.toString -> d.json
      if (d.isWireVertex) (objW + entry, objN) else (objW, objN + entry)
    }

    val (dirEdges, undirEdges) = graph.edata.foldLeft((JsonObject(), JsonObject()))
    { case ((objD,objU), (e,d)) =>
      val entry = e.toString -> (d.json + ("src" -> graph.source(e).toString, "tgt" -> graph.target(e).toString))
      if (d.isDirected) (objD + entry, objU) else (objD, objU + entry)
    }

    val bangBoxes = graph.bbdata.foldLeft(JsonObject()) { case (obj, (bb, d)) =>
      obj + (bb.toString ->
        JsonObject(
          "contains"   -> JsonArray(graph.contents(bb)),
          "parent"     -> (graph.bboxParent.get(bb) match {
                             case Some(p) => JsonString(p.toString)
                             case None    => JsonNull() }),
          "data"       -> d.data,
          "annotation" -> d.annotation
        ).noEmpty)
    }

    JsonObject(
      "wire_vertices" -> wireVertices.asObjectOrKeyArray,
      "node_vertices" -> nodeVertices.asObjectOrKeyArray,
      "dir_edges"     -> dirEdges,
      "undir_edges"   -> undirEdges,
      "bang_boxes"    -> bangBoxes,
      "data"          -> graph.data.data,
      "annotation"    -> graph.data.annotation
    ).noEmpty
  }

  def random(nverts: Int, nedges: Int, nbboxes: Int = 0) = {
    val rand = new util.Random
    var randomGraph = QGraph()
    for (i <- 1 to nverts) {
      val p = (rand.nextDouble * 6.0 - 3.0, rand.nextDouble * 6.0 - 3.0)
      randomGraph = randomGraph.newVertex(NodeV(p))
    }

    if (nverts != 0) {
      val varray = randomGraph.vdata.keys.toArray
      for(j <- 1 to nedges) {
        val s = varray(rand.nextInt(varray.size))
        val t = varray(rand.nextInt(varray.size))
        randomGraph = randomGraph.newEdge(DirEdge(), (s,t))
      }

      for (i <- 1 to nbboxes) {
        val randomVSet = (1 to (sqrt(nverts).toInt)).foldLeft(Set[VName]()) { (s,_) =>
          s + varray(rand.nextInt(varray.size))
        }

        randomGraph = randomGraph.newBBox(BBData(), randomVSet, None)
      }
    }

    randomGraph
  }

  def randomDag(nverts: Int, nedges: Int) = {
    val rand = new util.Random
    var randomGraph = QGraph()
    for (i <- 1 to nverts) {
      val p = (rand.nextDouble * 6.0 - 3.0, rand.nextDouble * 6.0 - 3.0)
      randomGraph = randomGraph.newVertex(NodeV(p))
    }
    val varray = randomGraph.vdata.keys.toArray

    // must have at least two verts to add edges since no self-loops allowed
    if (nverts > 1)
      for(j <- 1 to nedges) {
        val x = rand.nextInt(varray.size)
        val y = rand.nextInt(varray.size - 1)
        val s = varray(x)
        val t = varray(if (y >= x) y+1 else y)
        randomGraph = randomGraph.newEdge(DirEdge(),
          if (s <= t) (s,t) else (t,s)
        )
      }

    randomGraph
  }
}