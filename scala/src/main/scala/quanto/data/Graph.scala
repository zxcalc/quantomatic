package quanto.data

import Names._
import quanto.util.json._
import math.sqrt
import JsonValues._

class GraphLoadException(message: String, cause: Throwable = null)
  extends Exception(message, cause)


case class Graph(
                   data: GData                     = GData(),
                   vdata: Map[VName,VData]         = Map[VName,VData](),
                   edata: Map[EName,EData]         = Map[EName,EData](),
                   source: PFun[EName,VName]       = PFun[EName,VName](),
                   target: PFun[EName,VName]       = PFun[EName,VName](),
                   bbdata: Map[BBName,BBData]      = Map[BBName,BBData](),
                   inBBox: BinRel[VName,BBName]    = BinRel[VName,BBName](),
                   bboxParent: PFun[BBName,BBName] = PFun[BBName,BBName]())
  extends GraphLike[GData,VData,EData,BBData,Graph]
{
  protected val factory = new Graph(_,_,_,_,_,_,_,_)
}

object Graph {
  implicit def qGraphAndNameToQGraph[N <: Name[N]](t: (Graph, Name[N])) : Graph = t._1

  def apply(theory: Theory): Graph = Graph(data = GData(theory = theory))

  def fromJson(s: String, thy: Theory): Graph =
    try   { fromJson(Json.parse(s), thy) }
    catch { case e:JsonParseException => throw new GraphLoadException("Error parsing JSON", e) }


  def fromJson(s: String): Graph = fromJson(s, Theory.DefaultTheory)

  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory): Graph = try {
    Function.chain[Graph](Seq(

      (json ?# "wire_vertices").foldLeft(_) { (g,v) =>
        g.addVertex(v._1, WireV.fromJson(v._2, thy))
      },

      (json ?# "node_vertices").foldLeft(_) { (g,v) =>
        g.addVertex(v._1, NodeV.fromJson(v._2, thy))
      },

      (json ?# "dir_edges").foldLeft(_) { (g,e) =>
        val data = e._2.getOrElse("data", thy.defaultEdgeData).asObject
        val annotation = e._2 ?# "annotation"
        g.addEdge(e._1, DirEdge(data, annotation, thy),
          (e._2("src").stringValue, e._2("tgt").stringValue))
      },

      (json ?# "undir_edges").foldLeft(_) { (g,e) =>
        val data = e._2.getOrElse("data", thy.defaultEdgeData).asObject
        val annotation = e._2 ?# "annotation"
        g.addEdge(e._1, UndirEdge(data, annotation, thy), (e._2("src").stringValue, e._2("tgt").stringValue))
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
      Graph(GData(data, annotation, thy))
    })
  } catch {
    case e: JsonAccessException => throw new GraphLoadException("Error reading JSON", e)
  }

  def toJson(graph: Graph, thy: Theory = Theory.DefaultTheory): Json = {
    val (wireVertices, nodeVertices) = graph.vdata.foldLeft((JsonObject(), JsonObject()))
    {
      case ((objW,objN), (v,w: WireV)) => (objW + (v.toString -> w.toJson), objN)
      case ((objW,objN), (v,n: NodeV)) => (objW, objN + (v.toString -> n.toJson))
    }

    val (dirEdges, undirEdges) = graph.edata.foldLeft((JsonObject(), JsonObject()))
    { case ((objD,objU), (e,d)) =>
      val entry = e.toString -> (d.toJson + ("src" -> graph.source(e).toString, "tgt" -> graph.target(e).toString))
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
    var randomGraph = Graph()
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
    var randomGraph = Graph()
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