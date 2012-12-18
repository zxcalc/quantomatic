package quanto.data

import Names._
import quanto.util.json._
import JsonValues._

class QGraphJsonException(message: String, cause: Throwable = null)
extends Exception(message, cause)

case class QGraph(
  data: GData                     = GData(),
  verts: Map[VName,VData]         = Map[VName,VData](),
  edges: Map[EName,EData]         = Map[EName,EData](),
  source: PFun[EName,VName]       = PFun[EName,VName](),
  target: PFun[EName,VName]       = PFun[EName,VName](),
  bboxes: Map[BBName,BBData]      = Map[BBName,BBData](),
  inBBox: PFun[VName,BBName]      = PFun[VName,BBName](),
  bboxParent: PFun[BBName,BBName] = PFun[BBName,BBName]())
extends GraphLike[GData,VData,EData,BBData,QGraph]
{
  protected val factory = new QGraph(_,_,_,_,_,_,_,_)
}

object QGraph {
  implicit def qGraphAndNameToQGraph[N <: Name[N]](t: (QGraph, Name[N])) : QGraph = t._1

  // quietly treat lists like {"a":{}, "b":{}, ...} and None like {}
  private def objectLikeIterator(json: Option[Json]): Iterator[(String,Json)] = json match {
    case Some(JsonObject(x)) => x.iterator
    case Some(JsonArray(x)) => x.map(_.stringValue).iterator zip Iterator.continually(JsonObject())
    case Some(other) => throw new JsonAccessException("Expected: JsonObject or JsonArray", other)
    case None => Iterator.empty
  }

  def apply(json: Json): QGraph =
    Function.chain[QGraph](Seq(

      objectLikeIterator(json get "wire_vertices").foldLeft(_) { (g,v) =>
        val data = v._2.getOptObject("data")
        val annotation = v._2.getOptObject("annotation")
        g.addVertex(v._1, WireV(data, annotation))
      },

      objectLikeIterator(json get "node_vertices").foldLeft(_) { (g,v) =>
        val data = v._2.getOptObject("data")
        val annotation = v._2.getOptObject("annotation")
        g.addVertex(v._1, NodeV(data, annotation))
      },

      objectLikeIterator(json get "dir_edges").foldLeft(_) { (g,e) =>
        val data = e._2.getOptObject("data")
        val annotation = e._2.getOptObject("annotation")
        g.addEdge(e._1, DirEdge(data, annotation),
          (e._2("src").stringValue, e._2("tgt").stringValue))
      },

      objectLikeIterator(json get "undir_edges").foldLeft(_) { (g,e) =>
        val data = e._2.getOptObject("data")
        val annotation = e._2.getOptObject("annotation")
        g.addEdge(e._1, UndirEdge(data, annotation), (e._2("src").stringValue, e._2("tgt").stringValue))
      },

      objectLikeIterator(json get "bang_boxes").foldLeft(_) { (g,bb) =>
        val data = bb._2.getOptObject("data")
        val annotation = bb._2.getOptObject("annotation")
        val contents = bb._2.getOptArray("contains") map { VName(_) }
        val parent = bb._2.get("parent") map { BBName(_) }
        g.addBBox(bb._1, BBData(data, annotation), contents.toSet, parent)
      }

    ))({
      val data = json.getOptObject("data")
      val annotation = json.getOptObject("annotation")
      QGraph(GData(data, annotation))
    })
}