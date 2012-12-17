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
    case Some(obj: JsonObject) => obj.iterator
    case Some(arr: JsonArray) => arr.map(_.stringValue).iterator zip Iterator.continually(JsonObject())
    case Some(other) => throw new JsonAccessException("Expected: JsonObject or JsonArray", other)
    case None => Iterator.empty
  }

  def apply(json: Json): QGraph =
    Function.chain[QGraph](Seq(

      objectLikeIterator(json get "wire_vertices").foldLeft(_) { (g,v) =>
        val data = v._2.getOrElse("data", JsonObject()).asObject
        val annotation = v._2.getOrElse("annotation", JsonObject()).asObject
        g.addVertex(v._1, WireV(data, annotation))
      },

      objectLikeIterator(json get "node_vertices").foldLeft(_) { (g,v) =>
        val data = v._2.getOrElse("data", JsonObject()).asObject
        val annotation = v._2.getOrElse("annotation", JsonObject()).asObject
        g.addVertex(v._1, NodeV(data, annotation))
      },

      objectLikeIterator(json get "dir_edges").foldLeft(_) { (g,e) =>
        val data = e._2.getOrElse("data", JsonObject()).asObject
        val annotation = e._2.getOrElse("annotation", JsonObject()).asObject
        g.addEdge(e._1, DirEdge(data, annotation), (e._2("src").stringValue, e._2("tgt").stringValue))
      },

      objectLikeIterator(json get "undir_edges").foldLeft(_) { (g,e) =>
        val data = e._2.get("data")
        val annotation = e._2.get("annotation")
        g.addEdge(e._1, UndirEdge(data, annotation), (e._2("src").stringValue, e._2("tgt").stringValue))
      },

      objectLikeIterator(json get "bang_boxes").foldLeft(_) { case (g,(bbName,bbData)) =>
        val contains: JsonArray = bbData.get("contains")
        g.addBBox(bbName, BBData(bbData.get("data"), bbData.get("annotation")))
      }

    ))(QGraph())
}