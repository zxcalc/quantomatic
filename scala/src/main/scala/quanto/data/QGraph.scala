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
  inBBox: BinRel[VName,BBName]    = BinRel[VName,BBName](),
  bboxParent: PFun[BBName,BBName] = PFun[BBName,BBName]())
extends GraphLike[GData,VData,EData,BBData,QGraph]
{
  protected val factory = new QGraph(_,_,_,_,_,_,_,_)
}

object QGraph {
  implicit def qGraphAndNameToQGraph[N <: Name[N]](t: (QGraph, Name[N])) : QGraph = t._1


  def apply(json: Json): QGraph =
    Function.chain[QGraph](Seq(

      (json ?# "wire_vertices").foldLeft(_) { (g,v) =>
        val data = v._2 ?# "data"
        val annotation = v._2 ?# "annotation"
        g.addVertex(v._1, WireV(data, annotation))
      },

      (json ?# "node_vertices").foldLeft(_) { (g,v) =>
        val data = v._2 ?# "data"
        val annotation = v._2 ?# "annotation"
        g.addVertex(v._1, NodeV(data, annotation))
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
}