package quanto.data

import Names._
import quanto.util.json._
import math.{max,min,sqrt}
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

  def random(nverts: Int, nedges: Int) = {
    val rand = new util.Random
    var randomGraph = QGraph()
    for (i <- 1 to nverts) {
      val p = (rand.nextDouble * 6.0 - 3.0, rand.nextDouble * 6.0 - 3.0)
      randomGraph = randomGraph.newVertex(NodeV(p))
    }
    val varray = randomGraph.vdata.keys.toArray
    for(j <- 1 to nedges) {
      val s = varray(rand.nextInt(varray.size))
      val t = varray(rand.nextInt(varray.size))
      randomGraph = randomGraph.newEdge(DirEdge(), (s,t))
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