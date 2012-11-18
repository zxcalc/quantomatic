package quanto.data

import scala.collection.mutable._
import Names._

class Graph[G,V,E,B](val name: String, var data: G = ())
extends HasName
{
  val verts        = Map[String,Vertex[V]]()
  val edges        = Map[String,Edge[E]]()
  val bboxes       = Map[String,BBox[B]]()
  val source       = Map[Edge[E],Vertex[V]]()
  val target       = Map[Edge[E],Vertex[V]]()
  val inEdges      = Map[Vertex[V],Set[Edge[E]]]()
  val outEdges     = Map[Vertex[V],Set[Edge[E]]]()
  val bboxMap      = Map[BBox[B],Set[Vertex[V]]]()
  val bboxParent   = Map[BBox[B],Option[BBox[B]]]()
  val bboxChildren = Map[BBox[B],Set[BBox[B]]]()
  
  def addVertex(v: Vertex[V]): GraphChange[G,V,E,B] = {
    if (verts contains v.name)
      throw DuplicateVertexNameException(v.name)
    
    verts += v.name -> v
    inEdges += v -> Set[Edge[E]]()
    outEdges += v -> Set[Edge[E]]()
    GraphChangeAddVertex(v)
  }
  
  def addVertex(coord: Tuple2[Float,Float]=(0,0), data: V): GraphChange[G,V,E,B] =
    addVertex(new Vertex(verts fresh, coord, data))
  
  def addEdge(e: Edge[E], s: Vertex[V], t: Vertex[V]) : GraphChange[G,V,E,B] = {
    if (edges contains e.name)
      throw DuplicateEdgeNameException(e.name)
    
    edges += e.name -> e
    source += e -> s
    target += e -> t
    inEdges(t) += e
    outEdges(s) += e
    GraphChangeAddEdge(e, s, t)
  }
  
  def addEdge(data: E, s: Vertex[V], t: Vertex[V]) : GraphChange[G,V,E,B] =
    addEdge(new Edge(edges fresh, data), s, t)
  
  def addBBox(bbox: BBox[B], parent: Option[BBox[B]] = None) = {
    if (bboxes contains bbox.name)
      throw DuplicateBBoxNameException(bbox.name)
    
    bboxes += bbox.name -> bbox
    bboxChildren += bbox -> Set[BBox[B]]()
    bboxParent += bbox -> parent
    parent map (bboxChildren(_) += bbox)
    GraphChangeAddBBox(bbox, parent)
  }
  
  def deleteEdge(e: Edge[E]) : GraphChange[G,V,E,B] = {
    val s = source(e)
    val t = target(e)
    source -= e
    target -= e
    outEdges(s) -= e
    inEdges(t) -= e
    edges -= e.name
    GraphChangeDeleteEdge(e, s, t)
  }
  
  def deleteVertex(v: Vertex[V]) : GraphChange[G,V,E,B] = {
    val deleteIn = GraphChangeSequence(
        inEdges(v).toSeq map (deleteEdge _))
    val deleteOut = GraphChangeSequence(
        outEdges(v).toSeq map (deleteEdge _))
    inEdges -= v
    outEdges -= v
    verts -= v.name
    GraphChangeSequence(Seq(
      deleteIn, deleteOut,
      GraphChangeDeleteVertex(v)
    ))
  }
  
  def applyGraphChange(gc: GraphChange[G,V,E,B]) {
    gc match {
      case GraphChangeSequence(seq) =>
        seq foreach (applyGraphChange _)
      case GraphChangeAddVertex(v) => addVertex(v)
      case GraphChangeDeleteVertex(v) => deleteVertex(v)
      case GraphChangeAddEdge(e,s,t) => addEdge(e,s,t)
      case GraphChangeDeleteEdge(e,_,_) => deleteEdge(e)
    }
  }
}