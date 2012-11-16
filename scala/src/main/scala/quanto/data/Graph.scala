package quanto.data

import scala.collection.mutable._

class Graph[V,E]
{
  val verts = Map[String,Vertex[V]]()
  val edges = Map[String,Edge[E]]()
  val source = Map[Edge[E],Vertex[V]]()
  val target = Map[Edge[E],Vertex[V]]()
  val inEdges = Map[Vertex[V], Set[Edge[E]]]()
  val outEdges = Map[Vertex[V], Set[Edge[E]]]()
  
  def addVertex(v: Vertex[V]): GraphChange[V,E] = {
    if (verts contains v.name)
      throw DuplicateVertexNameException(v.name)
    
    verts += v.name -> v
    inEdges += v -> Set[Edge[E]]()
    outEdges += v -> Set[Edge[E]]()
    GraphChangeAddVertex(v)
  }
  
  def addEdge(e: Edge[E], s: Vertex[V], t: Vertex[V]) : GraphChange[V,E] = {
    if (edges contains e.name)
      throw DuplicateEdgeNameException(e.name)
    
    edges += e.name -> e
    source += e -> s
    target += e -> t
    inEdges(t) += e
    outEdges(s) += e
    GraphChangeAddEdge(e, s, t)
  }
  
  def deleteEdge(e: Edge[E]) : GraphChange[V,E] = {
    val s = source(e)
    val t = target(e)
    source -= e
    target -= e
    outEdges(s) -= e
    inEdges(t) -= e
    edges -= e.name
    GraphChangeDeleteEdge(e, s, t)
  }
  
  def deleteVertex(v: Vertex[V]) : GraphChange[V,E] = {
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
  
  def applyGraphChange(gc: GraphChange[V,E]) {
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