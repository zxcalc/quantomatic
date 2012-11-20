package quanto.data

import Names._

class Graph[G,V,E,B](val name: String, var data: G = ())
extends HasName
{
  var verts        = Map[String,Vertex[V]]()
  var edges        = Map[String,Edge[E]]()
  var bboxes       = Map[String,BBox[B]]()
  var source       = PFun[Edge[E],Vertex[V]]()
  var target       = PFun[Edge[E],Vertex[V]]()
  var bbox         = PFun[Vertex[V],BBox[B]]()
  var bboxParent   = PFun[BBox[B],BBox[B]]()

  var vertexCoords = Map[Vertex[V],(Float,Float)]()
  
  def addVertex(v: Vertex[V]): GraphChange[G,V,E,B] = {
    if (verts contains v.name)
      throw DuplicateVertexNameException(v.name)
    
    verts += v.name -> v
    vertexCoords += v -> (0.0f,0.0f)
    GraphChangeAddVertex(v)
  }

  def addVertex() : GraphChange[G,V,E,B] =
    addVertex(Vertex[V](verts fresh "v0"))

  def addVertex(data: V): GraphChange[G,V,E,B] =
    addVertex(Vertex[V](verts fresh "v0", data))

  def moveVertex(v : Vertex[V], coord: (Float,Float)) : GraphChange[G,V,E,B] = {
    val oldCoord = vertexCoords(v)
    vertexCoords += v -> coord
    GraphChangeMoveVertex(v, oldCoord, coord)
  }

  def addEdge(e: Edge[E], s: Vertex[V], t: Vertex[V]) : GraphChange[G,V,E,B] = {
    if (edges contains e.name)
      throw DuplicateEdgeNameException(e.name)
    
    edges += e.name -> e
    source += e -> s
    target += e -> t
    GraphChangeAddEdge(e, s, t)
  }

  def addEdge(s: Vertex[V], t: Vertex[V]) : GraphChange[G,V,E,B] =
    addEdge(Edge[E](edges fresh "e0"), s, t)

  def addEdge(s: Vertex[V], t: Vertex[V], data: E) : GraphChange[G,V,E,B] =
    addEdge(Edge(edges fresh "e0", data), s, t)
  
  def addBBox(bbox: BBox[B], parent: Option[BBox[B]] = None) = {
    if (bboxes contains bbox.name)
      throw DuplicateBBoxNameException(bbox.name)
    
    bboxes += bbox.name -> bbox
    parent map (bboxParent += bbox -> _)
    GraphChangeAddBBox(bbox, parent)
  }
  
  def deleteEdge(e: Edge[E]) : GraphChange[G,V,E,B] = {
    val s = source(e)
    val t = target(e)
    source -= e
    target -= e
    edges -= e.name
    GraphChangeDeleteEdge(e, s, t)
  }
  
  def deleteVertex(v: Vertex[V]) : GraphChange[G,V,E,B] = {
    val deleteIn = GraphChangeSequence(
        target.inv(v).toSeq map (deleteEdge _))
    val deleteOut = GraphChangeSequence(
        source.inv(v).toSeq map (deleteEdge _))
    val coord = vertexCoords(v)
    verts -= v.name
    vertexCoords -= v

    GraphChangeSequence(Seq(
      GraphChangeMoveVertex(v, coord, (0.0f,0.0f)),
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