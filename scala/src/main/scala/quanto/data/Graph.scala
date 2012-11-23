package quanto.data

import Names._



class Graph[G,V,E,B](val name: String, var data: G = ())
{
  var verts        = Map[VName,Vertex[V]]()
  var edges        = Map[EName,Edge[E]]()
  var bboxes       = Map[BBName,BBox[B]]()
  var source       = PFun[EName,VName]()
  var target       = PFun[EName,VName]()
  var bbox         = PFun[VName,BBName]()
  var bboxParent   = PFun[BBName,BBName]()

  var vertexCoords = Map[VName,(Float,Float)]()

  def addVertex(v: Vertex[V]): (VName,GraphChange[G,V,E,B]) = {
    if (verts contains v.name)
      throw DuplicateVertexNameException(v.name)
    
    verts += v.name -> v
    vertexCoords += v.name -> (0.0f,0.0f)
    (v.name, GraphChangeAddVertex(v))
  }

  def addVertex(data: V): (VName, GraphChange[G,V,E,B]) =
    addVertex(Vertex[V](verts fresh, data))

  def moveVertex(vn : VName, coord: (Float,Float)) : GraphChange[G,V,E,B] = {
    val oldCoord = vertexCoords(vn)
    vertexCoords += vn -> coord
    GraphChangeMoveVertex(vn, oldCoord, coord)
  }

  def addEdge(e: Edge[E], sn: VName, tn: VName) : (EName, GraphChange[G,V,E,B]) = {
    if (edges contains e.name)
      throw DuplicateEdgeNameException(e.name)
    
    edges += e.name -> e
    source += e.name -> sn
    target += e.name -> tn
    (e.name, GraphChangeAddEdge(e, sn, tn))
  }

  def addEdge(data: E, sn: VName, tn: VName) : (EName, GraphChange[G,V,E,B]) =
    addEdge(Edge(edges fresh, data), sn, tn)
  
  def addBBox(bbox: BBox[B], parent: Option[BBName]) : (BBName, GraphChange[G,V,E,B]) = {
    if (bboxes contains bbox.name)
      throw DuplicateBBoxNameException(bbox.name)
    
    bboxes += bbox.name -> bbox
    parent map (bboxParent += bbox.name -> _)
    (bbox.name, GraphChangeAddBBox(bbox, parent))
  }

  def addBBox(data: B, parent: Option[BBName] = None) : (BBName, GraphChange[G,V,E,B]) = {
    addBBox(BBox(bboxes fresh, data), parent)
  }
  
  def deleteEdge(en: EName) : GraphChange[G,V,E,B] = {
    val e = edges(en)
    val sn = source(en)
    val tn = target(en)
    source -= en
    target -= en
    edges -= en
    GraphChangeDeleteEdge(e, sn, tn)
  }
  
  def deleteVertex(vn: VName) : GraphChange[G,V,E,B] = {
    val v = verts(vn)
    val deleteIn = GraphChangeSequence(
        target.inv(vn).toSeq map (deleteEdge _))
    val deleteOut = GraphChangeSequence(
        source.inv(vn).toSeq map (deleteEdge _))
    val coord = vertexCoords(vn)
    verts -= vn
    vertexCoords -= vn

    GraphChangeSequence(Seq(
      GraphChangeMoveVertex(vn, coord, (0.0f,0.0f)),
      deleteIn, deleteOut,
      GraphChangeDeleteVertex(v)
    ))
  }
  
  def applyGraphChange(gc: GraphChange[G,V,E,B]) {
    gc match {
      case GraphChangeSequence(seq) =>
        seq foreach (applyGraphChange _)
      case GraphChangeAddVertex(v) => addVertex(v)
      case GraphChangeDeleteVertex(v) => deleteVertex(v.name)
      case GraphChangeAddEdge(e,sn,tn) => addEdge(e,sn,tn)
      case GraphChangeDeleteEdge(e,_,_) => deleteEdge(e.name)
    }
  }
}