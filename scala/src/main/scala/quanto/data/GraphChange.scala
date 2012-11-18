package quanto.data

abstract case class GraphChange[G,V,E,B] {
  def inverse : GraphChange[G,V,E,B]
}

case class GraphChangeSequence[G,V,E,B](changes: Seq[GraphChange[G,V,E,B]])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeSequence(
    changes.reverse.map(_.inverse))
}

case class GraphChangeAddVertex[G,V,E,B](val v: Vertex[V])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeDeleteVertex(v)
}

case class GraphChangeDeleteVertex[G,V,E,B](val v: Vertex[V])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeAddVertex(v)
}

case class GraphChangeMoveVertex[G,V,E,B](
    val vname: String,
    val oldCoord: Tuple2[Float,Float],
    val newCoord: Tuple2[Float,Float])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeMoveVertex(vname,newCoord,oldCoord)
}

case class GraphChangeAddEdge[G,V,E,B](
    val e: Edge[E],
    val s: Vertex[V], val t: Vertex[V])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeDeleteEdge(e, s, t)
}

case class GraphChangeDeleteEdge[G,V,E,B](val e: Edge[E],
    val s: Vertex[V], val t: Vertex[V])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeAddEdge(e, s, t)
}

case class GraphChangeAddBBox[G,V,E,B](
    val bbox: BBox[B], val parent: Option[BBox[B]])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeDeleteBBox(bbox, parent)
}

case class GraphChangeDeleteBBox[G,V,E,B](
    val bbox: BBox[B], val parent: Option[BBox[B]])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeAddBBox(bbox, parent)
}

case class GraphChangeSetBBox[G,V,E,B](
    val oldBBox : Option[BBox[B]], newBBox: Option[BBox[B]])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeSetBBox(newBBox, oldBBox)
}


