package quanto.data

abstract class GraphChange[G,V,E,B] {
  def inverse : GraphChange[G,V,E,B]
}

case class GraphChangeSequence[G,V,E,B](changes: Seq[GraphChange[G,V,E,B]])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeSequence(
    changes.reverse.map(_.inverse))
}

case class GraphChangeAddVertex[G,V,E,B](v: Vertex[V])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeDeleteVertex(v)
}

case class GraphChangeDeleteVertex[G,V,E,B](v: Vertex[V])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeAddVertex(v)
}

case class GraphChangeMoveVertex[G,V,E,B](
    v: Vertex[V],
    oldCoord: (Float,Float),
    newCoord: (Float,Float))
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeMoveVertex(v,newCoord,oldCoord)
}

case class GraphChangeAddEdge[G,V,E,B](
    e: Edge[E],
    s: Vertex[V], t: Vertex[V])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeDeleteEdge(e, s, t)
}

case class GraphChangeDeleteEdge[G,V,E,B](e: Edge[E],
    s: Vertex[V], t: Vertex[V])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeAddEdge(e, s, t)
}

case class GraphChangeAddBBox[G,V,E,B](
    bbox: BBox[B], parent: Option[BBox[B]])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeDeleteBBox(bbox, parent)
}

case class GraphChangeDeleteBBox[G,V,E,B](
    bbox: BBox[B], parent: Option[BBox[B]])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeAddBBox(bbox, parent)
}

case class GraphChangeSetBBox[G,V,E,B](
    oldBBox : Option[BBox[B]], newBBox: Option[BBox[B]])
extends GraphChange[G,V,E,B] {
  def inverse = GraphChangeSetBBox(newBBox, oldBBox)
}


