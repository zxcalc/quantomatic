package quanto.data

abstract case class GraphChange[V,E] {
  def inverse : GraphChange[V,E]
}

case class GraphChangeSequence[V,E](changes: Seq[GraphChange[V,E]])
extends GraphChange[V,E] {
  def inverse = GraphChangeSequence(
    changes.reverse.map(_.inverse))
}

case class GraphChangeAddVertex[V,E](val v: Vertex[V])
extends GraphChange[V,E] {
  def inverse = GraphChangeDeleteVertex(v)
}

case class GraphChangeDeleteVertex[V,E](val v: Vertex[V])
extends GraphChange[V,E] {
  def inverse = GraphChangeAddVertex(v)
}

case class GraphChangeAddEdge[V,E](
    val e: Edge[E],
    val s: Vertex[V], val t: Vertex[V])
extends GraphChange[V,E] {
  def inverse = GraphChangeDeleteEdge(e, s, t)
}

case class GraphChangeDeleteEdge[V,E](val e: Edge[E],
    val s: Vertex[V], val t: Vertex[V])
extends GraphChange[V,E] {
  def inverse = GraphChangeAddEdge(e, s, t)
}