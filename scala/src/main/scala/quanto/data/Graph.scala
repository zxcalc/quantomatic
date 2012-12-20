package quanto.data

import Names._

trait GraphException extends Exception

class SafeDeleteVertexException(name: VName, reason: String) extends
Exception("Unable to safely delete " + name + ", because " + reason)
with GraphException

class DanglingEdgeException(edge: EName, endPoint: VName) extends
Exception("Edge: " + edge + " has no endpoint: " + endPoint + " in graph")
with GraphException

trait GraphLike[G,V,E,B,This<:GraphLike[G,V,E,B,This]] {
  def data: G
  def verts: Map[VName,V]
  def edges: Map[EName,E]
  def source: PFun[EName,VName]
  def target: PFun[EName,VName]
  def bboxes: Map[BBName,B]
  def inBBox: BinRel[VName,BBName]
  def bboxParent: PFun[BBName,BBName]

  protected def factory :
    ((G,Map[VName,V],Map[EName,E],PFun[EName,VName],PFun[EName,VName],
     Map[BBName,B],BinRel[VName,BBName],PFun[BBName,BBName])=> This)

  def copy(data: G                         = this.data,
           verts: Map[VName,V]             = this.verts,
           edges: Map[EName,E]             = this.edges,
           source: PFun[EName,VName]       = this.source,
           target: PFun[EName,VName]       = this.target,
           bboxes: Map[BBName,B]           = this.bboxes,
           inBBox: BinRel[VName,BBName]      = this.inBBox,
           bboxParent: PFun[BBName,BBName] = this.bboxParent): This =
    factory(data,verts,edges,source,target,bboxes,inBBox,bboxParent)

  // convenience methods
  def inEdges(vn: VName) = target.codf(vn)
  def outEdges(vn: VName) = source.codf(vn)
  def adjacentEdges(vn: VName) = source.codf(vn) union target.codf(vn)

  def addVertex(vn: VName, data: V) = {
    if (verts contains vn)
      throw new DuplicateVertexNameException(vn) with GraphException

    copy(verts = verts + (vn -> data))
  }

  def newVertex(data: V) = {
    val vn = verts.fresh
    (addVertex(vn, data), vn)
  }

  def addEdge(en: EName, data: E, vns: (VName, VName)) = {
    if (edges contains en)
      throw new DuplicateEdgeNameException(en) with GraphException
    if (!verts.contains(vns._1))
      throw new DanglingEdgeException(en, vns._1)
    if (!verts.contains(vns._1))
      throw new DanglingEdgeException(en, vns._2)

    copy(
      edges = edges + (en -> data),
      source = source + (en -> vns._1),
      target = target + (en -> vns._2)
    )
  }

  def newEdge(data: E, vns: (VName, VName)) = {
    val en = edges.fresh
    (addEdge(en, data, vns), en)
  }
  
  def addBBox(bbn: BBName, data: B, contents: Set[VName] = Set[VName](), parent: Option[BBName] = None) = {
    if (bboxes contains bbn)
      throw new DuplicateBBoxNameException(bbn) with GraphException

    val bboxParent1 = parent match {
      case Some(p) => bboxParent + (bbn -> p)
      case None => bboxParent
    }

    copy(
      bboxes = bboxes + (bbn -> data),
      bboxParent = bboxParent1,
      inBBox = contents.foldLeft(inBBox){ (x,v) => x + (v -> bbn) }
    )
  }

  def newBBox(data: B, contents: Set[VName] = Set[VName](), parent: Option[BBName] = None) = {
    val bbn = bboxes.fresh
    (addBBox(bbn, data, contents, parent), bbn)
  }

  def deleteBBox(bb: BBName) = {
    copy()
  }
  
  def deleteEdge(en: EName) = {
    copy(
      edges = edges - en,
      source = source.unmapDom(en),
      target = target.unmapDom(en)
    )
  }

  def safeDeleteVertex(vn: VName) = {
    if ((source.codf(vn).isEmpty) && (target.codf(vn).isEmpty))
      copy(verts = verts - vn, inBBox = inBBox.unmapDom(vn))
    else throw new SafeDeleteVertexException(vn, "vertex has adjancent edges")
  }

  def deleteVertex(vn: VName) = {
    var g = this
    for (e <- source.codf(vn)) g = g.deleteEdge(e)
    for (e <- target.codf(vn)) g = g.deleteEdge(e)
    g.copy(verts = verts - vn)
  }

  // data updaters
  def updateData(f: G => G)                 = copy(data = f(data))
  def updateVData(vn: VName)(f: V => V)     = copy(verts = verts + (vn -> f(verts(vn))))
  def updateEData(en: EName)(f: E => E)     = copy(edges = edges + (en -> f(edges(en))))
  def updateBBData(bbn: BBName)(f: B => B)  = copy(bboxes = bboxes + (bbn -> f(bboxes(bbn))))

  override def toString = {
    """%s {
      |  verts: %s,
      |  edges: %s,
      |  bboxes: %s,
      |  nesting: %s
      |}""".stripMargin.format(
      data, verts,
      edges.map(kv => kv._1 -> "(%s => %s)::%s".format(source(kv._1), target(kv._1), kv._2)),
      bboxes.map(kv => kv._1 -> "%s::%s".format(inBBox.codf(kv._1), kv._2)),
      bboxParent.map(kv => "%s < %s".format(kv._1, kv._2))
    )
  }
}

class Graph[G,V,E,B](
  val data: G,
  val verts: Map[VName,V]             = Map[VName,V](),
  val edges: Map[EName,E]             = Map[EName,E](),
  val source: PFun[EName,VName]       = PFun[EName,VName](),
  val target: PFun[EName,VName]       = PFun[EName,VName](),
  val bboxes: Map[BBName,B]           = Map[BBName,B](),
  val inBBox: BinRel[VName,BBName]    = BinRel[VName,BBName](),
  val bboxParent: PFun[BBName,BBName] = PFun[BBName,BBName]()
) extends GraphLike[G,V,E,B,Graph[G,V,E,B]]
{
  protected val factory = new Graph[G,V,E,B](_,_,_,_,_,_,_,_)
}

object Graph {
  // name can be ignored on methods that return a graph and a name
  implicit def graphAndNameToGraph[G,V,E,B, N <: Name[N]](t: (Graph[G,V,E,B], Name[N])) : Graph[G,V,E,B] = t._1
}
