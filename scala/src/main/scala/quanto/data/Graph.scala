package quanto.data

import Names._

class SafeDeleteException[N <: Name[N]](name: Name[N], reason: String) extends
Exception("Unable to safely delete " + name + ", because " + reason)

class Graph[G,V,E,B](
  val name: GName,
  val data: G,
  val verts: Map[VName,V]             = Map(),
  val edges: Map[EName,E]             = Map(),
  val source: PFun[EName,VName]       = PFun(),
  val target: PFun[EName,VName]       = PFun(),
  val bboxes: Map[BBName,B]           = Map(),
  val inBBox: PFun[VName,BBName]      = PFun(),
  val bboxParent: PFun[BBName,BBName] = PFun())
{

  def copy(name: GName                     = this.name,
           data: G                         = this.data,
           verts: Map[VName,V]             = this.verts,
           edges: Map[EName,E]             = this.edges,
           source: PFun[EName,VName]       = this.source,
           target: PFun[EName,VName]       = this.target,
           bboxes: Map[BBName,B]           = this.bboxes,
           inBBox: PFun[VName,BBName]      = this.inBBox,
           bboxParent: PFun[BBName,BBName] = this.bboxParent): Graph[G,V,E,B] = {
    new Graph(name,data,verts,edges,source,target,bboxes,inBBox,bboxParent)
  }

  // convenience methods
  def inEdges(vn: VName) = target.inv(vn)
  def outEdges(vn: VName) = source.inv(vn)
  def adjacentEdges(vn: VName) = source.inv(vn) union target.inv(vn)

  def addVertex(vn: VName, data: V) = {
    if (verts contains vn)
      throw new DuplicateVertexNameException(vn)

    copy(verts = verts + (vn -> data))
  }

  def newVertex(data: V) = {
    val vn = verts.fresh
    (addVertex(vn, data), vn)
  }

  def addEdge(en: EName, data: E, vns: (VName, VName)) = {
    if (edges contains en)
      throw new DuplicateEdgeNameException(en)

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
      throw new DuplicateBBoxNameException(bbn)

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
  
  def deleteEdge(en: EName) = {
    copy(
      edges = edges - en,
      source = source - en,
      target = target - en
    )
  }

  def safeDeleteVertex(vn: VName) = {
    if ((source.inv(vn).isEmpty) && (target.inv(vn).isEmpty))
      copy(verts = verts - vn, inBBox = inBBox - vn)
    else throw new SafeDeleteException(vn, "vertex has adjancent edges")
  }

  def deleteVertex(vn: VName) = {
    var g = this
    for (e <- source.inv(vn)) g = g.deleteEdge(e)
    for (e <- target.inv(vn)) g = g.deleteEdge(e)
    g.copy(verts = verts - vn)
  }

  // data updaters
  def updateData(f: G => G)                 = copy(data = f(data))
  def updateVData(vn: VName)(f: V => V)     = copy(verts = verts + (vn -> f(verts(vn))))
  def updateEData(en: EName)(f: E => E)     = copy(edges = edges + (en -> f(edges(en))))
  def updateBBData(bbn: BBName)(f: B => B)  = copy(bboxes = bboxes + (bbn -> f(bboxes(bbn))))

  override def toString = {
    """%s::%s {
      |  verts: %s,
      |  edges: %s,
      |  bboxes: %s,
      |  nesting: %s
      |}""".stripMargin.format(
      name, data, verts,
      edges.map(kv => kv._1 -> "(%s => %s)::%s".format(source(kv._1), target(kv._1), kv._2)),
      bboxes.map(kv => kv._1 -> "%s::%s".format(inBBox.inv(kv._1), kv._2)),
      bboxParent.map(kv => "%s < %s".format(kv._1, kv._2))
    )
  }
}

object Graph {
  def apply[G,V,E,B](
    name: GName,
    data: G                         = (),
    verts: Map[VName,V]             = Map[VName,V](),
    edges: Map[EName,E]             = Map[EName,E](),
    source: PFun[EName,VName]       = PFun[EName,VName](),
    target: PFun[EName,VName]       = PFun[EName,VName](),
    bboxes: Map[BBName,B]           = Map[BBName,B](),
    inBBox: PFun[VName,BBName]      = PFun[VName,BBName](),
    bboxParent: PFun[BBName,BBName] = PFun[BBName,BBName]()): Graph[G,V,E,B] =
  new Graph(name,data,verts,edges,source,target,bboxes,inBBox,bboxParent)

  // name can be ignored on methods that return a graph and a name
  implicit def graphAndNameToGraph[G,V,E,B, N <: Name[N]](t: (Graph[G,V,E,B],Name[N])) = t._1
}
