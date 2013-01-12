// Mix in to add many utility methods to a basic bang-graph object

package quanto.data

import Names._

trait GraphException extends Exception

class SafeDeleteVertexException(name: VName, reason: String) extends
Exception("Unable to safely delete " + name + ", because " + reason)
with GraphException

class DanglingEdgeException(edge: EName, endPoint: VName) extends
Exception("Edge: " + edge + " has no endpoint: " + endPoint + " in graph")
with GraphException

case class GraphSearchContext(exploredV: Set[VName], exploredE: Set[EName])

abstract class GraphLike[G,V,E,B,This<:GraphLike[G,V,E,B,This]] {
  def data: G
  def vdata: Map[VName,V]
  def edata: Map[EName,E]
  def source: PFun[EName,VName]
  def target: PFun[EName,VName]
  def bbdata: Map[BBName,B]
  def inBBox: BinRel[VName,BBName]
  def bboxParent: PFun[BBName,BBName]

  def verts = vdata.keySet
  def edges = edata.keySet
  def bboxes = bbdata.keySet

  override def hashCode = {
    var h = data.hashCode
    h = 41 * h + vdata.hashCode
    h = 41 * h + edata.hashCode
    h = 41 * h + source.hashCode
    h = 41 * h + target.hashCode
    h = 41 * h + bbdata.hashCode
    h = 41 * h + inBBox.hashCode
    h = 41 * h + bboxParent.hashCode
    h
  }

  def canEqual(other: Any) = other.isInstanceOf[GraphLike[_,_,_,_,_]]
  override def equals(other: Any) = other match {
    case that: GraphLike[_,_,_,_,_] => (that canEqual this) &&
      vdata == that.vdata &&
      edata == that.edata &&
      source == that.source &&
      target == that.target &&
      bbdata == that.bbdata &&
      inBBox == that.inBBox &&
      bboxParent == that.bboxParent
    case _ => false
  }


  protected def factory :
    ((G,Map[VName,V],Map[EName,E],PFun[EName,VName],PFun[EName,VName],
     Map[BBName,B],BinRel[VName,BBName],PFun[BBName,BBName])=> This)

  def copy(data: G                         = this.data,
           vdata: Map[VName,V]             = this.vdata,
           edata: Map[EName,E]             = this.edata,
           source: PFun[EName,VName]       = this.source,
           target: PFun[EName,VName]       = this.target,
           bbdata: Map[BBName,B]           = this.bbdata,
           inBBox: BinRel[VName,BBName]    = this.inBBox,
           bboxParent: PFun[BBName,BBName] = this.bboxParent): This =
    factory(data,vdata,edata,source,target,bbdata,inBBox,bboxParent)

  // convenience methods
  def inEdges(vn: VName) = target.codf(vn)
  def outEdges(vn: VName) = source.codf(vn)
  def predVerts(vn: VName) = inEdges(vn).map(source(_))
  def succVerts(vn: VName) = outEdges(vn).map(target(_))
  def contents(bbn: BBName) = inBBox.codf(bbn)

  def adjacentEdges(vn: VName) = source.codf(vn) union target.codf(vn)

  def addVertex(vn: VName, data: V) = {
    if (vdata contains vn)
      throw new DuplicateVertexNameException(vn) with GraphException

    copy(vdata = vdata + (vn -> data))
  }

  def newVertex(data: V) = {
    val vn = vdata.fresh
    (addVertex(vn, data), vn)
  }

  def addEdge(en: EName, data: E, vns: (VName, VName)) = {
    if (edata contains en)
      throw new DuplicateEdgeNameException(en) with GraphException
    if (!vdata.contains(vns._1))
      throw new DanglingEdgeException(en, vns._1)
    if (!vdata.contains(vns._1))
      throw new DanglingEdgeException(en, vns._2)

    copy(
      edata = edata + (en -> data),
      source = source + (en -> vns._1),
      target = target + (en -> vns._2)
    )
  }

  def newEdge(data: E, vns: (VName, VName)) = {
    val en = edata.fresh
    (addEdge(en, data, vns), en)
  }
  
  def addBBox(bbn: BBName, data: B, contents: Set[VName] = Set[VName](), parent: Option[BBName] = None) = {
    if (bbdata contains bbn)
      throw new DuplicateBBoxNameException(bbn) with GraphException

    val bboxParent1 = parent match {
      case Some(p) => bboxParent + (bbn -> p)
      case None => bboxParent
    }

    copy(
      bbdata = bbdata + (bbn -> data),
      bboxParent = bboxParent1,
      inBBox = contents.foldLeft(inBBox){ (x,v) => x + (v -> bbn) }
    )
  }

  def newBBox(data: B, contents: Set[VName] = Set[VName](), parent: Option[BBName] = None) = {
    val bbn = bbdata.fresh
    (addBBox(bbn, data, contents, parent), bbn)
  }

  def deleteBBox(bb: BBName) = {
    copy()
  }
  
  def deleteEdge(en: EName) = {
    copy(
      edata = edata - en,
      source = source.unmapDom(en),
      target = target.unmapDom(en)
    )
  }

  def safeDeleteVertex(vn: VName) = {
    if ((source.codf(vn).isEmpty) && (target.codf(vn).isEmpty))
      copy(vdata = vdata - vn, inBBox = inBBox.unmapDom(vn))
    else throw new SafeDeleteVertexException(vn, "vertex has adjancent edges")
  }

  def deleteVertex(vn: VName) = {
    var g = this
    for (e <- source.codf(vn)) g = g.deleteEdge(e)
    for (e <- target.codf(vn)) g = g.deleteEdge(e)
    g.copy(vdata = vdata - vn)
  }

  // data updaters
  def updateData(f: G => G)                 = copy(data = f(data))
  def updateVData(vn: VName)(f: V => V)     = copy(vdata = vdata + (vn -> f(vdata(vn))))
  def updateEData(en: EName)(f: E => E)     = copy(edata = edata + (en -> f(edata(en))))
  def updateBBData(bbn: BBName)(f: B => B)  = copy(bbdata = bbdata + (bbn -> f(bbdata(bbn))))

  override def toString = {
    """%s {
      |  verts: %s,
      |  edges: %s,
      |  bboxes: %s,
      |  nesting: %s
      |}""".stripMargin.format(
      data, vdata,
      edata.map(kv => kv._1 -> "(%s => %s)::%s".format(source(kv._1), target(kv._1), kv._2)),
      bbdata.map(kv => kv._1 -> "%s::%s".format(inBBox.codf(kv._1), kv._2)),
      bboxParent.map(kv => "%s < %s".format(kv._1, kv._2))
    )
  }

  private def dftSuccessors[T](fromV: VName, exploredV: Set[VName], exploredE: Set[EName])(base: T)
                     (f: (T, EName, GraphSearchContext) => T): (T, Set[VName], Set[EName]) =
  {
    val nextEs = outEdges(fromV).filter(!exploredE.contains(_))

    if (!nextEs.isEmpty) {
      val e = nextEs.min
      val nextV = target(e)

      val (base1, exploredV1, exploredE1) =
        dftSuccessors(nextV, exploredV + nextV, exploredE + e)(base)(f)
      val (base2, exploredV2, exploredE2) =
        dftSuccessors(fromV, exploredV1, exploredE1)(base1)(f)

      val context = GraphSearchContext(exploredV2, exploredE2)
      (f(base2, e, context), exploredV2, exploredE2)
    } else {
      (base, exploredV, exploredE)
    }
  }

  private def dftComponents[T](exploredV: Set[VName], exploredE: Set[EName])(base: T)
                              (f: (T, EName, GraphSearchContext) => T) : T =
  {
    val nextVs = vdata.keySet.filter(!exploredV.contains(_))
    val initialVs = nextVs.filter(inEdges(_).isEmpty)

    // Try to start with the minimal unexplored vertex with no in-edges. Failing that, start with the
    // minimal unexplored vertex.
    val vOpt = if (!initialVs.isEmpty)   Some(initialVs.min)
               else if (!nextVs.isEmpty) Some(nextVs.min)
               else None

    vOpt match {
      case Some(v) =>
        val (base1, exploredV1, exploredE1) = dftSuccessors(v, exploredV + v, exploredE)(base)(f)
        dftComponents[T](exploredV1, exploredE1)(base1)(f)
      case None => base
    }
  }

  def dft[T](base: T)(f: (T, EName, GraphSearchContext) => T): T =
    dftComponents(Set[VName](), Set[EName]())(base)(f)

  // returns a topo ordering. If graph is a dag, all edges will be consistent with this ordering
  def topologicalOrdering: PartialOrdering[VName] = {
    val visited = collection.mutable.Set[VName]()
    var ordMap = Map[VName,Int]()
    var max = 0

    def visit(v: VName) {
      if (!visited.contains(v)) {
        visited += v
        for (e <- outEdges(v)) visit(target(e))
        ordMap += v -> max
        max += 1
      }
    }

    verts.foreach(visit(_))

    new PartialOrdering[VName] {
      def tryCompare(x: VName, y: VName) = (ordMap.get(x), ordMap.get(y)) match {
        case (Some(i1), Some(i2)) => Some(i2 compare i1)
        case _ => None
      }

      def lteq(x: VName, y: VName) = tryCompare(x,y) match {
        case Some(c)  => c != 1
        case None     => false
      }
    }
  }

  def dagCopy: This = {
    // make a copy with no edges
    val noEdges = copy(
      edata  = Map[EName,E](),
      source = PFun[EName,VName](),
      target = PFun[EName,VName]()
    )

    val ord = this.topologicalOrdering

    dft(noEdges) { (graph, e, context) =>
      val s = source(e)
      val t = target(e)

      if (s == t) graph // throw away self-loops
      else {
        // reverse back-edges to break cycles
        graph.addEdge(e, edata(e),
          if (ord.lteq(s,t)) (s,t) else (t,s))
      }
    }
  }

  def simpleCopy: This = {
    var g = copy(
      edata  = Map[EName,E](),
      source = PFun[EName,VName](),
      target = PFun[EName,VName]()
    )
    for (v <- verts; w <- verts) {
      outEdges(v).find(target(_) == w) match {
        case Some(e) => g = g.addEdge(e, edata(e), (v,w))
        case None => ()
      }
    }

    g
  }
}
