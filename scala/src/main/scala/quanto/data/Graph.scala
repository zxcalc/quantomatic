package quanto.data

import Names._
import quanto.util.json._
import math.sqrt
import JsonValues._
import collection.mutable.ArrayBuffer
import quanto.util._
import java.awt.datatransfer.{DataFlavor, Transferable}

trait GraphException extends Exception

class SafeDeleteVertexException(name: VName, reason: String) extends
Exception("Unable to safely delete " + name + ", because " + reason)
with GraphException

class EdgeOtherVertexException(edge: EName, vertex: VName) extends
Exception("Edge: " + edge + " is not connected to vertex: " + vertex)
with GraphException

class WireOtherEdgeException(wire: VName, edge: EName) extends
Exception("Wire: " + wire + " is not connected to edge: " + edge)
with GraphException

class DanglingEdgeException(edge: EName, endPoint: VName) extends
Exception("Edge: " + edge + " has no endpoint: " + endPoint + " in graph")
with GraphException

class CyclicBBoxParentException(bb: BBName, bbp: BBName) extends
Exception("Adding parent " + bbp + " to bbox " + bb + " introduces cycle.")

class PluggingException(msg: String) extends
Exception(msg)

case class GraphSearchContext(exploredV: Set[VName], exploredE: Set[EName])

class GraphLoadException(message: String, cause: Throwable = null)
extends Exception(message, cause)
with GraphException


case class Graph(
                   data: GData                     = GData(),
                   vdata: Map[VName,VData]         = Map[VName,VData](),
                   edata: Map[EName,EData]         = Map[EName,EData](),
                   source: PFun[EName,VName]       = PFun[EName,VName](),
                   target: PFun[EName,VName]       = PFun[EName,VName](),
                   bbdata: Map[BBName,BBData]      = Map[BBName,BBData](),
                   inBBox: BinRel[VName,BBName]    = BinRel[VName,BBName](),
                   bboxParent: PFun[BBName,BBName] = PFun[BBName,BBName]())
{
  def isInput (v: VName): Boolean = vdata(v).isWireVertex && inEdges(v).isEmpty && outEdges(v).size == 1
  def isOutput(v: VName): Boolean = vdata(v).isWireVertex && outEdges(v).isEmpty && inEdges(v).size == 1
  def isInternal(v: VName): Boolean = vdata(v).isWireVertex && outEdges(v).size == 1 && inEdges(v).size == 1
  def inputs: Set[VName] = verts.filter(isInput)
  def outputs: Set[VName] = verts.filter(isOutput)

  def verts: Set[VName] = vdata.keySet
  def edges: Set[EName] = edata.keySet
  def bboxes: Set[BBName] = bbdata.keySet

  override def hashCode: Int = {
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

  def canEqual(other: Any): Boolean = other.isInstanceOf[Graph]

  override def equals(other: Any): Boolean = other match {
    case that: Graph => (that canEqual this) &&
      vdata == that.vdata &&
      edata == that.edata &&
      source == that.source &&
      target == that.target &&
      bbdata == that.bbdata &&
      inBBox == that.inBBox &&
      bboxParent == that.bboxParent
    case _ => false
  }


  protected val factory = new Graph(_,_,_,_,_,_,_,_)

  def copy(data: GData                     = this.data,
           vdata: Map[VName,VData]         = this.vdata,
           edata: Map[EName,EData]         = this.edata,
           source: PFun[EName,VName]       = this.source,
           target: PFun[EName,VName]       = this.target,
           bbdata: Map[BBName,BBData]      = this.bbdata,
           inBBox: BinRel[VName,BBName]    = this.inBBox,
           bboxParent: PFun[BBName,BBName] = this.bboxParent): Graph =
    factory(data,vdata,edata,source,target,bbdata,inBBox,bboxParent)

  // convenience methods
  def inEdges(vn: VName): Set[EName] = target.codf(vn)
  def outEdges(vn: VName): Set[EName] = source.codf(vn)
  def predVerts(vn: VName): Set[VName] = inEdges(vn).map(source(_))
  def succVerts(vn: VName): Set[VName] = outEdges(vn).map(target(_))
  def contents(bbn: BBName): Set[VName] = inBBox.codf(bbn)
  def bboxesContaining(vn: VName): Set[BBName] = inBBox.domf(vn)
  def isBoundary(vn: VName): Boolean = vdata(vn) match {
    case _: WireV => (inEdges(vn).size + outEdges(vn).size) <= 1
    case _ => false
  }

  def vars: Set[String] = vdata.values.foldLeft(Set.empty[String]) {
    case (vs, d: NodeV) =>
      vs ++ d.angle.vars
    case (vs,_) => vs
  }

  /** Returns a set of edge names adjacent to vn */
  def adjacentEdges(vn: VName): Set[EName] = source.codf(vn) union target.codf(vn)

  /** Returns a set of edge names which connect v1 to v2 or vice versa */
  def edgesBetween(v1: VName, v2: VName): Set[EName] = {
    if (v1 == v2) {
      source.codf(v1) intersect target.codf(v1)
    }
    else {
      adjacentEdges(v1) intersect adjacentEdges(v2)
    }
  }

  /**
    * If "e" is not a self-loop, get the vertex connected
    * to "e" which is NOT "v". Otherwise, return "v".
    */
  def edgeGetOtherVertex(e: EName, v: VName): VName =
    if (source(e) == v) target(e)
    else if (target(e) == v) source(e)
    else throw new EdgeOtherVertexException(e, v)

  /**
    * Get the other edge connected to this wire vertex, if there is one
    * @param w a wire vertex
    * @param e an edge
    * @return an edge, optionally
    */
  def wireVertexGetOtherEdge(w: VName, e: EName): Option[EName] = {
    val adj = adjacentEdges(w)
    if (adj contains e) (adj - e).headOption
    else throw new WireOtherEdgeException(w, e)
  }

  /**
   * Partition of all edges into sets, s.t. they connect the same two vertices
   * regardless of edge direction
   */
  def edgePartition() : List[Set[EName]] = {
    var res : List[Set[EName]] = List()
    for ((v1,_) <- vdata; (v2,_) <- vdata if v1 <= v2) {
      val edgeSet = edgesBetween(v1,v2)
      if (edgeSet.nonEmpty) res = edgeSet :: res
    }
    res
  }

  def isBBoxed(v: VName): Boolean = inBBox.domf(v).nonEmpty

  /// by song
  // to compute whether two vertices are in the same bbox.
  def isInSameBBox(v1:VName, v2:VName): Boolean = (inBBox.domf(v1) & inBBox.domf(v2)).nonEmpty

  def addVertex(vn: VName, data: VData): Graph = {
    if (vdata contains vn)
      throw new DuplicateVertexNameException(vn) with GraphException

    copy(vdata = vdata + (vn -> data))
  }

  /**
   * @return A new graph where all vertices have coordinates which align to a
   * grid
   */
  def snapToGrid(): Graph = {

    def roundCoord(d : Double) = {
      math.rint(d * 4.0) / 4.0 // rounds to .25
    }

    val snapped_vdata = vdata.mapValues {vd =>
      val coord = vd.coord
      vd.withCoord(roundCoord(coord._1), roundCoord(coord._2))
    }
    copy(vdata = snapped_vdata)
  }

  def newVertex(data: VData): (Graph, VName) = {
    val vn = vdata.fresh
    (addVertex(vn, data), vn)
  }

  def addEdge(en: EName, data: EData, vns: (VName, VName)): Graph = {
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

  def newEdge(data: EData, vns: (VName, VName)): (Graph, EName) = {
    val en = edata.fresh
    (addEdge(en, data, vns), en)
  }

  def addBBox(bbn: BBName, data: BBData, contents: Set[VName] = Set[VName](), parent: Option[BBName] = None): Graph = {
    if (bbdata contains bbn)
      throw new DuplicateBBoxNameException(bbn) with GraphException

    val g1 = copy(
      bbdata = bbdata + (bbn -> data),
      inBBox = contents.foldLeft(inBBox){ (x,v) => x + (v -> bbn) }
    )

    parent match {
      case Some(p) => g1.setBBoxParent(bbn, Some(p))
      case None => g1
    }
  }

  def bboxParents(bb : BBName) : List[BBName] =
    bboxParent.get(bb) match {
      case Some(bb1) => bb1 :: bboxParents(bb1)
      case None => List()
    }

  def bboxChildren(bb: BBName) : Set[BBName] =
    bboxParent.codf(bb)


  /** Replace the contents of a bang box with new ones
    * NOTE: this affects parents as well. */
  def updateBBoxContents(bbn: BBName, newContents: Set[VName]): Graph = {
    val oldContents = contents(bbn)
    val updateBB = bbn :: bboxParents(bbn)

    var inBB = inBBox

    for (bb1 <- updateBB) {
      oldContents.foreach {v => inBB -= (v -> bb1) }
      newContents.foreach {v => inBB += (v -> bb1) }
    }

    copy( inBBox = inBB )
  }

  /** Change bbox parent. All contents will be removed from old parents and added to
    * new parents. */
  def setBBoxParent(bb: BBName, bbParentOpt: Option[BBName]): Graph = {
    val cont = contents(bb)
    var inBB = inBBox
    var bbP = bboxParent
    val oldParents = bboxParents(bb)
    val newParents = bbParentOpt match {
      case Some(bbParent) =>
        bbP += (bb -> bbParent)
        val newP = bbParent :: bboxParents(bbParent)
        if (newP.contains(bb)) throw new CyclicBBoxParentException(bb, bbParent)

        newP
      case None =>
        bbP -= bb
        List()
    }



    for (bbp <- oldParents) {
      cont.foreach {v => inBB -= (v -> bbp) }
    }

    for (bbp <- newParents) {
      cont.foreach {v => inBB += (v -> bbp) }
    }


    copy( inBBox = inBB , bboxParent = bbP )
  }

  def newBBox(data: BBData, contents: Set[VName] = Set[VName](), parent: Option[BBName] = None): (Graph, BBName) = {
    val bbn = bbdata.fresh
    (addBBox(bbn, data, contents, parent), bbn)
  }

  def deleteBBox(bb: BBName): Graph = {
    copy(
      bbdata = bbdata - bb,
      inBBox = inBBox.unmapCod(bb),
      bboxParent = bboxParent.unmapCod(bb).unmapDom(bb)
    )
  }

  def deleteEdge(en: EName): Graph = {
    copy(
      edata = edata - en,
      source = source.unmapDom(en),
      target = target.unmapDom(en)
    )
  }

  def safeDeleteVertex(vn: VName): Graph = {
    if (source.codf(vn).nonEmpty || target.codf(vn).nonEmpty)
      throw new SafeDeleteVertexException(vn, "vertex has adjancent edges")
    if (inBBox.domf(vn).nonEmpty)
      throw new SafeDeleteVertexException(vn, "vertex is in one or more bboxes")
    copy(vdata = vdata - vn, inBBox = inBBox.unmapDom(vn))
  }

  def deleteVertex(vn: VName): Graph = {
    var g = this
    for (e <- source.codf(vn)) g = g.deleteEdge(e)
    for (e <- target.codf(vn)) g = g.deleteEdge(e)

    g.copy(vdata = vdata - vn, inBBox = inBBox.unmapDom(vn))
  }

  // data updaters
  def updateData(f: GData => GData): Graph = copy(data = f(data))
  def updateVData(vn: VName)(f: VData => VData): Graph = copy(vdata = vdata + (vn -> f(vdata(vn))))
  def updateEData(en: EName)(f: EData => EData): Graph = copy(edata = edata + (en -> f(edata(en))))
  def updateBBData(bbn: BBName)(f: BBData => BBData): Graph = copy(bbdata = bbdata + (bbn -> f(bbdata(bbn))))

  def rename(vrn: Map[VName,VName], ern: Map[EName, EName], brn: Map[BBName,BBName]): Graph = {
    // compute inverses
//    val vrni = vrn.foldLeft(Map[VName,VName]()) { case (mp, (k,v)) => mp + (v -> k) }
//    val erni = ern.foldLeft(Map[EName,EName]()) { case (mp, (k,v)) => mp + (v -> k) }
//    val brni = brn.foldLeft(Map[BBName,BBName]()) { case (mp, (k,v)) => mp + (v -> k) }

    val vdata1 = vdata.foldLeft(Map[VName,VData]()) { case (mp, (k,v)) => mp + (vrn(k) -> v)}
    val edata1 = edata.foldLeft(Map[EName,EData]()) { case (mp, (k,v)) => mp + (ern(k) -> v)}
    val bbdata1 = bbdata.foldLeft(Map[BBName,BBData]()) { case (mp, (k,v)) => mp + (brn(k) -> v)}
    val source1 = source.foldLeft(PFun[EName,VName]()) { case (mp, (k,v)) => mp + (ern(k) -> vrn(v))}
    val target1 = target.foldLeft(PFun[EName,VName]()) { case (mp, (k,v)) => mp + (ern(k) -> vrn(v))}
    val inBBox1 = inBBox.foldLeft(BinRel[VName,BBName]()) { case (mp, (k,v)) => mp + (vrn(k) -> brn(v))}
    val bboxParent1 = bboxParent.foldLeft(PFun[BBName,BBName]()) { case (mp, (k,v)) => mp + (brn(k) -> brn(v))}

    copy(vdata=vdata1,edata=edata1,source=source1,target=target1,
      bbdata=bbdata1,inBBox=inBBox1,bboxParent=bboxParent1)
  }

  // get a subgraph consisting of the given vertices and bboxes, with any edges/nesting between them
  def fullSubgraph(vs: Set[VName], bbs: Set[BBName]): Graph = {
    val es = edges.filter { e => vs.contains(source(e)) && vs.contains(target(e)) }

    val vdata1 = vdata.filter { case (v,_) => vs.contains(v) }
    val edata1 = edata.filter { case (e,_) => es.contains(e) }
    val source1 = source.filter { case (e,_) => es.contains(e) }
    val target1 = target.filter { case (e,_) => es.contains(e) }
    val inBBox1 = inBBox.filter{ case (v,b) => vs.contains(v) && bbs.contains(b) }
    val bbdata1 = bbdata.filter { case (b,_) => bbs.contains(b) }
    val bboxParent1 = bboxParent.filter { case (b1,b2) => bbs.contains(b1) && bbs.contains(b2) }

    copy(data=GData(),vdata=vdata1,edata=edata1,source=source1,target=target1,
      bbdata=bbdata1,inBBox=inBBox1,bboxParent=bboxParent1)
  }

  def renameAvoiding1(g: Graph): (Graph, Map[VName,VName], Map[EName,EName], Map[BBName,BBName]) = {
    val vrn = verts.foldLeft((Map[VName,VName](), g.verts)) { case ((mp,avoid), x) =>
      val x1 = avoid.freshWithSuggestion(x)
      (mp + (x -> x1), avoid + x1)
    }._1

    val ern = edges.foldLeft((Map[EName,EName](), g.edges)) { case ((mp,avoid), x) =>
      val x1 = avoid.freshWithSuggestion(x)
      (mp + (x -> x1), avoid + x1)
    }._1

    val brn = bboxes.foldLeft((Map[BBName,BBName](), g.bboxes)) { case ((mp,avoid), x) =>
      val x1 = avoid.freshWithSuggestion(x)
      (mp + (x -> x1), avoid + x1)
    }._1

    (rename(vrn,ern,brn), vrn, ern, brn)
  }

  def renameAvoiding(g: Graph): Graph = renameAvoiding1(g)._1

  // append the given graph. note that its names should already be fresh
  def appendGraph(g: Graph): Graph = {
    val coords = verts.map(vdata(_).coord)

    // Pick any vertex in g and offset until that vertex is not sitting exactly
    // on top of another.
    var offset = 0.0
    g.verts.headOption.foreach { v1 =>
      val (x,y) = g.vdata(v1).coord
      while (coords.contains((x + offset, y))) offset += 1.0
    }

    val g1 = g.verts.foldLeft(g) { (g1,v) =>
      g1.updateVData(v) { d => d.withCoord (d.coord._1 + offset, d.coord._2) }
    }

    copy(
      vdata = vdata ++ g1.vdata,
      source = source ++ g1.source,
      target = target ++ g1.target,
      inBBox = inBBox ++ g1.inBBox,
      edata = edata ++ g1.edata,
      bbdata = bbdata ++ g1.bbdata,
      bboxParent = bboxParent ++ g1.bboxParent
    )
  }

  def plugBoundaries(b1: VName, b2: VName): Graph = {
    // pull a boundary edge, which we'll inherit the data from
    val be = inEdges(b2).headOption.getOrElse(
      outEdges(b2).headOption.getOrElse (
        throw new PluggingException("Target boundary is an isolated point.")))
    val beData = edata(be)

    // figure out who should be the source and target
    val (s,t) = (
      predVerts(b1).headOption, succVerts(b1).headOption,
      predVerts(b2).headOption, succVerts(b2).headOption) match {
      case (None, Some(t1), Some(s1), None) => (s1,t1)
      case (Some(s1), None, None, Some(t1)) => (s1,t1)
      case (Some(s1), None, Some(t1), None) if !beData.isDirected => (s1,t1)
      case (None, Some(s1), None, Some(t1)) if !beData.isDirected => (s1,t1)
      case _ => throw new PluggingException("Bad boundary arity")
    }

    this.deleteVertex(b1).deleteVertex(b2).addEdge(be, beData, (s,t))
  }

  // add g to this graph, plugging 'b' in this graph into 'bg' in g.
  def plugGraph(g: Graph, b: VName, bg: VName): Graph = {
    // freshen target graph w.r.t. source
    val (g1, vrn, _, _) = g.renameAvoiding1(this)
    val bg1 = vrn(bg)

    // re-position g relative to this graph, using the boundaries as a guide
    val bgcoord = g1.vdata(bg1).coord
    val bcoord = vdata(b).coord
    val dx = bcoord._1 - bgcoord._1
    val dy = bcoord._2 - bgcoord._2

    var g2 = g1.verts.foldLeft(g1) { (g1,v) =>
      g1.updateVData(v) { d => d.withCoord (d.coord._1 + dx, d.coord._2 + dy) }
    }

    this.appendGraph(g2).plugBoundaries(b, bg1)
  }

  override def toString: String = {
    """%s {
      |  verts: %s,
      |  edges: %s,
      |  bboxes: %s,
      |  nesting: %s
      |}""".stripMargin.format(
        data, vdata,
        edata.map(kv => kv._1 -> "(%s => %s)::%s".format(source(kv._1), target(kv._1), kv._2)),
        bbdata.map(kv => kv._1 -> "%s::%s".format(inBBox.codf(kv._1), kv._2)),
        bboxParent.toString
      )
  }

  private def dftSuccessors[T](fromV: VName, exploredV: Set[VName], exploredE: Set[EName])(base: T)
                              (f: (T, EName, GraphSearchContext) => T): (T, Set[VName], Set[EName]) =
  {
    val nextEs = outEdges(fromV).filter(!exploredE.contains(_))

    if (nextEs.nonEmpty) {
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
    val vOpt = if (initialVs.nonEmpty)   Some(initialVs.min)
    else if (nextVs.nonEmpty) Some(nextVs.min)
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


  private def bbDft(bb : BBName, bbSeq : collection.mutable.Buffer[BBName], bbs : collection.mutable.Set[BBName]) {
    for (ch <- bboxParent.codf(bb)) bbDft(ch, bbSeq, bbs)
    if (bbs.contains(bb)) {
      bbs.remove(bb)
      bbSeq += bb
    }
  }

  def bboxesChildrenFirst: Seq[BBName] = {
    val bbSeq = collection.mutable.Buffer[BBName]()
    val bbs = collection.mutable.Set[BBName](bboxes.toSeq : _*)
    while (bbs.nonEmpty) bbDft(bbs.iterator.next(),bbSeq,bbs)

    bbSeq.toSeq
  }

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

    verts.foreach(visit)

    new PartialOrdering[VName] {
      def tryCompare(x: VName, y: VName): Option[Int] = (ordMap.get(x), ordMap.get(y)) match {
        case (Some(i1), Some(i2)) => Some(i2 compare i1)
        case _ => None
      }

      def lteq(x: VName, y: VName): Boolean = tryCompare(x,y) match {
        case Some(c)  => c != 1
        case None     => false
      }
    }
  }

  def dagCopy: Graph = {
    // make a copy with no edges
    val noEdges = copy(
      edata  = Map[EName,EData](),
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

  def simpleCopy: Graph = {
    var g = copy(
      edata  = Map[EName,EData](),
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

object Graph {
//  val Flavor = new DataFlavor(Graph.getClass, "X-quantoderive/qgraph; class=<quanto.data.Graph>;")
//  class GraphPacket(graph: Graph, val theory: Theory) extends Transferable {
//    def getTransferData(f: DataFlavor) = this
//    def isDataFlavorSupported(f: DataFlavor) = { f == Graph.Flavor }
//    def getTransferDataFlavors = Array(Graph.Flavor)
//  }

  implicit def qGraphAndNameToQGraph[N <: Name[N]](t: (Graph, Name[N])) : Graph = t._1

  def apply(theory: Theory): Graph = Graph(data = GData(theory = theory))

  
  def fromJson(s: String, thy: Theory): Graph =
    try   { fromJson(Json.parse(s), thy) }
    catch { case e:JsonParseException => throw new GraphLoadException("Error parsing JSON", e) }


  def fromJson(s: String): Graph = fromJson(s, Theory.DefaultTheory)

  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory): Graph = try {
    Function.chain[Graph](Seq(

      (json ? "wire_vertices").asObject.foldLeft(_) { (g,v) =>
        g.addVertex(v._1, WireV.fromJson(v._2, thy))
      },

      (json ? "node_vertices").asObject.foldLeft(_) { (g,v) =>
        g.addVertex(v._1, NodeV.fromJson(v._2, thy))
      },

      (json ? "dir_edges").asObject.foldLeft(_) { (g,e) =>
        val data = e._2.getOrElse("data", thy.defaultEdgeData).asObject
        val annotation = (e._2 ? "annotation").asObject
        g.addEdge(e._1, DirEdge(data, annotation, thy),
          ((e._2 / "src").stringValue, (e._2 / "tgt").stringValue))
      },

      (json ? "undir_edges").asObject.foldLeft(_) { (g,e) =>
        val data = e._2.getOrElse("data", thy.defaultEdgeData).asObject
        val annotation = (e._2 ? "annotation").asObject
        g.addEdge(e._1, UndirEdge(data, annotation, thy), ((e._2 / "src").stringValue, (e._2 / "tgt").stringValue))
      },

      (json ? "bang_boxes").asObject.foldLeft(_) { (g,bb) =>
        val data = (bb._2 ? "data").asObject
        val annotation = (bb._2 ? "annotation").asObject
        val contains = (bb._2 ? "contents").vectorValue map { VName(_) }
        val parent = bb._2.get("parent") map { BBName(_) }
        g.addBBox(bb._1, BBData(data, annotation), contains.toSet, parent)
      }

    ))({
      val data = (json ? "data").asObject
      val annotation = (json ? "annotation").asObject
      Graph(GData(data, annotation, thy))
    })
  } catch {
    case e: JsonAccessException =>
      throw new GraphLoadException(e.getMessage, e)
    case e: Exception =>
      e.printStackTrace()
      throw new GraphLoadException("Unexpected error reading JSON", e)
  }

  def toJson(graph: Graph, thy: Theory = Theory.DefaultTheory): Json = {
    val (wireVertices, nodeVertices) = graph.vdata.foldLeft((JsonObject(), JsonObject()))
    {
      case ((objW,objN), (v,w: WireV)) => (objW + (v.toString -> w.toJson), objN)
      case ((objW,objN), (v,n: NodeV)) => (objW, objN + (v.toString -> n.toJson))
    }

    val (dirEdges, undirEdges) = graph.edata.foldLeft((JsonObject(), JsonObject()))
    { case ((objD,objU), (e,d)) =>
      val entry = e.toString -> (d.toJson + ("src" -> graph.source(e).toString, "tgt" -> graph.target(e).toString))
      if (d.isDirected) (objD + entry, objU) else (objD, objU + entry)
    }

    val bangBoxes = graph.bbdata.foldLeft(JsonObject()) { case (obj, (bb, d)) =>
      obj + (bb.toString ->
        JsonObject(
          "contents"   -> JsonArray(graph.contents(bb)),
          "parent"     -> (graph.bboxParent.get(bb) match {
            case Some(p) => JsonString(p.toString)
            case None    => JsonNull }),
          "data"       -> d.data,
          "annotation" -> d.annotation
        ).noEmpty)
    }

    JsonObject(
      "wire_vertices" -> wireVertices.asObjectOrKeyArray,
      "node_vertices" -> nodeVertices.asObjectOrKeyArray,
      "dir_edges"     -> dirEdges,
      "undir_edges"   -> undirEdges,
      "bang_boxes"    -> bangBoxes,
      "data"          -> graph.data.data,
      "annotation"    -> graph.data.annotation
    ).noEmpty
  }

  def random(nverts: Int, nedges: Int, nbboxes: Int = 0): Graph = {
    val rand = new util.Random
    var randomGraph = Graph()
    for (i <- 1 to nverts) {
      val p = (rand.nextDouble * 6.0 - 3.0, rand.nextDouble * 6.0 - 3.0)
      if (rand.nextBoolean()) randomGraph = randomGraph.newVertex(NodeV(p))
      else randomGraph = randomGraph.newVertex(WireV(p))
    }

    if (nverts != 0) {
      val sources = new ArrayBuffer[VName](randomGraph.vdata.keys.size)
      val targets = new ArrayBuffer[VName](randomGraph.vdata.keys.size)
      randomGraph.vdata.keys.foreach{k => sources += k; targets += k}
      for(j <- 1 to nedges if sources.nonEmpty && targets.nonEmpty) {
        val (si,ti) = (rand.nextInt(sources.size), rand.nextInt(targets.size))
        val s = sources(si)
        val t = targets(ti)
        if (randomGraph.vdata(s).isWireVertex) sources -= s
        if (randomGraph.vdata(t).isWireVertex) targets -= t

        randomGraph = randomGraph.newEdge(DirEdge(), (s,t))
      }

      val varray = randomGraph.vdata.keys.toArray

      for (i <- 1 to nbboxes) {
        val randomVSet = (1 to sqrt(nverts).toInt).foldLeft(Set[VName]()) { (s,_) =>
          s + varray(rand.nextInt(varray.length))
        }

        randomGraph = randomGraph.newBBox(BBData(), randomVSet, None)
      }
    }

    randomGraph
  }

  def randomDag(nverts: Int, nedges: Int): Graph = {
    val rand = new util.Random
    var randomGraph = Graph()
    for (i <- 1 to nverts) {
      val p = (rand.nextDouble * 6.0 - 3.0, rand.nextDouble * 6.0 - 3.0)
      randomGraph = randomGraph.newVertex(NodeV(p))
    }
    val varray = randomGraph.vdata.keys.toArray

    // must have at least two verts to add edges since no self-loops allowed
    if (nverts > 1)
      for(j <- 1 to nedges) {
        val x = rand.nextInt(varray.length)
        val y = rand.nextInt(varray.length - 1)
        val s = varray(x)
        val t = varray(if (y >= x) y+1 else y)
        randomGraph = randomGraph.newEdge(DirEdge(), if (s <= t) (s,t) else (t,s))
      }

    randomGraph
  }
}
