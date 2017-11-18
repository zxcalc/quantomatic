package quanto.data

import Names._
import quanto.cosy.AdjMat
import quanto.util.json._
import math.sqrt
import JsonValues._
import collection.mutable.ArrayBuffer
import quanto.util._
import java.awt.datatransfer.{DataFlavor, Transferable}

import scala.annotation.tailrec

class GraphException(msg: String, cause: Throwable = null) extends Exception(msg, cause)
class PluggingException(msg: String) extends GraphException(msg)

case class GraphSearchContext(exploredV: Set[VName], exploredE: Set[EName])

class GraphLoadException(message: String, cause: Throwable = null)
extends GraphException(message, cause)

sealed abstract class BBOp {
  def bb: BBName
  def shortName: String
  //override def toString: String = shortName
}

case class BBExpand(bb: BBName, mp: GraphMap, fresh: PFun[String,String]) extends BBOp { def shortName = "E(" + bb + ")" }
case class BBCopy(bb: BBName, mp: GraphMap) extends BBOp { def shortName = "C(" + bb + ")" }
case class BBDrop(bb: BBName, fresh: PFun[String,String]) extends BBOp { def shortName = "D(" + bb + ")" }
case class BBKill(bb: BBName) extends BBOp { def shortName = "K(" + bb + ")" }


case class Graph(
                   data: GData                     = GData(),
                   vdata: Map[VName,VData]         = Map[VName,VData](),
                   edata: Map[EName,EData]         = Map[EName,EData](),
                   source: PFun[EName,VName]       = PFun[EName,VName](),
                   target: PFun[EName,VName]       = PFun[EName,VName](),
                   bbdata: Map[BBName,BBData]      = Map[BBName,BBData](),
                   inBBox: BinRel[VName,BBName]    = BinRel[VName,BBName](),
                   bboxParent: PFun[BBName,BBName] = PFun[BBName,BBName]()) extends Ordered[Graph]
{
  def isInput (v: VName): Boolean = vdata(v).isWireVertex && inEdges(v).isEmpty && outEdges(v).size == 1
  def isOutput(v: VName): Boolean = vdata(v).isWireVertex && outEdges(v).isEmpty && inEdges(v).size == 1
  def isInternal(v: VName): Boolean = vdata(v).isWireVertex && outEdges(v).size == 1 && inEdges(v).size == 1
  def isBoundary(vn: VName): Boolean =
    vdata(vn).isWireVertex && (inEdges(vn).size + outEdges(vn).size) <= 1
  def isCircle(vn: VName): Boolean =
    vdata(vn).isWireVertex && inEdges(vn).size == 1 && inEdges(vn) == outEdges(vn)

  def typeOf(v: VName): String = vdata(v).typ
  def isAdjacentToBoundary(v: VName): Boolean = adjacentVerts(v).exists(isBoundary)
  def isAdjacentToType(v: VName, t: String): Boolean = adjacentVerts(v).exists(typeOf(_) == t)
  def isWireVertex(v: VName) = vdata(v).isWireVertex

  def representsWire(vn: VName) = vdata(vn).isWireVertex &&
    (predVerts(vn).headOption match {
      case None => true
      case Some(vn1) => vn == vn1 || !vdata(vn1).isWireVertex
    })

  def representsBareWire(vn: VName) =
    isInput(vn) &&
      (succVerts(vn).headOption match {
        case None => false
        case Some(vn1) => isOutput(vn1)
      })

  def arity(v: VName) = adjacentEdges(v).size

  def verts: Set[VName] = vdata.keySet
  def edges: Set[EName] = edata.keySet
  def bboxes: Set[BBName] = bbdata.keySet

  def inputs: Set[VName] = verts.filter(isInput)
  def outputs: Set[VName] = verts.filter(isOutput)
  def boundary: Set[VName] = verts.filter(isBoundary)

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


  def vars: Set[Var] = vdata.values.foldLeft(Set.empty[Var]) {
    case (vs, d: NodeV) =>
      vs ++ d.angle.vars
    case (vs,_) => vs
  }

  /** Returns a set of vertex names adjacent to vn */
  def adjacentVerts(vn: VName): Set[VName] = predVerts(vn) union succVerts(vn)

  /** Returns a set of vertex names adjacent to, and including, vset */
  def extendToAdjacentVerts(vset: Set[VName]): Set[VName] =
    vset.foldRight(Set[VName]()) { (v,vs) => (vs union adjacentVerts(v)) + v }

  /** Returns a set of vertex names adjacent to, but not including, vset */
  def adjacentVerts(vset: Set[VName]): Set[VName] =
    vset.foldRight(Set[VName]()) { (v,vs) => vs union adjacentVerts(v) } -- vset

  /** Returns a set of edge names adjacent to vn */
  def adjacentEdges(vn: VName): Set[EName] = source.codf(vn) union target.codf(vn)

  /** Returns a set of edge names adjacent to any vertex in vset */
  def adjacentEdges(vset: Set[VName]): Set[EName] =
    vset.foldRight(Set[EName]()) { (v,es) => es union adjacentEdges(v) }

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
    else throw new GraphException("Edge: " + e + " is not connected to vertex: " + v)

  /**
    * Get the other edge connected to this wire vertex, if there is one
    * @param w a wire vertex
    * @param e an edge
    * @return an edge, optionally
    */
  def wireVertexGetOtherEdge(w: VName, e: EName): Option[EName] = {
    val adj = adjacentEdges(w)
    if (adj contains e) (adj - e).headOption
    else throw new GraphException("Wire: " + w + " is not connected to edge: " + e)
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
      throw new DuplicateVertexNameException(vn)

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
      throw new DuplicateEdgeNameException(en)
    if (!vdata.contains(vns._1))
      throw new GraphException("Edge: " + en + " has no endpoint: " + vns._1 + " in graph")
    if (!vdata.contains(vns._1))
      throw new GraphException("Edge: " + en + " has no endpoint: " + vns._2 + " in graph")

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
      throw new DuplicateBBoxNameException(bbn)

    val g1 = copy(
      bbdata = bbdata + (bbn -> data),
      inBBox = contents.foldLeft(inBBox){ (x,v) => x + (v -> bbn) }
    )

    parent match {
      case Some(p) => g1.setBBoxParent(bbn, Some(p))
      case None => g1
    }
  }

  /**
    * A list of bbox parents, with the closest ancestor first
    * @param bb a bbox
    * @return
    */
  def bboxParentList(bb : BBName): List[BBName] =
    bboxParent.get(bb) match {
      case Some(bb1) => bb1 :: bboxParentList(bb1)
      case None => List()
    }

  /**
    * The set of all parents for a given bbox
    * @param bb
    * @return
    */
  def bboxParents(bb: BBName): Set[BBName] =
    bboxParent.get(bb) match {
      case Some(bb1) => bboxParents(bb1) + bb1
      case None => Set()
    }

  def bboxChildren(bb: BBName) : Set[BBName] =
    bboxParent.codf(bb)


  def addToBBox(v: VName, bb: BBName): Graph = {
    copy(inBBox = inBBox + (v,bb))
  }

  def addToBBoxes(v: VName, bboxes: Set[BBName]): Graph = {
    copy(inBBox = bboxes.foldRight(inBBox) { (bb,mp) => mp + (v,bb) })
  }

  /** Replace the contents of a bang box with new ones
    * NOTE: this affects parents as well. */
  def updateBBoxContents(bbn: BBName, newContents: Set[VName]): Graph = {
    val oldContents = contents(bbn)
    val updateBB = bbn :: bboxParentList(bbn)

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
    val oldParents = bboxParentList(bb)
    val newParents = bbParentOpt match {
      case Some(bbParent) =>
        bbP += (bb -> bbParent)
        val newP = bbParent :: bboxParentList(bbParent)
        if (newP.contains(bb))
          throw new GraphException("Adding parent " + bbParent + " to bbox " + bb + " introduces cycle.")

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

  def deleteEdges(es: Set[EName]): Graph = es.foldRight(this) { (e,g) => g.deleteEdge(e) }

  def safeDeleteVertex(vn: VName): Graph = {
    if (source.codf(vn).nonEmpty || target.codf(vn).nonEmpty)
      throw new GraphException("Unable to safely delete " + vn + ", because vertex has adjancent edges")
    if (inBBox.domf(vn).nonEmpty)
      throw new GraphException("Unable to safely delete " + vn + ", because vertex is in one or more bboxes")
    copy(vdata = vdata - vn, inBBox = inBBox.unmapDom(vn))
  }

  def deleteVertex(vn: VName): Graph = {
    var g = this
    for (e <- source.codf(vn)) g = g.deleteEdge(e)
    for (e <- target.codf(vn)) g = g.deleteEdge(e)

    g.copy(vdata = vdata - vn, inBBox = inBBox.unmapDom(vn))
  }

  def deleteVertices(vs: Set[VName]): Graph = vs.foldRight(this) { (v, g) => g.deleteVertex(v) }

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

  def makeRenaming(avoidVerts: Set[VName],
                   avoidEdges: Set[EName],
                   avoidBBoxes: Set[BBName],
                   initialMap: GraphMap = GraphMap()): GraphMap = {
    var mp = initialMap
    var avoidV = avoidVerts
    var avoidE = avoidEdges
    var avoidBB = avoidBBoxes

    for (x <- verts if !mp.v.domSet.contains(x)) {
      val fr = avoidV.freshWithSuggestion(x)
      mp = mp.addVertex(x -> fr)
      avoidV = avoidV + fr
    }

    for (x <- edges if !mp.e.domSet.contains(x)) {
      val fr = avoidE.freshWithSuggestion(x)
      mp = mp.addEdge(x -> fr)
      avoidE = avoidE + fr
    }

    for (x <- bboxes if !mp.bb.domSet.contains(x)) {
      val fr = avoidBB.freshWithSuggestion(x)
      mp = mp.addBBox(x -> fr)
      avoidBB = avoidBB + fr
    }

    mp
  }

  def renameAvoiding(g: Graph): Graph = makeRenaming(g.verts, g.edges, g.bboxes).image(this)

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

  // form a new graph by merging the given (non-empty) set of vertices into a new vertex with the given name
  def mergeVertices(vs: Set[VName], newV: VName): Graph = {
    val rep = vs.head
    val bboxes = inBBox.directImage(vs)

    val source1 = source.inverseImage(vs).foldRight(source) { (e, mp) => mp + (e -> newV) }
    val target1 = target.inverseImage(vs).foldRight(target) { (e, mp) => mp + (e -> newV) }

    val g1 = if (verts.contains(newV)) this else addVertex(newV, vdata(rep))

    g1.copy(source = source1, target = target1)
      .deleteVertices(vs - newV)
      .addToBBoxes(newV, bboxes)
  }

  // add g to this graph, plugging 'b' in this graph into 'bg' in g.
  def plugGraph(g: Graph, b: VName, bg: VName): Graph = {
    // freshen target graph w.r.t. source
    val mp = makeRenaming(verts, edges, bboxes)
    val g1 = mp.image(g)
    val bg1 = mp.v(bg)

    // re-position g relative to this graph, using the boundaries as a guide
    val bgcoord = g1.vdata(bg1).coord
    val bcoord = vdata(b).coord
    val dx = bcoord._1 - bgcoord._1
    val dy = bcoord._2 - bgcoord._2

    val g2 = g1.verts.foldLeft(g1) { (g1,v) =>
      g1.updateVData(v) { d => d.withCoord (d.coord._1 + dx, d.coord._2 + dy) }
    }

    this.appendGraph(g2).plugBoundaries(b, bg1)
  }

  def compare(that: Graph): Int = {
    val x : Graph = this
    val y : Graph = that
    if (x.verts.size > y.verts.size) {
      1
    } else {
      if (x.verts.size == y.verts.size) {
        if (x.edges.size > y.edges.size) {
          1
        } else {
          if (x.edges.size == y.edges.size) {
            0
          } else {
            -1
          }
        }
      } else {
        -1
      }
    }
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

  def expandWire(w: VName): (Graph, (VName, VName, EName)) = {
    val ed = adjacentEdges(w).headOption match {
      case Some(e) => edata(e)
      case None =>
        throw new GraphException("attempted to expand wire " + w + ", which has no adjacent edges")
    }

    val newW = verts.freshWithSuggestion(w)
    val newE = edges.freshWithSuggestion(Names.defaultEName)
    val newBB = inBBox.domf(w).foldRight(inBBox) { (bb, mp) => mp + (newW -> bb) }

    var g = addVertex(newW, vdata(w)).copy(inBBox = newBB)

    outEdges(w).headOption match {
      case None => // 'w' is an output, so it should stay an output
        g = g.addEdge(newE, ed, newW -> w)
        inEdges(w).headOption.foreach{e =>
          g = g.deleteEdge(e).addEdge(e, ed, source(e) -> newW)
        }

        (g, (newW, w, newE))
      case Some(e) => // 'w' is not an output, so we don't care
        g = g.addEdge(newE, ed, w -> newW)
        g = g.deleteEdge(e).addEdge(e, ed, newW -> target(e))

        (g, (w, newW, newE))
    }
  }

  def flipEdge(e: EName): Graph = {
    val s = source(e)
    val t = target(e)
    copy(source = source + (e -> t), target = target + (e -> s))
  }

  def collapseWire(e: EName): Graph = {
    val ws = Set(source(e), target(e))
    deleteEdge(e).mergeVertices(ws, ws.min)
  }

  def edgeToWire(e: EName): Graph = {
    val s = source(e)
    val t = target(e)
    val w = verts.fresh
    val ed = edata(e)

    this
      .deleteEdge(e)
      .addVertex(w, WireV(theory = ed.theory))
      .addToBBoxes(w, inBBox.domf(s) union inBBox.domf(t))
      .newEdge(ed, s -> w)
      .newEdge(ed, w -> t)
  }

  def wireToEdge(w: VName): Graph = {
    val es = adjacentEdges(w)
    if (es.size == 2) {
      val (e1, e2) = (es.min, es.max)

      // direct edge in the same direction as the minimum of the two edges connected to w
      val endPoints =
        if (target(e1) == w) (source(e1), edgeGetOtherVertex(e2,w))
        else (edgeGetOtherVertex(e2,w), target(e1))

      this
        .deleteVertex(w)
        .addEdge(e1, edata(e1), endPoints)
    } else this
  }

  /**
    * Put graph in normal form, where each (non-bare) wire has exactly 1 wire vertex
    * @return
    */
  def normalise: Graph = {
    var ch = false
    var g = this

    for (e <- edges) {
      val s = source(e)
      val t = target(e)
      (vdata(s), vdata(t)) match {
        case (_: NodeV, _: NodeV) =>
          g = g.edgeToWire(e)
          ch = true
        case (_: WireV, _: WireV) if s != t =>
          if (!isBoundary(s) || !isBoundary(t)) {
            g = g.collapseWire(e)
            ch = true
          }
        case _ => // do nothing
      }
    }

    for (w <- g.verts if g.vdata(w).isWireVertex) {
      val ine = g.inEdges(w)
      if (ine.size == 2) g = g.flipEdge(ine.head)
      else {
        val oute = g.outEdges(w)
        if (oute.size == 2) g = g.flipEdge(oute.head)
      }
    }

    g

//    if (ch) g.normalise
//    else g
  }

  def minimise: Graph = {
    val n = normalise
    n.verts.foldRight(n) { (v, g) =>
      if (g.vdata(v).isWireVertex) g.wireToEdge(v)
      else g
    }
  }

  /**
    * make a copy of the given bbox's contents, without copying the bbox itself
    * @param bb the bbox to be expanded
    * @return the new graph and a record containing relevant data for replaying the expansion
    */
  def expandBBox(bb: BBName, avoidV: Set[VName] = Set(), mp: GraphMap = GraphMap()): (Graph, BBExpand) = {
    if (bboxParent.domSet contains bb) throw new GraphException("Attempted to expand non-toplevel bbox")
    val g = fullSubgraph(contents(bb), bboxChildren(bb))

    var mp1 = g.makeRenaming(verts union avoidV, edges, bboxes, mp)
    var gfr = mp1.image(g)

    // add each expanded vertex to the bboxes that it was already in
    g.verts.foreach{v =>
      ((bboxesContaining(v) -- bboxChildren(bb)) - bb).foreach{ bb1 => gfr = gfr.addToBBox(mp1.v(v), bb1) }
    }

    var g1 = appendGraph(gfr)

    var freshE = g1.edges

    for (e <- adjacentEdges(g.verts) -- g.edges) {
      val s = source(e)
      val t = target(e)
      val e1 = freshE.freshWithSuggestion(e)
      freshE = freshE + e1
      mp1 = mp1.addEdge(e -> e1)
      g1 = g1.addEdge(e1, edata(e), mp1.v.getOrElse(s,s) -> mp1.v.getOrElse(t,t))
    }

    // TODO: fresh names

    (g1, BBExpand(bb,mp1,PFun()))
  }

  /**
    * make a copy of the given bbox
    * @param bb the bbox to be copied
    * @return the new graph and a record containing relevant data for replaying the copy
    */
  def copyBBox(bb: BBName, avoidV: Set[VName] = Set(), mp: GraphMap = GraphMap()): (Graph, BBCopy) = {
    var (g1, bbe) = expandBBox(bb, avoidV, mp)
    val mp1 = if (bbe.mp.bb.domSet contains bb) bbe.mp
              else bbe.mp.copy(bb = bbe.mp.bb + (bb -> g1.bboxes.freshWithSuggestion(bb)))
    val bb1 = mp1.bb(bb)
    g1 = g1.addBBox(bb1, bbdata(bb), mp1.v.codSet)

    for (bb2 <- mp1.bb.directImage(g1.bboxChildren(bb))) g1 = g1.setBBoxParent(bb2, Some(bb1))

    (g1, BBCopy(bb, mp1))
  }

  /**
    * drop the given bbox, keeping the contents intact
    * @param bb the bbox to be dropped
    * @return the new graph and a record containing relevant data for replaying the drop
    */
  def dropBBox(bb: BBName): (Graph, BBDrop) = {
    // TODO: fresh names
    (deleteBBox(bb), BBDrop(bb, PFun()))
  }

  /**
    * kill the given bbox, also deleting child nodes and bboxes
    * @param bb the bbox to be dropped
    * @return the new graph and a record containing relevant data for replaying the drop
    */
  def killBBox(bb: BBName): (Graph, BBKill) = {
    var g1 = this
    for (bb1 <- bboxChildren(bb)) g1 = g1.deleteBBox(bb1)
    g1 = g1.deleteVertices(contents(bb)).deleteBBox(bb)

    (g1, BBKill(bb))
  }

  private def freshMap(mp: PFun[VName, VName], avoid: Set[VName]) = {
    mp
  }

  /**
    * Apply the given bbox operation
    * @param bbop a !-box operation
    * @param avoidV an optional set of (extra) vertices to avoid when copying !-box contents
    * @return
    */
  def applyBBOp(bbop: BBOp, avoidV: Set[VName] = Set()): Graph = bbop match {
    case BBExpand(bb, mp, fresh) =>
      // TODO: fresh names
      val mp1 = GraphMap(v = mp.v.filterKeys(v => verts.contains(v) && isBoundary(v)), bb = mp.bb)
      expandBBox(bb, avoidV, mp1)._1
    case BBCopy(bb, mp) =>
      val mp1 = GraphMap(v = mp.v.filterKeys(v => verts.contains(v) && isBoundary(v)), bb = mp.bb)
      copyBBox(bb, avoidV, mp1)._1
    case BBDrop(bb, fresh) =>
      // TODO: fresh names
      dropBBox(bb)._1
    case BBKill(bb) => killBBox(bb)._1
  }

  def freeVars: Set[Var] = vdata.foldRight(Set[Var]()) {
    case ((_,d: NodeV), s) => s union d.angle.vars
    case (_, s) => s
  }

  def isWildBBox(bb: BBName): Boolean = {
    // TODO: check for bare wires as well
    // a wild !-box is a !-box which contains no vertices which are not in other !-boxes
    contents(bb).forall(v => bboxesContaining(v).size > 1)
  }

  def toJson(theory: Theory) : Json = Graph.toJson(this, theory)
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


  def fromAdjMat(amat: AdjMat, rdata: Vector[NodeV], gdata: Vector[NodeV]): Graph = {
    val thy =
      if (gdata.nonEmpty) gdata(0).theory
      else if (rdata.nonEmpty) rdata(0).theory
      else throw new GraphException("Must give at least one piece of node data")

    var g = Graph(thy)
    for (i <- 0 until amat.numBoundaries) g = g.addVertex(VName("v"+i), WireV(theory = thy))

    var i = amat.numBoundaries

    for (t <- 0 until amat.numRedTypes; v <- 0 until amat.red(t)) {
      g = g.addVertex(VName("v" + i), rdata(t))
      i += 1
    }

    for (t <- 0 until amat.numGreenTypes; v <- 0 until amat.green(t)) {
      g = g.addVertex(VName("v" + i), gdata(t))
      i += 1
    }

    val ed = UndirEdge(theory = thy, data = thy.defaultEdgeData)

    i = 0
    for (j <- 0 until amat.size; k <- 0 until j; if amat.mat(j)(k)) {
      g = g.addEdge(EName("e" + i), ed, VName("v" + j) -> VName("v" + k))
      i += 1
    }

    g
  }
}
