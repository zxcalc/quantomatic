package quanto.data

import Names._
import quanto.util.json._
import math.sqrt
import JsonValues._
import collection.mutable.ArrayBuffer
import quanto.util._

trait GraphException extends Exception

class SafeDeleteVertexException(name: VName, reason: String) extends
Exception("Unable to safely delete " + name + ", because " + reason)
with GraphException

class DanglingEdgeException(edge: EName, endPoint: VName) extends
Exception("Edge: " + edge + " has no endpoint: " + endPoint + " in graph")
with GraphException

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
  def inputs  = verts.filter(isInput)
  def outputs = verts.filter(isOutput)

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

  def canEqual(other: Any) = other.isInstanceOf[Graph]

  override def equals(other: Any) = other match {
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
  def inEdges(vn: VName) = target.codf(vn)
  def outEdges(vn: VName) = source.codf(vn)
  def predVerts(vn: VName) = inEdges(vn).map(source(_))
  def succVerts(vn: VName) = outEdges(vn).map(target(_))
  def contents(bbn: BBName) = inBBox.codf(bbn)

  def adjacentEdges(vn: VName) = source.codf(vn) union target.codf(vn)

  def edgesBetween(v1: VName, v2: VName) = adjacentEdges(v1) intersect adjacentEdges(v2)

  def isBBoxed(v: VName) = !inBBox.domf(v).isEmpty

  /// by song
  // to compute whether two vertices are in the same bbox.
  def isInSameBBox(v1:VName, v2:VName) = !(inBBox.domf(v1) & inBBox.domf(v2)).isEmpty

  def addVertex(vn: VName, data: VData) = {
    if (vdata contains vn)
      throw new DuplicateVertexNameException(vn) with GraphException

    copy(vdata = vdata + (vn -> data))
  }

  def newVertex(data: VData) = {
    val vn = vdata.fresh
    (addVertex(vn, data), vn)
  }

  def addEdge(en: EName, data: EData, vns: (VName, VName)) = {
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

  def newEdge(data: EData, vns: (VName, VName)) = {
    val en = edata.fresh
    (addEdge(en, data, vns), en)
  }

  def addBBox(bbn: BBName, data: BBData, contents: Set[VName] = Set[VName](), parent: Option[BBName] = None) = {
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

  /** Replace the contents of a bang box with new ones */
  def updateBBoxContents(bbn: BBName, newContents: Set[VName]) = {
    var rel = inBBox.unmapCod(bbn)
    newContents.foreach {v =>
      rel += (v,bbn)
    }
    copy( inBBox = rel )
  }

  def newBBox(data: BBData, contents: Set[VName] = Set[VName](), parent: Option[BBName] = None) = {
    val bbn = bbdata.fresh
    (addBBox(bbn, data, contents, parent), bbn)
  }

  def deleteBBox(bb: BBName) = {
    copy(
      bbdata = bbdata - bb,
      inBBox = inBBox.unmapCod(bb),
      bboxParent = bboxParent.unmapDom(bb)
    )
  }

  def deleteEdge(en: EName) = {
    copy(
      edata = edata - en,
      source = source.unmapDom(en),
      target = target.unmapDom(en)
    )
  }

  def safeDeleteVertex(vn: VName) = {
    if (source.codf(vn).isEmpty && target.codf(vn).isEmpty)
      copy(vdata = vdata - vn, inBBox = inBBox.unmapDom(vn))
    else throw new SafeDeleteVertexException(vn, "vertex has adjancent edges")
  }

  def deleteVertex(vn: VName) = {
    var g = this
    for (e <- source.codf(vn)) g = g.deleteEdge(e)
    for (e <- target.codf(vn)) g = g.deleteEdge(e)
    g.inBBox.domf(vn).foreach { bbname =>
      if (g.inBBox.codf(bbname).size == 1) g = g.deleteBBox(bbname)
      else g = g.copy( inBBox = g.inBBox.unmap(vn, bbname))
    }
    g.copy(vdata = vdata - vn)
  }

  // data updaters
  def updateData(f: GData => GData)                  = copy(data = f(data))
  def updateVData(vn: VName)(f: VData => VData)      = copy(vdata = vdata + (vn -> f(vdata(vn))))
  def updateEData(en: EName)(f: EData => EData)      = copy(edata = edata + (en -> f(edata(en))))
  def updateBBData(bbn: BBName)(f: BBData => BBData) = copy(bbdata = bbdata + (bbn -> f(bbdata(bbn))))

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


  private def bbDft(bb : BBName, bbSeq : collection.mutable.Buffer[BBName], bbs : collection.mutable.Set[BBName]) {
    for (ch <- bboxParent.codf(bb)) bbDft(ch, bbSeq, bbs)
    if (bbs.contains(bb)) {
      bbs.remove(bb)
      bbSeq += bb
    }
  }

  def bboxesChildrenFirst = {
    val bbSeq = collection.mutable.Buffer[BBName]()
    val bbs = collection.mutable.Set[BBName](bboxes.toSeq : _*)
    while (!bbs.isEmpty) bbDft(bbs.iterator.next(),bbSeq,bbs)

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
  implicit def qGraphAndNameToQGraph[N <: Name[N]](t: (Graph, Name[N])) : Graph = t._1

  def apply(theory: Theory): Graph = Graph(data = GData(theory = theory))

  
  def fromJson(s: String, thy: Theory): Graph =
    try   { fromJson(Json.parse(s), thy) }
    catch { case e:JsonParseException => throw new GraphLoadException("Error parsing JSON", e) }


  def fromJson(s: String): Graph = fromJson(s, Theory.DefaultTheory)

  def fromJson(json: Json, thy: Theory = Theory.DefaultTheory): Graph = try {
    Function.chain[Graph](Seq(

      (json ?# "wire_vertices").foldLeft(_) { (g,v) =>
        g.addVertex(v._1, WireV.fromJson(v._2, thy))
      },

      (json ?# "node_vertices").foldLeft(_) { (g,v) =>
        g.addVertex(v._1, NodeV.fromJson(v._2, thy))
      },

      (json ?# "dir_edges").foldLeft(_) { (g,e) =>
        val data = e._2.getOrElse("data", thy.defaultEdgeData).asObject
        val annotation = e._2 ?# "annotation"
        g.addEdge(e._1, DirEdge(data, annotation, thy),
          (e._2("src").stringValue, e._2("tgt").stringValue))
      },

      (json ?# "undir_edges").foldLeft(_) { (g,e) =>
        val data = e._2.getOrElse("data", thy.defaultEdgeData).asObject
        val annotation = e._2 ?# "annotation"
        g.addEdge(e._1, UndirEdge(data, annotation, thy), (e._2("src").stringValue, e._2("tgt").stringValue))
      },

      (json ?# "bang_boxes").foldLeft(_) { (g,bb) =>
        val data = bb._2 ?# "data"
        val annotation = bb._2 ?# "annotation"
        val contains = (bb._2 ?@ "contains") map { VName(_) }
        val parent = bb._2.get("parent") map { BBName(_) }
        g.addBBox(bb._1, BBData(data, annotation), contains.toSet, parent)
      }

    ))({
      val data = json ?# "data"
      val annotation = json ?# "annotation"
      Graph(GData(data, annotation, thy))
    })
  } catch {
    case e: JsonAccessException => throw new GraphLoadException("Error reading JSON", e)
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
          "contains"   -> JsonArray(graph.contents(bb)),
          "parent"     -> (graph.bboxParent.get(bb) match {
            case Some(p) => JsonString(p.toString)
            case None    => JsonNull() }),
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

  def random(nverts: Int, nedges: Int, nbboxes: Int = 0) = {
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
      for(j <- 1 to nedges if !sources.isEmpty && !targets.isEmpty) {
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
          s + varray(rand.nextInt(varray.size))
        }

        randomGraph = randomGraph.newBBox(BBData(), randomVSet, None)
      }
    }

    randomGraph
  }

  def randomDag(nverts: Int, nedges: Int) = {
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
        val x = rand.nextInt(varray.size)
        val y = rand.nextInt(varray.size - 1)
        val s = varray(x)
        val t = varray(if (y >= x) y+1 else y)
        randomGraph = randomGraph.newEdge(DirEdge(),
          if (s <= t) (s,t) else (t,s)
        )
      }

    randomGraph
  }
}