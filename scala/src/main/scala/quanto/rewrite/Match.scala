package quanto.rewrite
import quanto.data._

class MatchException(msg: String) extends Exception(msg)

case class Match(pattern0: Graph, // the pattern without bbox operations
                 pattern: Graph,
                 target: Graph,
                 map: GraphMap = GraphMap(),
                 bareWireMap: Map[VName, Vector[VName]] = Map(),
                 bbops: List[BBOp] = List(),
                 subst: Map[Var,AngleExpression] = Map()) {

  def addVertex(vPair: (VName, VName)): Match = {
    copy(map = map addVertex vPair)
  }

  def addEdge(ePair: (EName, EName)): Match = {
    copy(map = map addEdge ePair)
  }

  def addEdge(ePair: (EName, EName), vPair: (VName, VName)): Match = {
    if (!(vPair._1 == pattern.source(ePair._1) || vPair._1 == pattern.target(ePair._1)) ||
        !(vPair._2 == target.source(ePair._2)  || vPair._2 == target.target(ePair._2)))
      throw new MatchException("Attempted to add edges: " + ePair + " to unconnected vertices: " + vPair)

    copy(map = map addVertex vPair addEdge ePair)
  }

  def addBBox(bbPair: (BBName, BBName)): Match = {
    copy(map = map addBBox bbPair)
  }

  /**
    * create a new match by expanding any bare wires in the target, as listed in bareWireMap. Note this changes the
    * target, but in a reasonably predictable way.
    *
    * @return the normalised match
    */
  def normalize: Match = {
    bareWireMap.headOption match {
      case Some((tw, pw +: pws)) =>
        val (target1, (newW1, newW2, newE)) = target.expandWire(tw)
        val emap1 = map.e + (pattern.outEdges(pw).head -> newE)

        var vmap1 = map.v
        for (pw1 <- map.v.codf(tw)) {
          if (pattern.isInput(pw1)) vmap1 = vmap1 + (pw1 -> newW2)
        }
        vmap1 = vmap1 + (pw -> newW1) + (pattern.succVerts(pw).head -> newW2)

        copy(
          map = map.copy(v = vmap1, e = emap1),
          target = target1,
          bareWireMap = bareWireMap + (tw -> pws)
        ).normalize
      case Some((_, Vector())) => copy(bareWireMap = bareWireMap.tail).normalize
      case None => this
    }
  }

//  def freshenPattern: Match = {
//    val (p1, rnv, rne, rnbb) = pattern.renameAvoiding1(target)
//
//    copy(
//      pattern = p1,
//      vmap = vmap.foldRight(PFun[VName,VName]()) { case ((p,t),mp) => mp + (rnv(p) -> t) },
//      emap = emap.foldRight(PFun[EName,EName]()) { case ((p,t),mp) => mp + (rne(p) -> t) },
//      bbmap = bbmap.foldRight(PFun[BBName,BBName]()) { case ((p,t),mp) => mp + (rnbb(p) -> t) }
//    )
//  }

  def isNormalized: Boolean = bareWireMap.isEmpty

  def isHomomorphism: Boolean =
    if (!isNormalized) normalize.isHomomorphism
    else map.isHomomorphism(pattern, target)

  def isInjective: Boolean =
    if (!isNormalized) normalize.isInjective
    else map.isInjective

  def isTotal: Boolean =
    if (!isNormalized) normalize.isTotal
    else map.isTotal(pattern)
}
