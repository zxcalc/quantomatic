package quanto.rewrite
import quanto.data._

class MatchException(msg: String) extends Exception(msg)

sealed abstract class BBOp

case class Match(pattern: Graph,
                 patternExpanded: Graph,
                 target: Graph,
                 vmap: PFun[VName,VName] = PFun(),
                 emap: PFun[EName,EName] = PFun(),
                 bareWireMap: Map[VName, List[VName]] = Map(),
                 bbmap: PFun[BBName,BBName] = PFun(),
                 bbops: List[BBOp] = List(),
                 subst: Map[String,AngleExpression] = Map()) {

  def addVertex(vPair: (VName, VName)): Match = {
    copy(vmap = vmap + vPair)
  }

  def addEdge(ePair: (EName, EName)): Match = {
    copy(emap = emap + ePair)
  }

  def addEdge(ePair: (EName, EName), vPair: (VName, VName)): Match = {
    if (!(vPair._1 == pattern.source(ePair._1) || vPair._1 == pattern.target(ePair._1)) ||
        !(vPair._2 == target.source(ePair._2)  || vPair._2 == target.target(ePair._2)))
      throw new MatchException("Attempted to add edges: " + ePair + " to unconnected vertices: " + vPair)

    copy(vmap = vmap + vPair, emap = emap + ePair)
  }

  /**
    * create a new match by expanding any bare wires in the target, as listed in bareWireMap. Note this changes the
    * target, but in a reasonably predictable way.
    *
    * @return the normalised match
    */
  def normalize: Match = {
    bareWireMap.headOption match {
      case Some((tw, pw :: pws)) =>
        val (target1, (newW, newE)) = target.expandWire(tw)
        val emap1 = emap + (pattern.outEdges(pw).head -> newE)

        var vmap1 = vmap + (pattern.succVerts(pw).head -> newW)
        for (pw1 <- vmap.codf(tw)) {
          if (pw1 != pw && pattern.isInput(pw1)) vmap1 = vmap1 + (pw1 -> newW)
        }
        copy(
          vmap = vmap1, emap = emap1,
          target = target1,
          bareWireMap = bareWireMap + (tw -> pws)
        ).normalize
      case Some((_, List())) => copy(bareWireMap = bareWireMap.tail).normalize
      case None => this
    }
  }

  def isNormalized: Boolean = bareWireMap.isEmpty

  // TODO: bbox check
  def isHomomorphism: Boolean =
    if (!isNormalized) normalize.isHomomorphism
    else {
      emap.forall { case (ep, et) =>
        (pattern.edata(ep).isDirected && target.edata(et).isDirected &&
          vmap.get(pattern.source(ep)).contains(target.source(et)) &&
          vmap.get(pattern.target(ep)).contains(target.target(et))
          ) ||
          (!pattern.edata(ep).isDirected && !target.edata(et).isDirected &&
            (
              (
                vmap.get(pattern.source(ep)).contains(target.source(et)) &&
                  vmap.get(pattern.target(ep)).contains(target.target(et))
                ) ||
                (
                  vmap.get(pattern.source(ep)).contains(target.target(et)) &&
                    vmap.get(pattern.target(ep)).contains(target.source(et))
                  )
              ))
      }
    }

  def isTotal: Boolean =
    if (!isNormalized) normalize.isTotal
    else {
      vmap.domSet == pattern.verts &&
      emap.domSet == pattern.edges &&
      bbmap.domSet == pattern.bboxes
    }
}
