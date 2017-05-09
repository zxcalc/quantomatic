package quanto.rewrite
import quanto.data._

class MatchException(msg: String) extends Exception(msg)

sealed abstract class BBOp

case class Match(pattern: Graph,
                 target: Graph,
                 vmap: PFun[VName,VName] = PFun(),
                 emap: PFun[EName,EName] = PFun(),
                 bbmap: PFun[BBName,BBName] = PFun(),
                 bbops: List[BBOp] = List(),
                 subst: Map[String,AngleExpression] = Map()) {

  def addVertex(vPair: (VName, VName)): Match = {
    if (vmap.domSet contains vPair._1)
      throw new MatchException("Attempted to re-map vertex: " + vPair._1)

    copy(vmap = vmap + vPair)
  }

  def addEdge(ePair: (EName, EName)): Match = {
    if (emap.domSet contains ePair._1)
      throw new MatchException("Attempted to re-map edge: " + ePair._1)

    copy(emap = emap + ePair)
  }

  def addEdge(ePair: (EName, EName), vPair: (VName, VName)): Match = {
    if (!(vPair._1 == pattern.source(ePair._1) || vPair._1 == pattern.target(ePair._1)) ||
        !(vPair._2 == target.source(ePair._2)  || vPair._2 == target.target(ePair._2)))
      throw new MatchException("Attempted to add edges: " + ePair + " to unconnected vertices: " + vPair)
    if (vmap.domSet contains vPair._1)
      throw new MatchException("Attempted to re-map vertex: " + vPair._1)
    if (emap.domSet contains ePair._1)
      throw new MatchException("Attempted to re-map edge: " + ePair._1)

    copy(vmap = vmap + vPair, emap = emap + ePair)
  }

  // TODO: bbox check
  def isHomomorphism =
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

  def isTotal: Boolean =
    vmap.domSet == pattern.verts &&
    emap.domSet == pattern.edges &&
    bbmap.domSet == pattern.bboxes
}
