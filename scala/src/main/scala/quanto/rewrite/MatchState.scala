package quanto.rewrite
import quanto.data._

import scala.annotation.tailrec

case class MatchState(
                       m: Match,             // the match being built
                       tVerts: Set[VName],   // restriction of the range of the match in the target graph
                       angleMatcher: AngleExpressionMatcher,  // state of matched angle data
                       pNodes: Set[VName] = Set(),            // nodes with partially-mapped neighbourhood
                       psNodes: Set[VName] = Set()            // same, but scheduled for completion
                     ) {

  lazy val uNodes: Set[VName] = (m.pattern.verts -- m.vmap.domSet).filter { v => !m.pattern.vdata(v).isWireVertex }
  lazy val uWires: Set[VName] = (m.pattern.verts -- m.vmap.domSet).filter { v => m.pattern.vdata(v).isWireVertex }

  def matchPending(): Stream[MatchState] = {
    matchCircles().flatMap(_.matchMain())
  }

  // TODO: stub
  def matchCircles(): Stream[MatchState] =
    Stream(this)

  // TODO: should this be made tail-recursive?
  final def matchMain(rest: Stream[MatchState] = Stream()): Stream[MatchState] = {
    psNodes.headOption match {
      case Some(np) =>
        if (pVertexMayBeCompleted(np)) {
          val nt = m.vmap(np)
          m.pattern.adjacentEdges(np).find { e =>
            uWires.contains(m.pattern.edgeGetOtherVertex(e, np))
          } match {
            case Some(ep) =>
              m.target.adjacentEdges(nt).filter { e =>
                tVerts.contains(m.target.edgeGetOtherVertex(e,nt))
              }.foldRight(rest) { (et, rest1) =>
                matchNewWire(np,ep,nt,et) match {
                  case Some(ms1) => ms1.matchMain(rest1)
                  case None => rest1
                }
              }
            case None =>
              if (m.target.adjacentEdges(nt).forall(m.emap.codSet.contains))
                copy(pNodes = pNodes - np, psNodes = psNodes - np).matchMain(rest)
              else
                copy(psNodes = psNodes - np).matchMain(rest)
          }
        } else rest
      case None => uNodes.headOption match {
        case Some(np) =>
          val tNodes = tVerts.filter(!m.target.vdata(_).isWireVertex)
          tNodes.foldRight(rest) { (nt, rest1) =>
            matchNewNode(np, nt) match {
              case Some(ms1) => ms1.matchMain(rest1)
              case None => rest1
            }
          }
        case None =>
          if (m.isTotal) copy(m = m.copy(subst = angleMatcher.toMap)) #:: rest
          else rest
      }
    }
  }

  // TODO: stub
  def pVertexMayBeCompleted(v: VName) = true

  /**
    * Match a new node vertex
    *
    * (ported from the ML function match_new_nv)
    *
    * @param np node vertex in the pattern
    * @param nt node vertex in the target
    * @return
    */
  def matchNewNode(np: VName, nt: VName): Option[MatchState] = {
    (m.pattern.vdata(np), m.target.vdata(nt)) match {
      case (pd: NodeV, td: NodeV) =>
        if (pd.typ == td.typ) {
          if (pd.hasAngle)
            angleMatcher.addMatch(pd.angle, td.angle).map { angleMatcher1 =>
              copy(
                m = m.addVertex(np -> nt),
                pNodes = pNodes + np,
                psNodes = psNodes + np,
                tVerts = tVerts - nt,
                angleMatcher = angleMatcher1
              )
            }
          else if (pd.value == td.value)
            Some(copy(
              m = m.addVertex(np -> nt),
              pNodes = pNodes + np,
              psNodes = psNodes + np,
              tVerts = tVerts - nt
            ))
          else None
        } else None
      case _ => throw new MatchException("matchNewNode called on a non-node")
    }
  }

  /**
   * Try to recursively add wire to matching, starting with the given head
   * vertex and edge. Return NONE on failure.
   *
   * (ported from the ML function tryadd_wire)
   *
   * @param vp already-matched vertex
   * @param ep unmatched edge incident to vp (other end must be in P, Uw or Un)
   * @param vt target of vp
   * @param et unmatched edge incident to vt
   */
  def matchNewWire(vp:VName, ep:EName, vt:VName, et:EName): Option[MatchState] = {
    val pdir = m.pattern.edata(ep).isDirected
    val tdir = m.target.edata(et).isDirected
    val pOutEdge = m.pattern.source(ep) == vp
    val tOutEdge = m.target.source(et) == vt

    // match directedness and, if the edge is directed, direction
    if ((pdir && tdir && pOutEdge == tOutEdge) || (!pdir && !tdir)) {
      val newVp = m.pattern.edgeGetOtherVertex(ep, vp)
      val newVt = m.target.edgeGetOtherVertex(et, vt)

      if (pNodes contains newVp) {
        if (m.vmap contains (newVp -> newVt))
          Some(copy(psNodes = psNodes + newVp, m = m.addEdge(ep -> et)))
        else None
      } else if (tVerts contains newVt) {
        (m.pattern.vdata(newVp), m.target.vdata(newVt)) match {
          case (_: WireV, _: WireV) =>
            (m.pattern.wireVertexGetOtherEdge(newVp, ep), m.target.wireVertexGetOtherEdge(newVt, et)) match {
              case (Some(newEp), Some(newEt)) =>
                copy(
                  m = m.addEdge(ep -> et, newVp -> newVt),
                  tVerts = tVerts - newVt
                ).matchNewWire(newVp, newEp, newVt, newEt)
              case (Some(_), None) => None
              case (None, _) =>
                Some(copy(
                  m = m.addEdge(ep -> et, newVp -> newVt),
                  tVerts = tVerts - newVt
                ))
            }
          case (_: NodeV, _: NodeV) =>
            if (uNodes contains newVp) {
              matchNewNode(newVp, newVt).map { ms => copy(m = ms.m.addEdge(ep -> et)) }
            } else None
          case _ => None
        }
      } else None
    } else None
  }

}
