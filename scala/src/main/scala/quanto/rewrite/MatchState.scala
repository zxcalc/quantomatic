package quanto.rewrite
import quanto.data._

import scala.annotation.tailrec

case class MatchState(
                       m: Match,                                  // the match being built
                       tVerts: Set[VName],                        // restriction of the range of the match
                       angleMatcher: AngleExpressionMatcher,      // state of matched angle data
                       pNodes: Set[VName] = Set(),                // nodes with partially-mapped neighbourhood
                       psNodes: Set[VName] = Set(),               // same, but scheduled for completion
                       candidateNodes: Option[Set[VName]] = None, // nodes to try matching in the target
                       candidateEdges: Option[Set[EName]] = None, // edges to try matching in the target
                       candidateWires: Option[Set[VName]] = None, // wire-vertices to try matching bare wires on
                       nextState: Option[MatchState] = None       // next state to try after search terminates
                     ) {

  val uVerts: Set[VName]   = m.pattern.verts -- m.vmap.domSet
  val uCircles: Set[VName] = uVerts.filter(m.pattern.isCircle)
  lazy val uNodes: Set[VName] = uVerts.filter { v => !m.pattern.vdata(v).isWireVertex }
  lazy val uWires: Set[VName] = uVerts.filter { v => m.pattern.vdata(v).isWireVertex }
  lazy val uBareWires: Set[VName] =
    uVerts.filter { v =>
      m.pattern.isBoundary(v) &&
      (m.pattern.succVerts(v).headOption match {
        case Some(v1) => m.pattern.isBoundary(v1)
        case None => false
      })
    }



  /**
    * This is the main match loop. It maintains its own stack (via nextState), so it can be tail-recursive.
    *
    * @return the next match and if there could be more matches, the next MatchState in the search tree
    */
  @tailrec
  final def nextMatch(): Option[(Match, Option[MatchState])] = {
    // if unmatched circles are found in the pattern, match them first
    if (uCircles.nonEmpty) {
      val tCircles = tVerts.filter(m.target.isCircle)
      if (uCircles.size > tCircles.size) nextState match { case Some(next) => next.nextMatch(); case None => None }
      else {
        copy(m = uCircles.zip(tCircles).foldRight(m) { case ((pc, tc), m1) =>
          val pce = m.pattern.inEdges(pc).head
          val tce = m.target.inEdges(tc).head
          m.addEdge(pce -> tce, pc -> tc)
        }).nextMatch()
      }

    // if there is a scheduled node, try to match its neighbourhood in every possible way
    } else if (psNodes.nonEmpty) {
      val np = psNodes.head

      if (pVertexMayBeCompleted(np)) {
        val nt = m.vmap(np)
        // get the next matchable edge in the neighbourhood of np
        val epOpt = m.pattern.adjacentEdges(np).find { e => uWires.contains(m.pattern.edgeGetOtherVertex(e, np)) }
        epOpt match {
          // if there is an matchable edge in nhd(np), try to match it in every possible way
          // to an edge in the neighbourhood of nt
          case Some(ep) =>
            candidateEdges match {
              case None =>
                copy(candidateEdges = Some(m.target.adjacentEdges(nt).filter { e =>
                  !m.emap.codSet.contains(e) &&
                  tVerts.contains(m.target.edgeGetOtherVertex(e, nt))
                })).nextMatch()
              case Some(candidateEdges1) =>
                if (candidateEdges1.isEmpty) {
                  nextState match {
                    case Some(ms1) => ms1.nextMatch()
                    case None => None
                  }
                } else {
                  val next = copy(candidateEdges = Some(candidateEdges1.tail))
                  matchNewWire(np, ep, nt, candidateEdges1.head) match {
                    case Some(ms1) => ms1.copy(candidateEdges = None, nextState = Some(next)).nextMatch()
                    case None => next.nextMatch()
                  }
                }
            }
          // If there are no matchable edges in nhd(np), de-schedule np. If emap is now surjective on nhd(nt),
          // then additionally mark np as done.
          case None =>
            if (m.target.adjacentEdges(nt).forall(m.emap.codSet.contains))
              copy(pNodes = pNodes - np, psNodes = psNodes - np).nextMatch()
            else
              copy(psNodes = psNodes - np).nextMatch()
        }
      } else {
        // any match mapping np -> nt is doomed, so continue to the next possibility
        nextState match {
          case Some(ms1) => ms1.nextMatch()
          case None => None
        }
      }


    // if there are no scheduled nodes, pick a new unmatched node in the pattern, match it in every possible way
    // and schedule its neighbourhood for matching
    } else if (uNodes.nonEmpty) {
      val np = uNodes.head
      candidateNodes match {
        case None =>
          copy(candidateNodes = Some(tVerts.filter { v =>
            !m.target.vdata(v).isWireVertex
          })).nextMatch()
        case Some(candidateNodes1) =>
          if (candidateNodes1.isEmpty) {
            nextState match {
              case Some(ms1) => ms1.nextMatch()
              case None => None
            }
          } else {
            val next = copy(candidateNodes = Some(candidateNodes1.tail))
            matchNewNode(np, candidateNodes1.head) match {
              case Some(ms1) => ms1.copy(candidateNodes = None, nextState = Some(next)).nextMatch()
              case None => next.nextMatch()
            }
          }
      }

    // if there is nothing left to do, check if the match is complete and return it if so. If not, continue
    // the search from nextState
    } else {
      if (m.isTotal) {
        if (MatchState.countMatches) MatchState.matchCounter += 1
        val ms = copy(m = m.copy(subst = angleMatcher.toMap))
        Some((ms.m, nextState))
      } else {
        nextState match {
          case Some(ms1) => ms1.nextMatch()
          case None => None
        }
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
                  m = m.addEdge(ep -> et, newVp -> newVt)
                ).matchNewWire(newVp, newEp, newVt, newEt)
              case (Some(_), None) => None
              case (None, _) =>
                Some(copy(
                  m = m.addEdge(ep -> et, newVp -> newVt)
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

object MatchState {
  // for testing e.g. laziness in a single thread
  private var matchCounter = 0
  private var countMatches = false

  def startCountingMatches() = {
    matchCounter = 0
    countMatches = true
  }

  def matchCount(): Int = {
    countMatches = false
    matchCounter
  }
}
