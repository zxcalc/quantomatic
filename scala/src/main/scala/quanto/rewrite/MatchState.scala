package quanto.rewrite
import quanto.data._

import scala.annotation.tailrec

case class MatchState(
                       m: Match,                                        // the match being built
                       tVerts: Set[VName],                              // restriction of the range of the match
                       angleMatcher: AngleExpressionMatcher,            // state of matched angle data
                       pNodes: Set[VName] = Set(),                      // nodes with partially-mapped neighbourhood
                       psNodes: Set[VName] = Set(),                     // same, but scheduled for completion
                       sBBox: Option[BBName] = None,                    // a bbox scheduled for matching
                       candidateNodes: Option[Set[VName]] = None,       // nodes to try matching in the target
                       candidateEdges: Option[Set[EName]] = None,       // edges to try matching in the target
                       candidateWires: Option[Set[(VName,Int)]] = None, // wire-vertices to try matching bare wires on
                       candidateBBoxes: Option[Set[BBName]] = None,     // bboxes to try matching in the target
                       nextState: Option[MatchState] = None             // next state to try after search terminates
                     ) {

  val uVerts: Set[VName]          = m.pattern.verts.filter(v => bboxesMatched(v) && !m.map.v.domSet.contains(v))
  val uCircles: Set[VName]        = uVerts.filter(m.pattern.isCircle)
  lazy val uBareWires: Set[VName] = uVerts.filter(m.pattern.representsBareWire)
  lazy val uNodes: Set[VName]     = uVerts.filter { v => !m.pattern.vdata(v).isWireVertex }
  lazy val uWires: Set[VName]     = uVerts.filter { v => m.pattern.vdata(v).isWireVertex }
  val uBBoxes: Set[BBName]   = m.pattern.bboxes.filter(bb => parentBBoxesMatched(bb) && !m.map.bb.domSet.contains(bb))



  /**
    * This is the main match loop. It maintains its own stack (via nextState), so it can be tail-recursive.
    *
    * @return the next match and if there could be more matches, the next MatchState in the search tree
    */
  @tailrec
  final def nextMatch(): Option[(Match, Option[MatchState])] = {
    // if unmatched circles are found in the pattern, match them first
    if (uCircles.nonEmpty) {
      val pc = uCircles.min

      tVerts.find(v => m.target.isCircle(v) && reflectsBBoxes(pc, v)) match {
        case None => nextState match { case Some(next) => next.nextMatch(); case None => None }
        case Some(tc) =>
          val pce = m.pattern.inEdges(pc).min
          val tce = m.target.inEdges(tc).min
          copy(m = m.addEdge(pce -> tce, pc -> tc), tVerts = tVerts - tc).nextMatch()
      }

    // if there is a scheduled node, try to match its neighbourhood in every possible way
    } else if (psNodes.nonEmpty) {
      val np = psNodes.min

      if (pVertexMayBeCompleted(np)) {
        val nt = m.map.v(np)
        // get the next matchable edge in the neighbourhood of np
        val uEdges = m.pattern.adjacentEdges(np).filter(e =>
          !m.map.e.domSet.contains(e) &&
          bboxesMatched(m.pattern.edgeGetOtherVertex(e, np))
        )
        val epOpt = if (uEdges.isEmpty) None else Some(uEdges.min)
        epOpt match {
          // if there is an matchable edge in nhd(np), try to match it in every possible way
          // to an edge in the neighbourhood of nt
          case Some(ep) =>
            candidateEdges match {
              case None =>
                copy(candidateEdges = Some(m.target.adjacentEdges(nt).filter { e =>
                  !m.map.e.codSet.contains(e) &&
                  tVerts.contains(m.target.edgeGetOtherVertex(e, nt))
                })).nextMatch()
              case Some(candidateEdges1) =>
                if (candidateEdges1.isEmpty) {
                  nextState match {
                    case Some(ms1) => ms1.nextMatch()
                    case None => None
                  }
                } else {
                  val et = candidateEdges1.min
                  val next = copy(candidateEdges = Some(candidateEdges1 - et))
                  matchNewWire(np, ep, nt, et) match {
                    case Some(ms1) => ms1.copy(candidateEdges = None, nextState = Some(next)).nextMatch()
                    case None => next.nextMatch()
                  }
                }
            }
          // If there are no matchable edges in nhd(np), de-schedule np. If emap is now surjective on nhd(nt),
          // then additionally mark np as done.
          case None =>
            if (m.target.adjacentEdges(nt).forall(m.map.e.codSet.contains))
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
      val np = uNodes.min
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
            val nt = candidateNodes1.min
            val next = copy(candidateNodes = Some(candidateNodes1 - nt))
            matchNewNode(np, nt) match {
              case Some(ms1) => ms1.copy(candidateNodes = None, nextState = Some(next)).nextMatch()
              case None => next.nextMatch()
            }
          }
      }

    // if there are bare wires remaining, add them in all possible ways
    } else if (uBareWires.nonEmpty) {
      val pbw = uBareWires.min
      candidateWires match {
        case None =>
          // pull all the candidate locations for matching this bare wire. If a wire already has n bare wires matched
          // on it, this is n+1 possible locations.
          val cwires =
            for (v <- tVerts if m.target.representsWire(v) && reflectsBBoxes(pbw, v);
                 i <- 0 to m.bareWireMap.get(v).map(_.length).getOrElse(0))
              yield (v,i)
          copy(candidateWires = Some(cwires.toSet)).nextMatch()
        case Some(candidateWires1) =>
          if (candidateWires1.isEmpty) {
            nextState match {
              case Some(ms1) => ms1.nextMatch()
              case None => None
            }
          } else {
            val tbw = candidateWires1.head
            val next = copy(candidateWires = Some(candidateWires1 - tbw))
            val wireV: Vector[VName] = m.bareWireMap.getOrElse(tbw._1, Vector())
            val newMap = m.bareWireMap + (tbw._1 -> ((wireV.take(tbw._2) :+ pbw) ++
              wireV.takeRight(wireV.length - tbw._2)))
            copy(
              m = m.addVertex(pbw -> tbw._1).copy(bareWireMap = newMap),
              candidateWires = None,
              nextState = Some(next)).nextMatch()
          }
      }

    // if all matchable verts are matched, pull the first top-level bbox and try to kill, expand, or copy+match
    } else if (uBBoxes.nonEmpty && uVerts.isEmpty) {
      val pbb = uBBoxes.min
      candidateBBoxes match {
        case Some(candidateBBoxes1) =>
          if (candidateBBoxes1.isEmpty) {
            nextState match {
              case Some(ms1) => ms1.nextMatch()
              case None => None
            }
          } else {
            val tbb = candidateBBoxes1.min
            val next = copy(candidateBBoxes = Some(candidateBBoxes1 - tbb))
//            val schedule =
//              m.pattern.adjacentVerts(m.pattern.contents(pbb)).filter { v =>
//                !m.pattern.vdata(v).isWireVertex &&
//                bboxesMatched(v)
//              }
            copy(
              m = m.addBBox(pbb -> tbb),
              psNodes = pNodes, // re-schedule everything
              candidateBBoxes = None,
              nextState = Some(next)
            ).nextMatch()
          }
        case None =>
          val (killGraph, killOp) = m.pattern.killBBox(pbb)

          val next1 =
            if (m.pattern.isWildBBox(pbb)) nextState
            else { // only expand/copy non-wild !-boxes, or we'll get infinite matchings
              val (expandGraph, expandOp) = m.pattern.expandBBox(pbb)
              val (copyGraph, copyOp) = m.pattern.copyBBox(pbb)

              val expanded = m.bbops.exists { case BBExpand(bb1, _) => bb1 == pbb; case _ => false }

              // only copy bboxes that have never been expanded
              val next2 =
                if (expanded) nextState
                else
                  Some(copy(
                    m = m.copy(pattern = copyGraph, bbops = copyOp :: m.bbops),
                    candidateBBoxes = Some(m.target.bboxes.filter { tbb => reflectsParentBBoxes(pbb, tbb)
                    })))

              //          val schedule =
              //            (expandGraph.adjacentVerts(expandGraph.contents(pbb)) ++
              //             expandOp.mp.v.directImage(m.pattern.contents(pbb)))
              //              .filter{ v =>
              //                !expandGraph.vdata(v).isWireVertex &&
              //                bboxesMatched(v)
              //              }

              Some(copy(
                m = m.copy(pattern = expandGraph, bbops = expandOp :: m.bbops),
                psNodes = pNodes, // re-schedule everything
                nextState = next2
              ))
            }


          copy(
            m = m.copy(pattern = killGraph, bbops = killOp :: m.bbops),
            nextState = next1
          ).nextMatch()
      }

    // if there is nothing left to do, check if the match is complete and return it if so. If not, continue
    // the search from nextState
    } else {
      if (pNodes.isEmpty && m.isTotal) {
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

  // TODO: we may only need to check the closest parent, not all parents

  def reflectsBBoxes(vp: VName, vt: VName) =
    m.map.bb.directImage(m.pattern.bboxesContaining(vp)) == m.target.bboxesContaining(vt)

  def bboxesMatched(vp: VName) =
    m.pattern.bboxesContaining(vp).forall(m.map.bb.domSet.contains)

  def reflectsParentBBoxes(bbp: BBName, bbt: BBName) =
    m.map.bb.directImage(m.pattern.bboxParents(bbp)) == m.target.bboxParents(bbt)

  def parentBBoxesMatched(bbp: BBName) = {
    m.pattern.bboxParents(bbp).forall(m.map.bb.domSet.contains)
  }

  /**
    * Match a new node vertex
    *
    * (ported from the ML function match_new_nv)
    *
    * @param np node vertex in the pattern
    * @param nt node vertex in the target
    * @return
    */
  def matchNewNode(np: VName, nt: VName): Option[MatchState] =
    if (!reflectsBBoxes(np, nt)) None
    else
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
        if (m.map.v contains (newVp -> newVt))
          Some(copy(psNodes = psNodes + newVp, m = m.addEdge(ep -> et)))
        else None
      } else if (tVerts contains newVt) {
        (m.pattern.vdata(newVp), m.target.vdata(newVt)) match {
          case (_: WireV, _: WireV) if reflectsBBoxes(newVp, newVt) =>
            (m.pattern.wireVertexGetOtherEdge(newVp, ep), m.target.wireVertexGetOtherEdge(newVt, et)) match {
              case (Some(newEp), Some(newEt)) =>
                copy(
                  m = m.addEdge(ep -> et, newVp -> newVt),
                  tVerts = tVerts - newVt
                ).matchNewWire(newVp, newEp, newVt, newEt)
              case (Some(_), None) => None
              case (None, _) =>
                Some(copy(
                  m = m.addEdge(ep -> et, newVp -> newVt)
                ))
            }
          case (_: NodeV, _: NodeV) =>
            if (uNodes contains newVp) {
              matchNewNode(newVp, newVt).map { ms => ms.copy(m = ms.m.addEdge(ep -> et)) }
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
