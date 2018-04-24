package quanto.rewrite

import quanto.data._

import scala.annotation.tailrec

case class MatchState(
                       m: Match, // the match being built
                       targetVertices: Set[VName], // restriction of the range of the match
                       expressionMatcher: CompositeExpressionMatcher, // state of matched angle data
                       pNodes: Set[VName] = Set(), // nodes with partially-mapped neighbourhood
                       psNodes: Set[VName] = Set(), // same, but scheduled for completion
                       sBBox: Option[BBName] = None, // a bbox scheduled for matching
                       candidateNodes: Option[Set[VName]] = None, // nodes to try matching in the target
                       candidateEdges: Option[Set[EName]] = None, // edges to try matching in the target
                       candidateWires: Option[Set[(VName, Int)]] = None, // wire-vertices to try matching bare wires on
                       candidateBBoxes: Option[Set[BBName]] = None, // bboxes to try matching in the target
                       bboxOrbits: PFun[VName, VName] = PFun(), // for smashing redundant matches
                       nextState: Option[MatchState] = None // next state to try after search terminates
                     ) {

  lazy val uBareWires: Set[VName] = unmatchedVertices.filter(m.pattern.representsBareWire)
  lazy val uNodes: Set[VName] = unmatchedVertices.filter { v => !m.pattern.vdata(v).isWireVertex }
  lazy val uWires: Set[VName] = unmatchedVertices.filter { v => m.pattern.vdata(v).isWireVertex }
  val unmatchedVertices: Set[VName] = m.pattern.verts.filter(v => bboxesMatched(v) && !m.map.v.domSet.contains(v))
  val uCircles: Set[VName] = unmatchedVertices.filter(m.pattern.isCircle)
  val uBBoxes: Set[BBName] = m.pattern.bboxes.filter(bb => parentBBoxesMatched(bb) && !m.map.bb.domSet.contains(bb))


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

      targetVertices.find(v => m.target.isCircle(v) && reflectsBBoxes(pc, v)) match {
        case None => nextState match {
          case Some(next) => next.nextMatch();
          case None => None
        }
        case Some(tc) =>
          val pce = m.pattern.inEdges(pc).min
          val tce = m.target.inEdges(tc).min
          copy(m = m.addEdge(pce -> tce, pc -> tc), targetVertices = targetVertices - tc).nextMatch()
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
                    targetVertices.contains(m.target.edgeGetOtherVertex(e, nt))
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
          copy(candidateNodes = Some(targetVertices.filter { v =>
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
          val cWires =
            for (v <- targetVertices if m.target.representsWire(v) && reflectsBBoxes(pbw, v);
                 i <- 0 to m.bareWireMap.get(v).map(_.length).getOrElse(0))
              yield (v, i)
          copy(candidateWires = Some(cWires)).nextMatch()
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
    } else if (uBBoxes.nonEmpty && unmatchedVertices.isEmpty) {
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
          val killState = copy(
            m = m.copy(pattern = killGraph, bbops = killOp :: m.bbops)
          )

          val expState =
            if (m.pattern.isWildBBox(pbb)) {
              // if a !-box is wild, drop it, rather than copy/expand
              val (dropGraph, dropOp) = m.pattern.dropBBox(pbb)
              copy(m = m.copy(pattern = dropGraph, bbops = dropOp :: m.bbops),
                nextState = Some(killState))
            } else { // only expand/copy non-wild !-boxes, or we'll get infinite matchings
              val minV = m.pattern.contents(pbb).min
              val (expandGraph, expandOp) = m.pattern.expandBBox(pbb)
              val (copyGraph, copyOp) = m.pattern.copyBBox(pbb)

              val expanded = m.bbops.exists { case BBExpand(bb1, _) => bb1 == pbb; case _ => false }

              // only copy bboxes that have never been expanded
              val copyState =
                if (expanded) killState
                else
                  copy(
                    m = m.copy(pattern = copyGraph, bbops = copyOp :: m.bbops),
                    candidateBBoxes = Some(m.target.bboxes.filter { tbb => reflectsParentBBoxes(pbb, tbb) }),
                    bboxOrbits = bboxOrbits + (copyOp.mp.v(minV) -> minV),
                    nextState = Some(killState)
                  )

              //          val schedule =
              //            (expandGraph.adjacentVerts(expandGraph.contents(pbb)) ++
              //             expandOp.mp.v.directImage(m.pattern.contents(pbb)))
              //              .filter{ v =>
              //                !expandGraph.vdata(v).isWireVertex &&
              //                bboxesMatched(v)
              //              }

              copy(
                m = m.copy(pattern = expandGraph, bbops = expandOp :: m.bbops),
                psNodes = pNodes, // re-schedule everything
                bboxOrbits = bboxOrbits + (expandOp.mp.v(minV) -> minV),
                nextState = Some(copyState)
              )
            }


          expState.nextMatch()
      }

      // if there is nothing left to do, check if the match is complete and return it if so. If not, continue
      // the search from nextState
    } else {
      if (pNodes.isEmpty && m.isTotal) {
        if (MatchState.countMatches) MatchState.matchCounter += 1
        val ms = copy(m = m.copy(subst = expressionMatcher.toMap))
        Some((ms.m, nextState))
      } else {
        nextState match {
          case Some(ms1) => ms1.nextMatch()
          case None => None
        }
      }
    }
  }

  def matchIsMonotone(pv: VName, tv: VName): Boolean =
    if (!MatchState.smashSymmetries) true
    else
      bboxOrbits.get(pv) match {
        case Some(rep) =>
          // check the orbit of v is being matched in a monotone manner w.r.t. v
          bboxOrbits.codf(rep).forall { pv1 =>
            m.map.v.get(pv1) match {
              case None => true
              case Some(tv1) => (pv <= pv1) == (tv <= tv1)
            }
          }
        case None => true
      }

  def pVertexMayBeCompleted(vp: VName): Boolean = {
    val allVertices = m.pattern.adjacentVerts(vp)
    val concreteVertices = allVertices.filter(v => m.pattern.bboxesContaining(v).isEmpty)
    val hasBBox = allVertices.size > concreteVertices.size
    val tArity = m.target.arity(m.map.v(vp))

    concreteVertices.size == tArity || (hasBBox && concreteVertices.size <= tArity)
  }

  // TODO: we may only need to check the closest parent, not all parents

  def reflectsBBoxes(vp: VName, vt: VName): Boolean =
    m.map.bb.directImage(m.pattern.bboxesContaining(vp)) == m.target.bboxesContaining(vt)

  def bboxesMatched(vp: VName): Boolean =
    m.pattern.bboxesContaining(vp).forall(m.map.bb.domSet.contains)

  def reflectsParentBBoxes(bbp: BBName, bbt: BBName): Boolean =
    m.map.bb.directImage(m.pattern.bboxParents(bbp)) == m.target.bboxParents(bbt)

  def parentBBoxesMatched(bbp: BBName): Boolean = {
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
    if (!reflectsBBoxes(np, nt) || !matchIsMonotone(np, nt)) None
    else
      (m.pattern.vdata(np), m.target.vdata(nt)) match {
        case (pd: NodeV, td: NodeV) =>
          if (pd.typ == td.typ) {
            if (pd.hasValue)
              expressionMatcher.addMatch(pd.phaseData, td.phaseData).map {
                angleMatcher1 =>
                  copy(
                    m = m.addVertex(np -> nt),
                    pNodes = pNodes + np,
                    psNodes = psNodes + np,
                    targetVertices = targetVertices - nt,
                    expressionMatcher = angleMatcher1
                  )
              }
            else if (pd.value == td.value)
              Some(copy(
                m = m.addVertex(np -> nt),
                pNodes = pNodes + np,
                psNodes = psNodes + np,
                targetVertices = targetVertices - nt
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
  def matchNewWire(vp: VName, ep: EName, vt: VName, et: EName): Option[MatchState] = {
    val pDir = m.pattern.edata(ep).isDirected
    val tDir = m.target.edata(et).isDirected
    val pOutEdge = m.pattern.source(ep) == vp
    val tOutEdge = m.target.source(et) == vt

    // match directedness and, if the edge is directed, direction
    if ((pDir && tDir && pOutEdge == tOutEdge) || (!pDir && !tDir)) {
      val newVp = m.pattern.edgeGetOtherVertex(ep, vp)
      val newVt = m.target.edgeGetOtherVertex(et, vt)

      if (pNodes contains newVp) {
        if (m.map.v contains (newVp -> newVt))
          Some(copy(psNodes = psNodes + newVp, m = m.addEdge(ep -> et)))
        else None
      } else if (targetVertices contains newVt) {
        (m.pattern.vdata(newVp), m.target.vdata(newVt)) match {
          case (_: WireV, _: WireV) if reflectsBBoxes(newVp, newVt) && matchIsMonotone(newVp, newVt) =>
            (m.pattern.wireVertexGetOtherEdge(newVp, ep), m.target.wireVertexGetOtherEdge(newVt, et)) match {
              case (Some(newEp), Some(newEt)) =>
                copy(
                  m = m.addEdge(ep -> et, newVp -> newVt),
                  targetVertices = targetVertices - newVt
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
  // use !-box orbits to ignore redundant matches
  var smashSymmetries = true
  // for testing e.g. laziness in a single thread
  private var matchCounter = 0
  private var countMatches = false

  def startCountingMatches(): Unit = {
    matchCounter = 0
    countMatches = true
  }

  def matchCount(): Int = {
    countMatches = false
    matchCounter
  }
}
