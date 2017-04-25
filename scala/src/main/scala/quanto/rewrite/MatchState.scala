package quanto.rewrite
import quanto.data._


case class MatchState(
                       m: Match,
                       finished: Boolean,
                       uNodes: Set[VName],
                       uWires: Set[VName],
                       pNodes: Set[VName],
                       psNodes: Set[VName],
                       tVerts: Set[VName],
                       angleMatcher: AngleExpressionMatcher) {

  def copy(m: Match = this.m,
           finished: Boolean = this.finished,
           uNodes: Set[VName] = this.uNodes,
           uWires: Set[VName] = this.uWires,
           pNodes: Set[VName] = this.pNodes,
           psNodes: Set[VName] = this.psNodes,
           tVerts: Set[VName] = this.tVerts,
           angleMatcher: AngleExpressionMatcher = this.angleMatcher) =
    MatchState(m,finished,uNodes,uWires,pNodes,psNodes,
      tVerts,angleMatcher)


  def matchPending(): Stream[MatchState] = {
    matchCircles().flatMap(_.matchMain())
  }

  // TODO: stub
  def matchCircles(): Stream[MatchState] =
    Stream(this)

  def matchMain(): Stream[MatchState] = {
    psNodes.headOption match {
      case Some(v) => continueMatchingFrom(v)
      case None => uNodes.headOption match {
        case Some(v) => matchAndScheduleNew(v)
        case None => Stream(this)
      }
    }
  }

  def continueMatchingFrom(np: VName): Stream[MatchState] =
    if (pVertexMayBeCompleted(np)) copy(psNodes = psNodes - np).matchNhd(np)
    else Stream()

  // TODO: stub
  def pVertexMayBeCompleted(v: VName) = true

  def matchNhd(np: VName): Stream[MatchState] = {
    val nt = m.vmap(np)
    m.pattern.adjacentEdges(np).find { e =>
      uWires.contains(m.pattern.edgeGetOtherVertex(e, np))
    } match {
      case Some(ep) =>
        m.target.adjacentEdges(nt).filter { e =>
            tVerts.contains(m.target.edgeGetOtherVertex(e,nt))
        }.foldRight(Stream[MatchState]()) { (et, stream) =>
          matchNewWire(np,ep,nt,et) match {
            case Some(ms1) => ms1.matchNhd(np) ++ stream
            case None => stream
          }
        }
      case None =>
        if (m.target.adjacentEdges(nt).forall(m.emap.codSet.contains(_)))
          copy(pNodes = pNodes - nt).matchMain()
        else
          matchMain()
    }
  }

  def matchAndScheduleNew(np: VName): Stream[MatchState] = {
    val tNodes = tVerts.filter(!m.target.vdata(_).isWireVertex)
    tNodes.foldRight(Stream[MatchState]()) { (nt, stream) =>
      matchNewNode(np, nt) match {
        case Some(ms1) => ms1.matchMain() ++ stream
        case None => stream
      }
    }
  }

  def matchNewNode(np: VName, nt: VName): Option[MatchState] = None

  def matchNewWire(np:VName, ep:EName, nt:VName, et:EName): Option[MatchState] = None



}
