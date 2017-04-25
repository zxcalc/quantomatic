package quanto.rewrite
import quanto.data._


case class MatchState(
                       m: Match,
                       finished: Boolean,
                       uNodes: Set[VName],
                       uWires: Set[VName],
                       pNodes: Set[VName],
                       psNodes: Set[VName],
                       angleMatcher: AngleExpressionMatcher) {

  def copy(m: Match = this.m,
           finished: Boolean = this.finished,
           uNodes: Set[VName] = this.uNodes,
           uWires: Set[VName] = this.uWires,
           pNodes: Set[VName] = this.pNodes,
           psNodes: Set[VName] = this.psNodes,
           angleMatcher: AngleExpressionMatcher = this.angleMatcher) =
    MatchState(m,finished,uNodes,uWires,pNodes,psNodes,angleMatcher)


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

  def continueMatchingFrom(v: VName): Stream[MatchState] =
    if (pVertexMayBeCompleted(v)) matchNhd(v)
    else Stream()

  def pVertexMayBeCompleted(v: VName) = true

  def matchNhd(v: VName): Stream[MatchState] = Stream()



  def matchAndScheduleNew(v: VName): Stream[MatchState] = {
    Stream.empty
  }
}
