package quanto.rewrite
import quanto.data._

import scala.annotation.tailrec

object Matcher {
  private def matchMain(ms: MatchState): Stream[Match] =
    ms.nextMatch() match {
      case Some((m1,Some(next))) => m1 #:: matchMain(next)
      case Some((m1,None)) => Stream(m1)
      case None => Stream()
    }

  def initialise(pat: Graph, tgt: Graph, restrictTo: Set[VName]): MatchState = {
    // TODO: new free vars should be fresh w.r.t. vars in target
    val patVars = Vector()
    val tgtVars = Vector()

    MatchState(
      m = Match(pattern = pat, patternExpanded = pat, target = tgt),
      tVerts = restrictTo,
      uBareWires = pat.verts.filter(pat.representsBareWire),
      angleMatcher = AngleExpressionMatcher(patVars,tgtVars))
  }

  def findMatches(pat: Graph, tgt: Graph, restrictTo: Set[VName]): Stream[Match] = {
    matchMain(initialise(pat, tgt, restrictTo))
  }

  def findMatches(pat: Graph, tgt: Graph): Stream[Match] =
    findMatches(pat, tgt, tgt.verts)

}
