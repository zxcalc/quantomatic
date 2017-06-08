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
    val patN = pat.normalise
    val tgtN = tgt.normalise
    val restrict1 = restrictTo.foldRight(restrictTo) { (v, s) =>
      if (tgtN.verts contains v) {
        s union tgtN.adjacentVerts(v)
      } else s
    }

    MatchState(
      m = Match(pattern = patN, patternExpanded = patN, target = tgtN),
      tVerts = restrict1,
      angleMatcher = AngleExpressionMatcher(patVars,tgtVars))
  }

  def findMatches(pat: Graph, tgt: Graph, restrictTo: Set[VName]): Stream[Match] = {
    matchMain(initialise(pat, tgt, restrictTo))
  }

  def findMatches(pat: Graph, tgt: Graph): Stream[Match] =
    findMatches(pat, tgt, tgt.verts)

}
