package quanto.rewrite

import quanto.data.Theory.ValueType
import quanto.data._

import scala.annotation.tailrec

object Matcher {
  def initialise(pat: Graph, tgt: Graph, restrictTo: Set[VName]): MatchState = {
    // TODO: new free vars should be fresh w.r.t. vars in target
    val patVars: Vector[(ValueType, String)] = pat.freeVars.toVector
    val tgtVars: Vector[(ValueType, String)] = tgt.freeVars.toVector
    val patN = pat.normalise
    val tgtN = tgt.normalise
    val restrict0 = restrictTo intersect tgtN.verts
    val restrict1 = restrict0.foldRight(restrict0) { (v, s) =>
      if (tgtN.verts contains v) {
        s union tgtN.adjacentVerts(v)
      } else s
    }

    MatchState(
      m = Match(pattern0 = patN, pattern = patN, target = tgtN),
      tVerts = restrict1,
      expressionMatcher = CompositeExpressionMatcher()) // Create the matcher empty, it will fill itself in in time
  }

  def findMatches(pat: Graph, tgt: Graph, restrictTo: Set[VName]): Stream[Match] = {
    matchMain(initialise(pat, tgt, restrictTo))
  }

  def findMatches(pat: Graph, tgt: Graph): Stream[Match] =
    findMatches(pat, tgt, tgt.verts)

  private def matchMain(ms: MatchState): Stream[Match] =
    ms.nextMatch() match {
      case Some((m1, Some(next))) => m1 #:: matchMain(next)
      case Some((m1, None)) => Stream(m1)
      case None => Stream()
    }

}
