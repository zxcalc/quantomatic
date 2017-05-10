package quanto.rewrite
import quanto.data._

object Matcher {
  def findMatches(pat: Graph, tgt: Graph, restrictTo: Set[VName]): Stream[Match] = {
    val patVars = Vector()
    val tgtVars = Vector()
//    val patNodes = pat.verts.filter(!pat.vdata(_).isWireVertex)
//    val patWires = pat.verts.filter(pat.vdata(_).isWireVertex)

    // TODO: how is this set used? (tVerts?)
//    val concrete = pat.verts.filter{ v =>
//      !pat.vdata(v).isWireVertex && pat.bboxesContaining(v).isEmpty
//    }

    val ms = MatchState(
      m = Match(pat, tgt),
      tVerts = restrictTo,
      angleMatcher = AngleExpressionMatcher(patVars,tgtVars))

    // TODO: !-boxes

    ms.matchPending()
  }

  def findMatches(pat: Graph, tgt: Graph): Stream[Match] =
    findMatches(pat, tgt, tgt.verts)

}
