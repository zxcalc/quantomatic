package quanto.rewrite
import quanto.data._

object Matcher {
  def findMatches(pat: Graph, tgt: Graph): Stream[Match] = {
    val patVars = Vector()
    val tgtVars = Vector()
    val patNodes = pat.verts.filter(!pat.vdata(_).isWireVertex)
    val patWires = pat.verts.filter(pat.vdata(_).isWireVertex)
    val concrete = pat.verts.filter{ v =>
      !pat.vdata(v).isWireVertex && pat.bboxesContaining(v).isEmpty
    }

    val ms = MatchState(Match(pat, tgt), finished=false,
      patNodes, patWires, Set.empty, concrete,
      AngleExpressionMatcher(patVars,tgtVars))

    // TODO: !-boxes
    ms.matchPending().flatMap { ms1 =>
      if (ms1.finished) Stream(ms1.m) else Stream()
    }
  }




}
