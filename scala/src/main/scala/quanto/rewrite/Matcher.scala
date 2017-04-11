package quanto.rewrite
import quanto.data._
//
//case class MatchState(
//              m: Match,
//              finished: Boolean,
//              uNodes: Set[VName],
//              uWires: Set[VName],
//              pNodes: Set[VName],
//              psNodes: Set[VName],
//              angleMatcher: AngleExpressionMatcher) {
//
//  def copy(m: Match = this.m,
//           finished: Boolean = this.finished,
//           uNodes: Set[VName] = this.uNodes,
//           uWires: Set[VName] = this.uWires,
//           pNodes: Set[VName] = this.pNodes,
//           psNodes: Set[VName] = this.psNodes,
//           angleMatcher: AngleExpressionMatcher = this.angleMatcher) =
//    MatchState(m,finished,uNodes,uWires,pNodes,psNodes,angleMatcher)
//}
//
//object Matcher {
//  def findMatches(pat: Graph, tgt: Graph): Stream[Match] = {
//    val patVars = Vector()
//    val tgtVars = Vector()
//    val patNodes = pat.verts.filter(!pat.vdata(_).isWireVertex)
//    val patWires = pat.verts.filter(pat.vdata(_).isWireVertex)
//    val concrete = pat.verts.filter{ v =>
//      !pat.vdata(v).isWireVertex && pat.bboxesContaining(v).isEmpty
//    }
//
//    val ms = MatchState(Match(pat, tgt), finished=false,
//      patNodes, patWires, Set.empty, concrete,
//      AngleExpressionMatcher(patVars,tgtVars))
//
//    Stream.empty
//  }
//}
