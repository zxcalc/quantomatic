theory scratch
imports quanto
begin

ML {*
open RG_SimpUtil;
open RG_Theory;

val _ = cd "/Users/alek/git/quantomatic/core/test";
val g = load_graph "graphs/target-test";
val r = snd (load_rule "rules/rotate-targeted");

val SOME v = min_arity_vertex_where is_interior_green g;
val pre = VVInj.empty |> VVInj.add (V.mk "v10", v);
val m = MatchSearch.match_with_prematch (Rule.get_lhs r) g pre;

PolyML.exception_trace (fn () => Seq.pull m);

*}

end
