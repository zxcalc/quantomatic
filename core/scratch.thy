theory scratch
imports quanto
begin

ML {*
open RG_SimpUtil;
open RG_Theory;

val _ = cd "/Users/alek/git/quantomatic/core/test";
val g = load_graph "graphs/target-test";
val r = snd (load_rule "rules/rotate-targeted");
val l = Rule.get_lhs r;
val vs = Graph.get_vertices_in_bbox l (B.mk "bx0");
val bb = Graph.is_bboxed l (V.mk "v0");


fun max_concrete_nhd_bbox g bset = let
  fun concrete_nhd_size bb = 
    V.NSet.cardinality (V.NSet.filter
      (not o Graph.is_bboxed g)
      (Graph.get_adj_vertices_to_set g
        (Graph.get_vertices_in_bbox g bb)))
  fun max bb' v = let
    val i' = concrete_nhd_size bb'
  in case v
       of SOME (i,bb) => if (i' > i) then SOME (i',bb') else SOME (i,bb)
        | NONE => SOME (i',bb')
  end
in case fold max bset NONE of SOME (_,bb) => SOME bb | NONE => NONE
end


*}

end
