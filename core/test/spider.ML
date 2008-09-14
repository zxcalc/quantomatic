structure Test_Spider = 
struct

val g = Rule.get_lhs (TheoryHack.x_isometry_rule);
val L = Seq.list_of (Spider.mk_max_spider_rule g);

end;

(*
     PolyML.exception_trace (fn () =>     );
*)
