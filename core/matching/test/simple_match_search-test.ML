(* test matching with matchsearch *)
local

open Testing;

open Test_MkG;
structure M = Test_Match;
structure S = Test_SimpleMatchSearch;
(* set log level for tests *)
val _ = (S.Log.level_ref := 4)
val no_extra_names = (V.NSet.empty, E.NSet.empty, B.NSet.empty);
fun match g1 g2 = Seq.list_of (S.match no_extra_names g1 g2);
in val _ = let 


(* Graph: (B - X) *)
val SOME (g0,g1) = test "match1: {B - X} matches {B - X - B}" 
  (fn () => 
  let
    val g = G.empty;
    val (b1, g) = g |> G.add_vertex bvert;
    val (n1, g) = g |> G.add_vertex (mkX zero_angle);
    val (e0, g) = g |> G.add_edge undir_edge b1 n1;
    val g0 = g;
    
    (* Graph: (B - X - B) *)
    val g = G.empty;
    val (b1, g) = g |> G.add_vertex bvert;
    val (n1, g) = g |> G.add_vertex (mkX zero_angle);
    val (b2, g) = g |> G.add_vertex bvert;
    val (e0, g) = g |> G.add_edge undir_edge b1 n1;
    val (e1, g) = g |> G.add_edge undir_edge n1 b2;
    val g1 = g;
    
    val ms = match g1 g0;
    val _ = map M.print ms;
    val [] = ms;
  in (g0,g1) end) ();

val SOME g2 = test "match2: {B - X - B} matches {B - X - B}" 
  (fn () => 
  let
    val (_,g2) = G.rename_apart g1 g1;
    val _ = G.print g1;
    val _ = V.NSet.print (G.get_boundary g1);
    val _ = G.print g2;
    
    val ms = match g1 g2;
    val _ = map M.print ms;
    val [m1,m2] = ms;
  in g2 end) ();


val SOME (g3,g4,g5) = test ("match3; setup: "
  ^ "g3={X(0) - Z(0) = B} "
  ^ "g4={X(pi) - Z(0) = B} "
  ^ "g5={X(a) - Z(0) = B}")
  (fn () => 
  let
    val g = G.empty;
    val (b1, g) = g |> G.add_vertex bvert;
    val (b2, g) = g |> G.add_vertex bvert;
    val (n1, g) = g |> G.add_vertex (mkX zero_angle);
    val (n2, g) = g |> G.add_vertex (mkZ zero_angle);
    val (_, g) = g |> G.add_edge undir_edge n2 n1;
    val (_, g) = g |> G.add_edge undir_edge b1 n2;
    val (_, g) = g |> G.add_edge undir_edge b2 n2;
    val g3 = g;
    
    val g = G.empty;
    val (b1, g) = g |> G.add_vertex bvert;
    val (b2, g) = g |> G.add_vertex bvert;
    val (n1, g) = g |> G.add_vertex (mkX pi_angle);
    val (n2, g) = g |> G.add_vertex (mkZ zero_angle);
    val (_, g) = g |> G.add_edge undir_edge n2 n1;
    val (_, g) = g |> G.add_edge undir_edge b1 n2;
    val (_, g) = g |> G.add_edge undir_edge b2 n2;
    val g4 = g;
    
    val g = G.empty;
    val (b1, g) = g |> G.add_vertex bvert;
    val (b2, g) = g |> G.add_vertex bvert;
    val (n1, g) = g |> G.add_vertex (mkX (var_angle "a"));
    val (n2, g) = g |> G.add_vertex (mkZ zero_angle);
    val (_, g) = g |> G.add_edge undir_edge n2 n1;
    val (_, g) = g |> G.add_edge undir_edge b1 n2;
    val (_, g) = g |> G.add_edge undir_edge b2 n2;
    val g5 = g;
  in (g3,g4,g5) end) ();

val _ = test "match4a: {X(0) - Z(0) = B} matches {X(a) - Z(0) = B}" (fn () => 
  let    
    (* val pat = g3;
    val (rn,pat) = (G.rename_apart tgt g3); *)
    val [] = match g3 g5;
  in () end) ();

val _ = test "match4b: {X(pi) - Z(0) = B} matches {X(a) - Z(0) = B}" (fn () => 
  let val [] = match g4 g5;
  in () end);

val _ = test "match4c: {X(a) - Z(0) = B} matches {X(pi) - Z(0) = B}" (fn () => 
  let val [m1,m2] = match g5 g3;
  in () end);

val _ = test "match4d: {X(a) - Z(0) = B} matches {X(pi) - Z(0) = B}" (fn () => 
  let val [m1,m2] = match g5 g4;
  in () end);

val _ = test "match6: match self-loop" (fn () => 
  let 
    val g = G.empty;
    val (b1, g) = g |> G.add_vertex bvert;
    val (_, g) = g |> G.add_edge undir_edge b1 b1;
    val pat = g;
    
    val g = G.empty;
    val (n1, g) = g |> G.add_vertex (mkX zero_angle);
    val (_, g) = g |> G.add_edge undir_edge n1 n1;
    val (_, g) = g |> G.add_edge undir_edge n1 n1;
    val tgt = g;
    
    val (rn,pat) = (G.rename_apart tgt pat);
    val _ = G.print pat;
    val _ = G.print tgt;
    
    val [m] = match pat tgt;
    val _ = M.print m;
  in () end);

val _ = test "match7: removing a self-loop on an X node: drop bbox" (fn () => 
  let
    (* X spider self-loop lhs deletion *)
    val g = G.empty;
    val (b1, g) = g |> G.add_vertex bvert;
    val (n1, g) = g |> G.add_vertex (mkX (var_angle "a"));
    val (_, g) = g |> G.add_edge undir_edge n1 n1;
    val (_, g) = g |> G.add_edge undir_edge b1 n1;
    val (bbn, g) = g |> G.new_box (B.mk "B0");
    val g = g |> G.move_to_box bbn (V.NSet.single b1);
    val pat = g;
    
    (* single self-loop on X(0) *)
    val g = G.empty;
    val (n1, g) = g |> G.add_vertex (mkX (var_angle "0"));
    val (_, g) = g |> G.add_edge undir_edge n1 n1;
    val tgt = g;
    
    val (rn,pat) = (G.rename_apart tgt pat);
    val _ = G.print pat;
    val _ = G.print tgt;
    
    (* note: self-loops on undir edges get matched twice by simple-matcher. TODO: fix this. *)
    val [m1,m2] = match pat tgt;
    val _ = M.print m1; 
    val _ = M.print m2;
  in () end) ();

val _ = test "match8: unfolding 1 bbox" (fn () => 
  let     
    (* X spider self-loop lhs deletion *)
    val g = G.empty;
    val (b1, g) = g |> G.add_vertex bvert;
    val (n1, g) = g |> G.add_vertex (mkX (var_angle "a"));
    val (_, g) = g |> G.add_edge undir_edge n1 n1;
    val (_, g) = g |> G.add_edge undir_edge b1 n1;
    val (bbn, g) = g |> G.new_box (B.mk "B0");
    val g = g |> G.move_to_box bbn (V.NSet.single b1);
    val pat = g;

    (* needs one unfolding of bangbox...*)
    val g = G.empty;
    val (b1, g) = g |> G.add_vertex bvert;
    val (n1, g) = g |> G.add_vertex (mkX (var_angle "b"));
    val (_, g) = g |> G.add_edge undir_edge n1 n1;
    val (_, g) = g |> G.add_edge undir_edge b1 n1;
    val tgt = g;
    
    val (rn,pat) = (G.rename_apart tgt pat);
    val _ = G.print pat;
    val _ = G.print tgt;
    
    val [m1,m2] = match pat tgt;
    val _ = M.print m1;
    val _ = M.print m2;
  in () end) ();


val _ = test "match9: should fail to match" (fn () => 
  let
    (* test graph (isometry lhs) *)
    val g = G.empty;
    val (b1, g) = g |> G.add_vertex bvert;
    val (n1, g) = g |> G.add_vertex (mkX zero_angle);
    val (n2, g) = g |> G.add_vertex (mkX zero_angle);
    val (b2, g) = g |> G.add_vertex bvert;
    val (e0, g) = g |> G.add_edge undir_edge b1 n1;
    val (e1, g) = g |> G.add_edge undir_edge n1 n2;
    val (e2, g) = g |> G.add_edge undir_edge n1 n2;
    val (e3, g) = g |> G.add_edge undir_edge n2 b2;
    val _ = G.print g;    
    val tgt = g;

    (* Another graph, should not match the above *)
    val g = G.empty;
    val (b1, g) = g |> G.add_vertex bvert;
    val (n1, g) = g |> G.add_vertex (mkX zero_angle);
    val (n2, g) = g |> G.add_vertex (mkX zero_angle);
    val (b2, g) = g |> G.add_vertex bvert;
    val (e1, g) = g |> G.add_edge undir_edge n1 n2;
    val (e2, g) = g |> G.add_edge undir_edge n1 n2;
    val _ = G.print g;    
    val pat = g;  

    val [] = match pat tgt;
  in () end) ();

val _ = test "match-with-pluggings" 
  (fn () => 
  let
    (* Graph: (B - X - X - B) *)
    val g = G.empty;
    val (b1, g) = g |> G.add_vertex bvert;
    val (n1, g) = g |> G.add_vertex (mkX zero_angle);
    val (n2, g) = g |> G.add_vertex (mkX zero_angle);
    val (b2, g) = g |> G.add_vertex bvert;
    val (e0, g) = g |> G.add_edge undir_edge b1 n1;
    val (e1, g) = g |> G.add_edge undir_edge n2 b2;
    val (e2, g) = g |> G.add_edge undir_edge n1 n2;
    val pat = g;
    
    (* Graph: X = X *)
    val g = G.empty;
    val (n1', g) = g |> G.add_vertex (mkX zero_angle);
    val (n2', g) = g |> G.add_vertex (mkX zero_angle);
    val (e0', g) = g |> G.add_edge undir_edge n1' n2';
    val (e1', g) = g |> G.add_edge undir_edge n1' n2';
    val (_, tgt) = G.rename_apart pat g;
        
    val [m1,m2,m3,m4] = match pat tgt;
    val _ = writeln "----------";
    val _ = G.print pat;
    val _ = G.print tgt;
    val _ = M.print m1;
    val pat_inst1 = M.inst_pat_graph m1 pat; 
    val _ = G.print pat_inst1;
    
    val pat_inst2 = M.inst_pat_graph m2 pat; 
    val pat_inst3 = M.inst_pat_graph m3 pat; 
    val pat_inst4 = M.inst_pat_graph m4 pat; 
    
    val _ = if V.NSet.is_empty (G.get_edgepoints pat_inst1) then () 
        else raise ERROR "has an edge-point";

  in () end) ();
  
(* one unfolding of bangbox...*)
(* 
val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;
val (b2, g) = g |> G.add_vertex bvert;
val (n1, g) = g |> G.add_vertex (mkX (var_angle "b"));
val (_, g) = g |> G.add_edge undir_edge n1 n1;
val (_, g) = g |> G.add_edge undir_edge b1 n1;
val (_, g) = g |> G.add_edge undir_edge b2 n1;
val tgt = g;

val (rn,pat) = (G.rename_apart tgt pat);
val _ = G.print pat;
val _ = G.print tgt;

val [m] = Seq.list_of (MatchSearch.match pat tgt V.NTab.empty);
val _ = Match.print m;
*)
in
  (S.Log.level_ref := 0; 
   assert_no_failed_tests (); 
   Pretty.writeln (Pretty.str "Tests passed: graph matching."))
end  (* let *)
handle e => 
       (Pretty.writeln (Pretty.str "Test Failed: graph matching."); raise e);
end; (* local *)
