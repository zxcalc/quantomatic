PolyML.SaveState.loadState "../heaps/quanto.heap";
PolyML.Compiler.printDepth:=100;


open RG_SimpUtil

val rotate = load_rule "rules/rotate"
val green_ids = load_rule "rules/green_ids"
val green_elim = load_rule "rules/green_elim"

val red_copy = load_rule "rules/red_copy"
val red_sp = load_rule "rules/red_sp"
val green_sp = load_rule "rules/green_sp"
val green_scalar = load_rule "rules/green_scalar"
val red_scalar = load_rule "rules/red_scalar"
val hopf = load_rule "rules/hopf"

val simps = ruleset [red_copy, red_sp, green_sp, hopf, red_scalar, green_scalar]

val rotate_simproc = (
  REDUCE_ALL simps ++
  REDUCE_METRIC num_boundary_red green_ids ++
  LOOP (
    REDUCE_METRIC_TO 1 min_green_arity rotate ++
    REDUCE_WITH { PRE  = fn g => (min_green_arity g = 1),
                  POST = K true } green_elim
  ) ++
  REDUCE_ALL simps
)


(* testing simproc *)

val sample = load_graph "sample2"

val steps = Json.Object (fold_index (fn (i,step) => (
    Json.update ("s" ^ Int.toString i, Json.Int step)
  )) [0,1,2,3,4] Json.empty_obj)


(*(Seq.list_of (rotate_simproc sample)) Json.empty_obj)*)

val SOME g' = normalise rotate_simproc sample

val _ = save_graph "sample2_simp" g'

(*
val _ = Json.write_file "test.qgraph" (JsonIO.OutputGraphJSON.output (g', gann))*)

(*TextIO.print (RG_Mathematica.to_mathematica "sample" sample)*)



