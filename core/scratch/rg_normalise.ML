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

val simproc = (
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

val (sample, gann) = JsonIO.InputGraphJSON.input (Json.read_file "sample2.qgraph");

(*val SOME (_, g') = final (simproc sample)

val _ = Json.write_file "test.qgraph" (JsonIO.OutputGraphJSON.output (g', gann))*)

TextIO.print (RG_Mathematica.to_mathematica "sample" sample)



