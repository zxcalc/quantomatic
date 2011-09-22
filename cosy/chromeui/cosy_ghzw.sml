
structure Cosy =
struct
  structure Theory = GHZW_Theory
  structure Synth = GHZW_DefaultSynth
  structure RSBuilder = GHZW_RSBuilder
  structure Tensor = GHZW_TensorData.Tensor
  
  datatype T =
   SYNTH of Synth.T | 
   RS of Theory.Ruleset.T |
   RULE of Theory.Rule.T |
   GRAPH of Theory.Graph.T |
   ERR of string
   
  val output_dot = GHZW_OutputGraphDot.output
  val gens = GHZW_Gens.gen GHZW_VertexData.TICK (1,1) ::
             (GHZW_Gens.gen_list 4 [GHZW_VertexData.GHZ, GHZW_VertexData.W])
  
  local
    val rs' = GHZW_Theory.Ruleset.empty
    val (_,rs') = rs' |> GHZW_Theory.Ruleset.add_fresh_rule
                          (RuleName.mk "ghz_fr", GHZW_Rws.frob GHZW_VertexData.GHZ)
    val (_,rs') = rs' |> GHZW_Theory.Ruleset.add_fresh_rule
                          (RuleName.mk "ghz_sp", GHZW_Rws.special GHZW_VertexData.GHZ)
    val (_,rs') = rs' |> GHZW_Theory.Ruleset.add_fresh_rule
                          (RuleName.mk "w_fr", GHZW_Rws.frob GHZW_VertexData.W)
    val redex = TagName.mk "r"
    val rs' = rs' |> GHZW_Theory.Ruleset.tag_rule (RuleName.mk "ghz_fr") redex
                  |> GHZW_Theory.Ruleset.tag_rule (RuleName.mk "ghz_sp") redex
                  |> GHZW_Theory.Ruleset.tag_rule (RuleName.mk "w_fr") redex
  in
    val initial_rs = RS rs'
  end
  
  val rule_matches_graph = GHZW_Enum.rule_matches_graph
end



(*
val ghzw_data : (GHZW_Theory.Graph.T, GHZW_Theory.Ruleset.T, GHZW_DefaultSynth.T) TheoryData.T = {
  name = "GHZ/W",
  dotfun = GHZW_OutputGraphDot.output,
  gens = GHZW_Gens.gen GHZW_VertexData.TICK (1,1) :: (GHZW_Gens.gen_list 4 [GHZW_VertexData.GHZ, GHZW_VertexData.W]),
  stats = GHZW_DefaultSynth.stats,
  class_list = fn synth => GHZW_DefaultSynth.eqclass_fold (cons o (apfst GHZW_TensorData.Tensor.to_string)) synth [],
  rs_pairs =
    (rule_data GHZW_Theory.Rule.get_lhs GHZW_Theory.Rule.get_rhs) o
    GHZW_Theory.Ruleset.get_allrules
}
*)

(*
fun synth run = SYNTH (GHZW_DefaultSynth.synth (TheoryData.get_gens ghzw_data) run)
fun synth_with_rs (RS rs) run =
  SYNTH (GHZW_DefaultSynth.synth_with_rs rs (TheoryData.get_gens ghzw_data) run)
fun ruleset (SYNTH s) = RS (GHZW_RSBuilder.from_synth s)
fun update (SYNTH s) (RS rs) = RS (rs |> GHZW_RSBuilder.update s)
fun reduce (RS rs) = RS (GHZW_RSBuilder.reduce rs)
fun update_redex run rs = rs |> update (synth_with_rs rs run) |> reduce
fun update_naive run rs = rs |> update (synth run)
fun size (RS rs) = RuleName.NTab.cardinality (GHZW_Theory.Ruleset.get_allrules rs)
fun rule_matches_rule (RULE r1) (RULE r2) = GHZW_RSBuilder.rule_matches_rule r1 r2

fun match_rule (RULE rule) (GRAPH target) = GHZW_Enum.rule_matches_graph rule target


(*fun update_with run rs = rs |> update (synth run) |> reduce;*)
fun get_rule (RS rs) name = case GHZW_Theory.Ruleset.lookup_rule rs (RuleName.mk name)
			      of SOME r => RULE r 
			       | _ => ERR "Rule not found."

fun get_lhs (RULE rule) = GRAPH (GHZW_Theory.Rule.get_lhs rule)
fun get_rhs (RULE rule) = GRAPH (GHZW_Theory.Rule.get_rhs rule)

(*fun synth_list runs rs = fold update_redex runs rs*)

fun escape str = let
  fun esc #"<" = "&lt;"
    | esc #">" = "&gt;"
    | esc #"&" = "&amp;"
    | esc #"\"" = "&quot;"
    | esc c = String.str c
in String.translate esc str
end

fun to_xml (RULE rule) = GHZW_Theory.IO_Xml.Output.Rule.output rule
  | to_xml (GRAPH graph) = GHZW_Theory.IO_Xml.Output.Graph.output graph

fun xml item = output_string (escape (XMLWriter.write_to_string (to_xml item)))

fun save file item = XMLWriter.write_to_file file (to_xml item)

fun load file = let
  val data = XMLReader.read_from_file file
in if String.isSuffix ".rule" file then
     RULE (GHZW_Theory.IO_Xml.Input.Rule.input data)
   else if String.isSuffix ".graph" file then
     GRAPH (GHZW_Theory.IO_Xml.Input.Graph.input data)
   else ERR "Unknown file extension"
end

val rs' = GHZW_Theory.Ruleset.empty
val (_,rs') = rs' |> GHZW_Theory.Ruleset.add_fresh_rule (RuleName.mk "ghz_fr", GHZW_Rws.frob GHZW_VertexData.GHZ)
val (_,rs') = rs' |> GHZW_Theory.Ruleset.add_fresh_rule (RuleName.mk "ghz_sp", GHZW_Rws.special GHZW_VertexData.GHZ)
val (_,rs') = rs' |> GHZW_Theory.Ruleset.add_fresh_rule (RuleName.mk "w_fr", GHZW_Rws.frob GHZW_VertexData.W)

val redex = TagName.mk "r"
val rs' = rs' |> GHZW_Theory.Ruleset.tag_rule (RuleName.mk "ghz_fr") redex
              |> GHZW_Theory.Ruleset.tag_rule (RuleName.mk "ghz_sp") redex
              |> GHZW_Theory.Ruleset.tag_rule (RuleName.mk "w_fr") redex

val rs = RS rs'

val default_runs = [(0,0,3,3),(0,1,3,3),(1,0,3,3),(0,2,3,3),(1,1,3,3),
                    (2,0,3,3),(0,3,3,3),(1,2,3,3),(2,1,3,3),(3,0,3,3)]
val short_runs = [(0,0,2,2),(0,1,2,2)]
val long_runs = default_runs @ [(2,2,4,4)]

fun process updater ruleset runs = let
  fun do_update (run as (r1,r2,r3,r4)) rs = let
    val _ = output_string ("Updating for: ("^
                           Int.toString r1^","^
                           Int.toString r2^","^
                           Int.toString r3^","^
                           Int.toString r4^")...")
    val rs' = rs |> updater run
  in (size rs', rs')
  end
  val (sizes, final_rs) = fold_map do_update runs ruleset
  val _ = output_string "Done."
in (sizes, final_rs)
end

fun as_data list = fold2 (fn i => fn d => fn str => str^"("^Int.toString i^","^Int.toString d^")\n") (0 upto (length list - 1)) list ""

fun out (SYNTH s) = output_synth ghzw_data s
  | out (RS rs) = output_ruleset ghzw_data rs
  | out (RULE r) = output_rule ghzw_data 
			       (GHZW_Theory.Rule.get_lhs r)
			       (GHZW_Theory.Rule.get_rhs r)
  | out (GRAPH g) = output_graph ghzw_data g
  | out (ERR e) = output_string e
*)