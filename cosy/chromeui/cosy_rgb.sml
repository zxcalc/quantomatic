structure Cosy =
struct
  structure Theory = RGB_Theory
  structure Synth = RGB_Synth
  structure RSBuilder = RGB_RSBuilder
  structure Tensor = RGB_TensorData.Tensor
  
  datatype T =
   SYNTH of Synth.T | 
   RS of Theory.Ruleset.T |
   RULE of Theory.Rule.T |
   GRAPH of Theory.Graph.T |
   ERR of string
   
  val output_dot = RGB_OutputGraphDot.output
  val gens = RGB_Gens.gen_list 4 [RGB_VertexData.Red, RGB_VertexData.Green, RGB_VertexData.Blue]
  
  local
    val rs' = RGB_Theory.Ruleset.empty
    val (_,rs') = rs' |> RGB_Theory.Ruleset.add_fresh_rule
                          (R.mk "r_fr", RGB_Rws.frob RGB_VertexData.Red)
    val (_,rs') = rs' |> RGB_Theory.Ruleset.add_fresh_rule
                          (R.mk "g_fr", RGB_Rws.frob RGB_VertexData.Green)
    val (_,rs') = rs' |> RGB_Theory.Ruleset.add_fresh_rule
                          (R.mk "b_fr", RGB_Rws.frob RGB_VertexData.Blue)
    val (_,rs') = rs' |> RGB_Theory.Ruleset.add_fresh_rule
                          (R.mk "r_sp", RGB_Rws.special RGB_VertexData.Red)
    val (_,rs') = rs' |> RGB_Theory.Ruleset.add_fresh_rule
                          (R.mk "g_sp", RGB_Rws.special RGB_VertexData.Green)
    val (_,rs') = rs' |> RGB_Theory.Ruleset.add_fresh_rule
                          (R.mk "b_sp", RGB_Rws.special RGB_VertexData.Blue)
    
    val redex = TagName.mk "r"
    val rs' = fold (fn s => RGB_Theory.Ruleset.tag_rule (R.mk s) redex)
               ["r_fr","g_fr","b_fr","r_sp","g_sp","b_sp"] rs'
  in
    val initial_rs = RS rs'
  end
  
  val rule_matches_graph = RGB_Enum.rule_matches_graph
end

(*
datatype cosy = 
	 SYNTH of RGB_Synth.T | 
	 RS of RGB_Theory.Ruleset.T |
	 RULE of RGB_Theory.Rule.T |
	 ERR of string

val rgb_data : (RGB_Theory.Graph.T, RGB_Theory.Ruleset.T, RGB_Synth.T) TheoryData.T = {
  name = "RGB",
  dotfun = RGB_OutputGraphDot.output,
  gens = RGB_Gens.gen_list 4 [RGB_VertexData.Red, RGB_VertexData.Green, RGB_VertexData.Blue],
  stats = RGB_Synth.stats,
  class_list = fn synth => RGB_Synth.eqclass_fold (cons o (apfst RGB_TensorData.Tensor.to_string)) synth [],
  rs_pairs =
    (rule_data RGB_Theory.Rule.get_lhs RGB_Theory.Rule.get_rhs) o
    RGB_Theory.Ruleset.get_allrules
}

fun synth run = SYNTH (RGB_Synth.synth (TheoryData.get_gens rgb_data) run)
fun synth_with_rs (RS rs) run =
  SYNTH (RGB_Synth.synth_with_rs rs (TheoryData.get_gens rgb_data) run)
fun ruleset (SYNTH s) = RS (RGB_RSBuilder.from_synth s)
fun update (SYNTH s) (RS rs) = RS (rs |> RGB_RSBuilder.update s)
fun reduce (RS rs) = RS (RGB_RSBuilder.reduce rs)
fun update_with run rs = rs |> update (synth_with_rs rs run) |> reduce
fun size (RS rs) = R.NTab.cardinality (RGB_Theory.Ruleset.get_allrules rs)
fun rule_matches_rule (RULE r1) (RULE r2) = RGB_RSBuilder.rule_matches_rule r1 r2


(*fun update_with run rs = rs |> update (synth run) |> reduce;*)
fun get_rule (RS rs) name = case RGB_Theory.Ruleset.lookup_rule rs (R.mk name)
			      of SOME r => RULE r 
			       | _ => ERR "Rule not found."

fun synth_list runs rs = fold update_with runs rs


val rs' = RGB_Theory.Ruleset.empty
val (_,rs') = rs' |> RGB_Theory.Ruleset.add_fresh_rule (R.mk "r_fr", RGB_Rws.frob RGB_VertexData.Red)
val (_,rs') = rs' |> RGB_Theory.Ruleset.add_fresh_rule (R.mk "g_fr", RGB_Rws.frob RGB_VertexData.Green)
val (_,rs') = rs' |> RGB_Theory.Ruleset.add_fresh_rule (R.mk "b_fr", RGB_Rws.frob RGB_VertexData.Blue)
val (_,rs') = rs' |> RGB_Theory.Ruleset.add_fresh_rule (R.mk "r_sp", RGB_Rws.special RGB_VertexData.Red)
val (_,rs') = rs' |> RGB_Theory.Ruleset.add_fresh_rule (R.mk "g_sp", RGB_Rws.special RGB_VertexData.Green)
val (_,rs') = rs' |> RGB_Theory.Ruleset.add_fresh_rule (R.mk "b_sp", RGB_Rws.special RGB_VertexData.Blue)

val redex = TagName.mk "r"
val rs' = fold (fn s => RGB_Theory.Ruleset.tag_rule (R.mk s) redex)
               ["r_fr","g_fr","b_fr","r_sp","g_sp","b_sp"] rs'

val rs = RS rs'

fun out (SYNTH s) = output_synth rgb_data s
  | out (RS rs) = output_ruleset rgb_data rs
  | out (RULE r) = output_rule rgb_data 
			       (RGB_Theory.Rule.get_lhs r)
			       (RGB_Theory.Rule.get_rhs r)
  | out (ERR e) = output_string e
  
*)
