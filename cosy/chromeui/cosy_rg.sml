structure Cosy =
struct
  structure Theory = RG_Theory
  structure Synth = RG_Synth
  structure RSBuilder = RG_RSBuilder
  structure Tensor = RG_TensorData.Tensor
  
  datatype T =
   SYNTH of Synth.T | 
   RS of Theory.Ruleset.T |
   RULE of Theory.Rule.T |
   GRAPH of Theory.Graph.T |
   ERR of string
   
  val output_dot = RG_OutputGraphDot.output
  val zero = LinratAngleExpr.zero
  val gens = RG_Gens.gen_list 3 [RG_InternVData.Xnd zero, RG_InternVData.Znd zero]
  
  local
    val rs' = RG_Theory.Ruleset.empty
    val (_,rs') = rs' |> RG_Theory.Ruleset.add_fresh_rule
                          (R.mk "r_fr", RG_Rws.frob (RG_InternVData.Xnd zero))
    val (_,rs') = rs' |> RG_Theory.Ruleset.add_fresh_rule
                          (R.mk "g_fr", RG_Rws.frob (RG_InternVData.Znd zero))
    val (_,rs') = rs' |> RG_Theory.Ruleset.add_fresh_rule
                          (R.mk "r_sp", RG_Rws.special (RG_InternVData.Xnd zero))
    val (_,rs') = rs' |> RG_Theory.Ruleset.add_fresh_rule
                          (R.mk "g_sp", RG_Rws.special (RG_InternVData.Znd zero))
    
    val redex = TagName.mk "r"
    val rs' = fold (fn s => RG_Theory.Ruleset.tag_rule (R.mk s) redex)
               ["r_fr","g_fr","r_sp","g_sp"] rs'
  in
    val initial_rs = RS rs'
  end
  
  val rule_matches_graph = RG_Enum.rule_matches_graph
end