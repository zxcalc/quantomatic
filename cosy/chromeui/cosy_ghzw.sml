datatype cosy = 
	 SYNTH of GHZW_DefaultSynth.T | 
	 RS of GHZW_Theory.Ruleset.T |
	 RULE of GHZW_Theory.Rule.T |
	 ERR of string

val ghzw_data : (GHZW_Theory.Graph.T, GHZW_Theory.Ruleset.T) TheoryData.T = {
  name = "GHZ/W",
  dotfun = GHZW_OutputGraphDot.output,
  gens = GHZW_Gens.gen_list 4 [GHZW_VertexData.GHZ, GHZW_VertexData.W],
  stats = GHZW_DefaultSynth.stats,
  rs_pairs =
    (rule_data GHZW_Theory.Rule.get_lhs GHZW_Theory.Rule.get_rhs) o
    GHZW_Theory.Ruleset.get_allrules
}

fun synth run = SYNTH (GHZW_DefaultSynth.synth (TheoryData.get_gens ghzw_data) run)
fun ruleset (SYNTH s) = RS (GHZW_RSBuilder.from_synth s)
fun update (SYNTH s) (RS rs) = RS (rs |> GHZW_RSBuilder.update s)
fun reduce (RS rs) = RS (GHZW_RSBuilder.reduce rs)
fun size (RS rs) = RuleName.NTab.cardinality (GHZW_Theory.Ruleset.get_allrules rs)
fun update_with run rs = rs |> update (synth run) |> reduce;
fun get_rule (RS rs) name = case GHZW_Theory.Ruleset.lookup_rule rs (RuleName.mk name)
			      of SOME r => RULE r 
			       | _ => ERR "Rule not found."

fun synth_list rs runs = fold update_with runs rs

val rs = RS GHZW_Theory.Ruleset.empty;

fun out (SYNTH s) = output_synth ghzw_data s
  | out (RS rs) = output_ruleset ghzw_data rs
  | out (RULE r) = output_rule ghzw_data 
			       (GHZW_Theory.Rule.get_lhs r)
			       (GHZW_Theory.Rule.get_rhs r)
  | out (ERR e) = output_string e
