
signature RULESET_BUILDER =
sig
  structure GraphEnum : GRAPH_ENUMERATOR
  structure Ruleset : RULESET
  structure Synthesize : SYNTHESIZE
  sharing GraphEnum.Graph.SharingBangGraph =
          Ruleset.Rule.Graph.SharingBangGraph =
          Synthesize.Graph.SharingBangGraph
  val update : Synthesize.T -> Ruleset.T -> Ruleset.T
  val from_synth : Synthesize.T -> Ruleset.T
  val reduce : Ruleset.T -> Ruleset.T
  val rule_matches_rule : Ruleset.Rule.T -> Ruleset.Rule.T -> bool
end

functor RulesetBuilder (
  structure Synthesize : SYNTHESIZE
) : RULESET_BUILDER =
struct
  structure Synthesize = Synthesize
  structure GraphEnum = Synthesize.GraphEnum
  structure Ruleset = GraphEnum.Ruleset
  structure Rule = Ruleset.Rule
  
  fun update synth ruleset = let
    fun add_rule tag rhs lhs rs = let
      val tryname = RuleName.mk ((TagName.string_of_name tag) ^ "_a")
      val (nm,rs') = rs |> Ruleset.add_fresh_rule (tryname, Rule.mk (lhs,rhs))
    in rs' |> Ruleset.tag_rule nm tag
    end
    fun process_class class rs =
      case (EqClass.get_rep class)
        of SOME rep =>
             fold (add_rule (TagName.mk "c") rep) (EqClass.get_congs class)
               (fold (add_rule (TagName.mk "r") rep) (EqClass.get_redexes class) rs)
         | NONE => rs
  in Synthesize.eqclass_fold (process_class o snd) synth ruleset
  end
  
  fun from_synth synth = Ruleset.empty |> update synth
  
  fun rule_matches_rule r1 r2 = GraphEnum.rule_matches_graph r1 (Rule.get_lhs r2)
  
  (*fun matches_with_boundary pattern target = let
    val m = Match.init (Vertex.NSet.empty, Edge.NSet.empty, Graph.BBox.NSet.empty)
                       pattern target
    val m = Vertex.NSet.fold (fn b => Match.match_boundary b b) (Graph.get_boundary pattern) m
    val gs = [pattern, target]
    val names = (Vertex.NSet.empty,Edge.NSet.empty,Graph.BBox.NSet.empty)
  in case Seq.pull (SimpleMatchSearch.match' m pattern target Vertex.NTab.empty)
       of NONE=>false | _=>true
  end*)
  
  (* iso of lhs and rhs need to respect boundaries *)
  fun cong_trivial r = Rule.GraphIso.test (Rule.get_lhs r) (Rule.get_rhs r)
  
  
  fun has_match rs target_rn =
    GraphEnum.has_match rs (Ruleset.get_rules_in_tag rs (TagName.mk "r") |> RuleName.NSet.delete target_rn)
                           (Rule.get_lhs (Ruleset.get_rule rs target_rn))
  
  fun reduce ruleset = let
    val all_rules = RuleName.NTab.get_nameset o Ruleset.get_allrules
    fun iso_filter rn rs = if cong_trivial (Ruleset.get_rule rs rn) then rs |> Ruleset.delete_rule rn else rs
    fun match_filter rn rs = if has_match rs rn
                             then rs |> Ruleset.delete_rule rn else rs
    val ruleset' = RuleName.NSet.fold iso_filter (Ruleset.get_rules_in_tag ruleset (TagName.mk "c")) ruleset
  in RuleName.NSet.fold match_filter (all_rules ruleset') ruleset'
  end
  
end
