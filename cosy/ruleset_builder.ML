signature RULESET_BUILDER =
sig
  structure Theory : GRAPHICAL_THEORY
  structure EqClassTab : EQ_CLASS_TAB
  val get_ruleset : EqClassTab.T -> Theory.Ruleset.T
end


functor RulesetBuilder(
  structure Theory : GRAPHICAL_THEORY
  structure EqClassTab : EQ_CLASS_TAB
  sharing Theory.Graph.SharingOGraph =
          EqClassTab.GraphEntry.Equiv.Graph.SharingOGraph) =
struct
  structure Theory = Theory
  structure EqClassTab = EqClassTab
  
  fun get_ruleset eq_tab =
  let
    val tag_redex = TagName.mk "redex"
    val tag_cong = TagName.mk "cong"
    fun add_rule pair is_redex rs =
    let
      val (nm, rs') = rs |> Theory.Ruleset.add_fresh_rule
        (R.mk (if is_redex then "r_0" else "c_0"), Theory.Rule.mk pair)
    in
      rs' |> Theory.Ruleset.tag_rule nm
        (if is_redex then tag_redex else tag_cong)
    end
  in
    EqClassTab.fold_rule_pairs add_rule eq_tab Theory.Ruleset.empty
  end 
end


(* signature RULESET_BUILDER =
sig
  structure Synthesize : SYNTHESIZE
  
  val update : Synthesize.T -> Synthesize.GraphEnum.Theory.Ruleset.T -> Synthesize.GraphEnum.Theory.Ruleset.T
  val from_synth : Synthesize.T -> Synthesize.GraphEnum.Theory.Ruleset.T
  val reduce : Synthesize.GraphEnum.Theory.Ruleset.T -> Synthesize.GraphEnum.Theory.Ruleset.T
  val rule_matches_rule : Synthesize.GraphEnum.Theory.Ruleset.Rule.T -> Synthesize.GraphEnum.Theory.Ruleset.Rule.T -> bool
end

functor RulesetBuilder (
  structure Synthesize : SYNTHESIZE
) : RULESET_BUILDER =
struct
  structure Synthesize = Synthesize
  structure GraphEnum = Synthesize.GraphEnum
  structure Theory = GraphEnum.Theory
  structure Ruleset = Theory.Ruleset
  structure Rule = Theory.Rule
  structure HomeoFinder = BangGraphHomeomorphismSearcher(Theory.Graph)
  
  fun update synth ruleset = let
    fun add_rule tag rhs lhs rs = let
      (*val tryname = R.mk ((TagName.string_of_name tag) ^ "_a")*)
      val (nm,rs') = rs |> Ruleset.add_fresh_rule (Rule.mk (lhs,rhs))
    in rs' |> Ruleset.tag_rule nm tag
    end
    fun process_class class rs =
      case (EqClass.get_rep class)
        of SOME rep =>
             fold (add_rule (TagName.mk "c") rep) (EqClass.get_congs class)
               (fold (add_rule (TagName.mk "r") rep) (EqClass.get_redexes class) rs)
         | NONE => rs
  in Synthesize.eqclass_fold process_class synth ruleset
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
  fun cong_trivial r = HomeoFinder.is_homeomorphic (Rule.get_lhs r) (Rule.get_rhs r)
  
  
  fun has_match rs target_rn =
    GraphEnum.has_match rs (Ruleset.get_rules_in_tag rs (TagName.mk "r") |> R.NSet.delete target_rn)
                           (Rule.get_lhs (Ruleset.get_rule rs target_rn))
  
  fun reduce ruleset = let
    val all_rules = RTab.get_dom_set o Ruleset.get_allrules
    fun iso_filter rn rs = if cong_trivial (Ruleset.get_rule rs rn) then rs |> Ruleset.delete_rule rn else rs
    fun match_filter rn rs = if has_match rs rn
                             then rs |> Ruleset.delete_rule rn else rs
    val ruleset' = R.NSet.fold iso_filter (Ruleset.get_rules_in_tag ruleset (TagName.mk "c")) ruleset
  in R.NSet.fold match_filter (all_rules ruleset') ruleset'
  end
  
end *)
