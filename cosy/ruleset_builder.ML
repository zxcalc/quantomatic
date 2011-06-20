
signature RULESET_BUILDER =
sig
  structure Thy : GRAPHICAL_THEORY
  structure Synthesize : SYNTHESIZE
  sharing Thy.Graph.SharingBangGraph = Synthesize.Graph.SharingBangGraph
  val update : Synthesize.T -> Thy.Ruleset.T -> Thy.Ruleset.T
  val from_synth : Synthesize.T -> Thy.Ruleset.T
  val reduce : Thy.Ruleset.T -> Thy.Ruleset.T
  val rule_matches_rule : Thy.Ruleset.Rule.T -> Thy.Ruleset.Rule.T -> bool
end

functor RulesetBuilder (
  structure Thy : GRAPHICAL_THEORY
  structure Synthesize : SYNTHESIZE
  sharing Thy.Graph.SharingBangGraph = Synthesize.Graph.SharingBangGraph
) : RULESET_BUILDER =
struct
  structure Synthesize = Synthesize
  structure Thy = Thy
  open Thy
  
  
  fun update (_,class_table) ruleset = let
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
  in Tensor.Tab.fold (process_class o snd) class_table ruleset
  end
  
  fun from_synth synth = Ruleset.empty |> update synth
  
  fun rule_matches_rule r1 r2 = case Seq.pull (RulesetRewrites.rule_matches r1 (Rule.get_lhs r2))
                                  of NONE=>false | _=>true
  
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
  
  fun reduce rs = let
    val all_reds = Ruleset.get_rules_in_tag rs (TagName.mk "r")
    val all_congs = Ruleset.get_rules_in_tag rs (TagName.mk "c")
    fun iso_filter rn (rs,congs) = case Ruleset.lookup_rule rs rn
      of SOME r => if cong_trivial r then (rs |> Ruleset.delete_rule rn, congs |> RuleName.NSet.delete rn) else (rs,congs)
       | NONE   => (rs,congs)
    val (rs,congs) = RuleName.NSet.fold iso_filter all_congs (rs,all_congs)
    fun filter rn1 rn2 rs = 
    	if RuleName.name_eq (rn1,rn2) then rs
    	else case (Ruleset.lookup_rule rs rn1, Ruleset.lookup_rule rs rn2)
                   of (SOME r1, SOME r2) => if rule_matches_rule r1 r2
                                            then rs |> Ruleset.delete_rule rn2
                                            else rs
                    | _ => rs
    fun reduce' reds rs =
      case RuleName.NSet.get_min reds
        of SOME red => reduce' (reds |> RuleName.NSet.delete red)
						             (RuleName.NSet.fold (filter red) all_reds
					                (RuleName.NSet.fold (filter red) congs rs))
         | NONE => rs
  in reduce' all_reds rs
  end
  
end
