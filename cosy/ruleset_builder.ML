
signature RULESET_BUILDER =
sig
  structure Ruleset : RULESET
  structure Synthesize : SYNTHESIZE
  sharing Ruleset.Rule.Graph.SharingOGraph = Synthesize.Graph.SharingOGraph
  val update : Synthesize.T -> Ruleset.T -> Ruleset.T
  val from_synth : Synthesize.T -> Ruleset.T
  val reduce : Ruleset.T -> Ruleset.T
end

functor RulesetBuilder (
  structure RulesetRewrites : RULESET_REWRITES
  structure Synthesize : SYNTHESIZE
  sharing RulesetRewrites.Rule.Graph.SharingOGraph = Synthesize.Graph.SharingOGraph
) : RULESET_BUILDER =
struct
  structure Ruleset = RulesetRewrites.Ruleset
  structure Synthesize = Synthesize
  
  fun update (_,class_table) ruleset = let
    fun add_rule tag rhs lhs rs = let
      val (nm,rs') = rs |> Ruleset.add_fresh_rule (RuleName.mk "ra", Ruleset.Rule.mk (lhs,rhs))
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
  
  fun rule_matches_rule r1 r2 = case Seq.pull (RulesetRewrites.rule_matches r1 (Ruleset.Rule.get_lhs r2))
                                  of SOME _ => true
                                   | NONE => false
  
  (* iso of lhs and rhs need to respect boundaries *)
  fun bad_trivial r = let val rop = Ruleset.Rule.symmetric r
                      in rule_matches_rule r rop andalso rule_matches_rule rop r
                      end
  
  fun reduce rs = let
    val all_reds = Ruleset.get_rules_in_tag rs (TagName.mk "r")
    val all_congs = Ruleset.get_rules_in_tag rs (TagName.mk "c")
    fun iso_filter rn (rs,congs) = case Ruleset.lookup_rule rs rn
      of SOME r => if bad_trivial r then (rs|> Ruleset.delete_rule rn,congs|>RuleName.NSet.delete rn) else (rs,congs)
       | NONE   => (rs,congs)
    val (rs,congs) = RuleName.NSet.fold iso_filter all_congs (rs,all_congs)
    fun filter rn1 rn2 rs = case (Ruleset.lookup_rule rs rn1, Ruleset.lookup_rule rs rn2)
                              of (SOME r1, SOME r2) => if rule_matches_rule r1 r2
                                                       then rs |> Ruleset.delete_rule rn2
                                                       else rs
                               | _ => rs
    fun reduce' reds rs = case RuleName.NSet.get_min reds
                            of SOME red => let val new_reds = reds |> RuleName.NSet.delete red
                                           in reduce' new_reds
                                               (RuleName.NSet.fold (filter red) new_reds
                                                (RuleName.NSet.fold (filter red) congs
                                                 rs))
                                           end
                             | NONE => rs
  in reduce' all_reds rs
  end
  
end