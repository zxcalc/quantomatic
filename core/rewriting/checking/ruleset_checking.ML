(* Code to check a ruleset:
 * the goal is to check the ruleset to see if there are cycles.
 * This won't prevent infinite loops from occuring at rewrite time but
 * at least there won't be any trivial mistakes *)
(* Idea is simply to build a graph whose vertices are rules
 * LHSs and RHSs and whose edges are rules
 *)
signature RSET_CHECKING =
sig
  structure GTheory : GRAPHICAL_THEORY
  structure Ruleset : RULESET
  structure Rule : RULE
  structure RSetRewrites : RULESET_REWRITES
  structure RewriteGraph : RGRAPH 

  type T

  val init : T
  val set_rset : Ruleset.T -> T -> T
  val get_rgraph : T -> RewriteGraph.T
  val do_checking : T -> T

  exception no_active_rules_exp of string
  exception name_conflict_exp of string
end;

functor RSetChecking (
  GTheory : GRAPHICAL_THEORY
) : RSET_CHECKING = struct

  structure GTheory = GTheory;
  structure Ruleset = GTheory.Ruleset;
  structure Rule = GTheory.Rule;
  structure RSetRewrites = GTheory.RulesetRewrites;
  structure RewriteGraph = RGraph(structure RSetRewrites = RSetRewrites);

  exception no_active_rules_exp of string
  exception name_conflict_exp of string

  datatype T = RSetChecking of {
    rset : Ruleset.T,
    rgraph : RewriteGraph.T
  };

  (* Basic functions *)

  fun update_rset f (RSetChecking rsetchk) = 
    RSetChecking { rset = (f (#rset rsetchk)), rgraph = #rgraph rsetchk };
  fun update_rgraph f (RSetChecking rsetchk) =
    RSetChecking { rset = #rset rsetchk, rgraph = f (#rgraph rsetchk)};
  val set_rset = update_rset o K;
  val set_rgraph = update_rgraph o K;

  fun get_rset (RSetChecking {rset = r,...}) = r;
  fun get_rgraph (RSetChecking {rgraph = r,...}) = r;

  val init = RSetChecking {rset = Ruleset.empty, rgraph = RewriteGraph.init};

  (* Auxiliary functions *)
  fun assert_ruleset_has_active_rules rset =
    if (R.NSet.is_empty o Ruleset.get_active) rset
      then raise no_active_rules_exp "assert_ruleset_has_active_rules"
      else ()

  fun flip f x y = f y x
  fun rname_to_graphname suffix name =
    let
      val s_name = (R.dest name) ^ "_" ^ suffix
    in
      GraphName.mk s_name
    end;

  (* For a given rule, get all the rules that match its LHS *)
  fun lookup_matches getter rset rnames rname matches =
    let
      val xhs = (getter o (Ruleset.get_rule rset)) rname
      (* Now get the rules that give a match for that LHS *)
      (* Seq gives us a nice lazy way to get matches: do not use
       * applicable_rules' which would force us to evaluate the whole
       * thing in order to remove duplicates. Use rule_matches instead *)
      fun rule_matches current_rulename ntab = 
        let
          val current_rule = Ruleset.get_rule rset current_rulename 
        in
        case (Seq.pull o snd o (flip RSetRewrites.rule_matches xhs))
                  current_rule    
          of SOME m => (case (R.NTab.try_ins (current_rulename, 
                                              (current_rule,fst m)) ntab)
                          of SOME tab => tab
                          (* Should never happen *)
                           | NONE => raise name_conflict_exp "lookup_matches")
           | NONE => ntab
        end
      val matching_rules = R.NSet.fold (rule_matches) rnames R.NTab.empty
    in 
      (case R.NTab.try_ins (rname, matching_rules) matches
        of SOME tab => tab
        | NONE => raise name_conflict_exp "lookup_matches")
    end; 

  fun add_xhs rset name rgraph =
    let
      val rule = Ruleset.get_rule rset name
      val lhs = Rule.get_lhs rule
      val rhs = Rule.get_rhs rule
    in
      rgraph |> RewriteGraph.add_named_vertex lhs (rname_to_graphname "lhs" name)
             |> snd
             |> RewriteGraph.add_named_vertex rhs (rname_to_graphname "rhs" name)
             |> snd
    end;

  fun add_edge s1 s2 rset (n1,tab) rgraph =
    let
     fun add (n2, r) rg = RewriteGraph.add_edge "other" ((rname_to_graphname s1 n1,
                                                        rname_to_graphname s2 n2), (n2, r)) rg
                          |> snd
    in
      rgraph |> R.NTab.fold add tab
    end; 

  fun do_checking rsetchk = 
    let
     val rset = get_rset rsetchk
     val () = assert_ruleset_has_active_rules rset
     val activerulenames = Ruleset.get_active rset
    
     (* Compute the "edges"*)
     val matches_lhs = R.NSet.fold (lookup_matches Rule.get_lhs rset activerulenames) activerulenames
                                                        R.NTab.empty
     val matches_rhs = R.NSet.fold (lookup_matches Rule.get_rhs rset activerulenames) activerulenames
                                                        R.NTab.empty
     val rgraph = RewriteGraph.init |> R.NSet.fold (add_xhs rset) activerulenames 
                                    |> R.NTab.fold (add_edge "lhs" "rhs" rset) matches_lhs
                                    |> R.NTab.fold (add_edge "rhs" "rhs" rset) matches_rhs
    in
      rsetchk |> set_rgraph rgraph
    end;
end;
