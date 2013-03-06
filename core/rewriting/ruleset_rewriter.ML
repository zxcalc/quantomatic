signature RULESET_REWRITER_SHARING =
sig
  structure Ruleset : RULESET_SHARING
  type match
end;

signature RULESET_REWRITER =
sig
  structure Log : LOG

  structure Ruleset : RULESET
  type match

  structure Sharing : RULESET_REWRITER_SHARING
  sharing Sharing.Ruleset = Ruleset.Sharing;
  sharing type Sharing.match = match;

  (* Finds all rewrites for a graph using the active rules
   * in a ruleset
   *)
  val apply : Ruleset.T (* ruleset to use *)
           -> Ruleset.Rule.Graph.T (* graph to rewrite *)
           -> ((R.name * Ruleset.Rule.T) * Ruleset.Rule.Graph.T) Seq.seq;
              (* instantiated rule and new graph *)

  (* Finds all rewrites for a subgraph using the active rules
   * in a ruleset. Note that the match is actually performed
   * on the smallest open subgraph containing the given
   * vertices
   *)
  val apply_in : Ruleset.T (* ruleset to use *)
              -> Ruleset.Rule.Graph.T (* graph to rewrite *)
              -> V.NSet.T (* subgraph to match against *)
              -> ((R.name * Ruleset.Rule.T) * Ruleset.Rule.Graph.T) Seq.seq;
              (* instantiated rule and new graph *)

  (* As apply, but gives only the first result *)
  val apply_first : Ruleset.T
                 -> Ruleset.Rule.Graph.T 
                 -> ((R.name * Ruleset.Rule.T) * Ruleset.Rule.Graph.T) option;
  (* As apply_in, but gives only the first result *)
  val apply_first_in : Ruleset.T
                    -> Ruleset.Rule.Graph.T
                    -> V.NSet.T  
                    -> ((R.name * Ruleset.Rule.T) * Ruleset.Rule.Graph.T) option;

  (* Find all rewrites in a set of rules *)
  val applicable_rules : Ruleset.T (* background ruleset *)
      -> (Ruleset.Rule.T -> Ruleset.Rule.Graph.T -> Ruleset.Rule.T * match Seq.seq) (* matcher *)
      -> R.NSet.T (* subset of rules to use *) 
      -> Ruleset.Rule.Graph.T (* graph to look for matches of rules in *)
      -> ((R.name * Ruleset.Rule.T) * Ruleset.Rule.Graph.T) Seq.seq;
         (* instantiated rule and new graph *)

  (* a couple of helpful fns because match is opaque *)
  val print_match : match -> unit;
  val pretty_match : match -> Pretty.T;
end;

(* *)
functor RulesetRewriter(
  structure Ruleset : BANG_GRAPH_RULESET
  structure Rewriter : REWRITER
  sharing Rewriter.Rule.Sharing = Ruleset.Rule.Sharing
  sharing Rewriter.Graph.SharingOGraph = Ruleset.Rule.Graph.SharingOGraph
) : RULESET_REWRITER =
struct
  structure Log : LOG = Log(val init_level = 0)

  structure Ruleset = Ruleset;
  type match = Rewriter.match;

  structure Rule = Ruleset.BangGraphRule;
  structure G = Rule.BangGraph;

  fun log_p pretty_f lvl name =
    Log.logf lvl (fn g => Pretty.string_of
        (Pretty.chunks [Pretty.str (name^":"),(pretty_f g)]))

  val log_graph = log_p G.pretty
  val log_rule = log_p Rule.pretty
  
  (* apply rules to whole graph *)
  (* IMPROVE: provide a single name-space for all rules, and thus avoid 
     multiple renamings. *)
  fun applicable_rules rset matcher rnset g = let
    val _ = log_graph 4 "RulesetRewriter.applicable_rules: graph" g
  in
    R.NSet.fold
    (fn n =>
        let
          val rule = (Ruleset.get_rule rset n)
          val rulename = R.string_of_name n
          val _ = log_rule 2 ("Trying rule "^rulename) rule
          val (r,matches) = matcher rule g
          val _ = log_rule 3 "Rule instantiated as" r
          fun apply_rw m = let
            val _ = Log.log 1 ("Applying rule "^rulename)
            val (rule,g) = Rewriter.rewrite_at r m
          in
            ((n,rule),g)
          end
        in
          Seq.append (Seq.map apply_rw matches)
        end)
     rnset
     Seq.empty
  end;
  
  fun apply rset g = 
      applicable_rules rset Rewriter.find_matches 
       (Ruleset.get_active rset) g;

  fun apply_in rset g vset = 
  let
    fun find_matches rule graph =
        Rewriter.find_matches_in_subgraph rule graph vset
  in
      applicable_rules rset find_matches
        (Ruleset.get_active rset) g
  end;
  
  (* apply just first rule that can be applied *)
  fun apply_first rset g =
      case Seq.pull (apply rset g)
       of NONE => NONE
        | SOME (h,_) => SOME h;

  fun apply_first_in rset vset g = 
      case Seq.pull (apply_in rset vset g) 
       of NONE => NONE
        | SOME (h,_) => SOME h;

  val print_match = Rewriter.print_match;
  val pretty_match = Rewriter.pretty_match;

  structure Sharing : RULESET_REWRITER_SHARING
  = struct
    structure Ruleset = Ruleset.Sharing;
    type match = match;
  end


end; (* *)

