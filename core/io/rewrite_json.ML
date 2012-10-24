(**
 * JSON output of rewrites
 *
 * REWRITE_LIST ::= [ REWRITE (, REWRITE)* ] | []
 * REWRITE ::=
 * {
 *   "rule": RULE_N,
 *   "rewritten_graph": GRAPH
 * }
 * RULE_N ::=
 * {
 *   "name": NAME,
 *   "lhs": GRAPH,
 *   "rhs": GRAPH,
 *   "annotation": ANNDATA
 * }
 *
 * See graph_json.ML and rule_json.ML for details.
 *)

functor OutputAnnotatedRewriteJSON(
  structure Rule : BASIC_RULE
  structure OutputAnnotation : RULE_ANN_OUTPUT_JSON
  structure OutputGraph : OUTPUT_JSON where type data = (Rule.Graph.T * OutputAnnotation.GraphAnnOutput.data)
) : OUTPUT_JSON
= struct
  structure OG = OutputGraph;
  structure OA = OutputAnnotation;

  open JsonOutputUtils;
  type data = ((R.name * Rule.T) * OA.data) * OG.data;

  fun output_rule ((rn,rule),ann) = let
    val la = OA.get_lhs_annotation ann
    val ra = OA.get_rhs_annotation ann
    val lhs = Rule.get_lhs rule
    val rhs = Rule.get_rhs rule
  in
    Json.Object (
      Json.empty_obj |> update ("name",Json.String (R.dest rn))
                     |> update ("lhs",(OG.output (lhs,la)))
                     |> update ("rhs",(OG.output (rhs,ra)))
                     |> update ("annotation",(OA.output_rule_annotation ann))
    )
  end;

  fun output (r,g) =
    Json.Object (
      Json.empty_obj |> update ("rule",output_rule r)
                     |> update ("rewritten_graph",(OG.output g))
    );
end;

functor OutputRewriteJSON(
  structure Rule : BASIC_RULE
  structure OutputGraph : OUTPUT_JSON
  sharing type OutputGraph.data = Rule.Graph.T
) : OUTPUT_JSON
= struct
  structure OG = OutputGraph;

  open JsonOutputUtils;
  type data = (R.name * Rule.T) * Rule.Graph.T;

  fun output_rule (rn,rule) =
    Json.Object (
      Json.empty_obj |> update ("name",Json.String (R.dest rn))
                     |> update ("lhs",(OG.output (Rule.get_lhs rule)))
                     |> update ("rhs",(OG.output (Rule.get_rhs rule)))
    );

  fun output (r,g) =
    Json.Object (
      Json.empty_obj |> update ("rule",output_rule r)
                     |> update ("rewritten_graph",(OG.output g))
    );
end;

functor OutputAnnotatedRewriteListJSON(
  structure Rule : BASIC_RULE
  structure OutputAnnotation : RULESET_ANN_OUTPUT_JSON
  structure OutputRewrite : OUTPUT_JSON
    where type data = ((R.name * Rule.T) * OutputAnnotation.RuleAnnOutput.data)
                      * (Rule.Graph.T * OutputAnnotation.RuleAnnOutput.GraphAnnOutput.data);
) : OUTPUT_JSON
= struct
  structure OA = OutputAnnotation;
  structure OGA = OA.RuleAnnOutput.GraphAnnOutput;

  open JsonOutputUtils;
  type data = ((R.name * Rule.T) * (Rule.Graph.T * OGA.data)) list * OA.data;

  fun output_rewrite ann (r as (rn,_),g) =
    OutputRewrite.output ((r,OA.get_rule_annotation ann rn),g);

  fun output (rws,ann) =
    Json.Array (map (output_rewrite ann) rws);
end;


