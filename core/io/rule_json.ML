(**
 * JSON rule parsing and writing
 *
 * RULE ::=
 * {
 *   "lhs": GRAPH,
 *   "rhs": GRAPH,
 *   "annotation": ANNDATA
 * }
 *
 * See graph_json.ML for details.  "lhs" and "rhs" are required, but
 * "annotation" is optional.
 *)

signature RULE_ANN_INPUT_JSON
= sig
  structure GraphAnnInput : GRAPH_ANN_INPUT_JSON;
  exception bad_input_exp of string*string;
  type data;
  val empty_annotation : data;
  val set_lhs_annotation : GraphAnnInput.data -> data -> data;
  val set_rhs_annotation : GraphAnnInput.data -> data -> data;
  val input_rule_annotation : Json.json -> data -> data;
end;

structure EmptyRuleAnnotationInputJson
  : RULE_ANN_INPUT_JSON where type data = unit
= struct
  structure GraphAnnInput = EmptyGraphAnnotationInputJson;
  exception bad_input_exp = GraphAnnInput.bad_input_exp;
  type data = unit;
  val empty_annotation = ();
  fun set_lhs_annotation _ = I;
  fun set_rhs_annotation _ = I;
  fun input_rule_annotation _ = I;
end;

signature RULE_ANN_OUTPUT_JSON
= sig
  structure GraphAnnOutput : GRAPH_ANN_OUTPUT_JSON;
  type data;
  val empty_annotation : data;
  val get_lhs_annotation : data ->  GraphAnnOutput.data;
  val get_rhs_annotation : data ->  GraphAnnOutput.data;
  val output_rule_annotation : data -> Json.json;
end;

structure EmptyRuleAnnotationOutputJson
  : RULE_ANN_OUTPUT_JSON where type data = unit
= struct
  structure GraphAnnOutput = EmptyGraphAnnotationOutputJson;
  type data = unit;
  val empty_annotation = ();
  fun get_lhs_annotation _ = GraphAnnOutput.empty_annotation;
  fun get_rhs_annotation _ = GraphAnnOutput.empty_annotation;
  fun output_rule_annotation _ = Json.Null;
end;

functor InputAnnotatedRuleJSON(
  structure Rule : RULE
  structure InputAnnotation : RULE_ANN_INPUT_JSON
  structure InputGraph : INPUT_JSON where type data = (Rule.Graph.T * InputAnnotation.GraphAnnInput.data)
) : INPUT_JSON
= struct
  structure IA = InputAnnotation;

  open JsonInputUtils;
  type data = Rule.T * IA.data;

  fun get_ann obj prop ann =
        IA.input_rule_annotation (get_easy Json.Null obj prop) ann
        handle IA.bad_input_exp (m,l) =>
          raise bad_input_exp (m,prepend_prop prop l)

  fun input (Json.Object obj) =
        let
          val (lhs,la) = InputGraph.input (get obj "lhs")
                         handle InputGraph.bad_input_exp (m,l) =>
                           raise bad_input_exp (m,prepend_prop "lhs" l);
          val (rhs,ra) = InputGraph.input (get obj "rhs")
                         handle InputGraph.bad_input_exp (m,l) =>
                           raise bad_input_exp (m,prepend_prop "rhs" l);
          val ann = IA.empty_annotation |> get_ann obj "annotation"
                                        |> IA.set_lhs_annotation la
                                        |> IA.set_rhs_annotation ra
        in
          (Rule.mk (lhs,rhs),ann)
          handle Rule.bad_rule_exp (msg,_,_) =>
            raise bad_input_exp ("Invalid rule: "^msg,"")
        end
    | input _ = raise bad_input_exp ("Expected object","");
end;

functor InputRuleJSON(
  structure Rule : RULE
  structure InputGraph : INPUT_JSON
  sharing type InputGraph.data = Rule.Graph.T
) : INPUT_JSON
= struct
  open JsonInputUtils;
  type data = Rule.T;

  fun input (Json.Object obj) =
        let
          val lhs = InputGraph.input (get obj "lhs")
                    handle InputGraph.bad_input_exp (m,l) =>
                      raise bad_input_exp (m,prepend_prop "lhs" l);
          val rhs = InputGraph.input (get obj "rhs")
                    handle InputGraph.bad_input_exp (m,l) =>
                      raise bad_input_exp (m,prepend_prop "rhs" l);
        in
          Rule.mk (lhs,rhs)
          handle Rule.bad_rule_exp (msg,_,_) =>
            raise bad_input_exp ("Invalid rule: "^msg,"")
        end
    | input _ = raise bad_input_exp ("Expected object","");
end;

functor OutputAnnotatedRuleJSON(
  structure Rule : RULE
  structure OutputAnnotation : RULE_ANN_OUTPUT_JSON
  structure OutputGraph : OUTPUT_JSON where type data = (Rule.Graph.T * OutputAnnotation.GraphAnnOutput.data)
) : OUTPUT_JSON
= struct
  structure OG = OutputGraph;
  structure OA = OutputAnnotation;

  open JsonOutputUtils;
  type data = Rule.T * OA.data;

  fun output (r,ann) = let
    val la = OA.get_lhs_annotation ann
    val ra = OA.get_rhs_annotation ann
    val lhs = Rule.get_lhs r
    val rhs = Rule.get_rhs r
  in
    Json.Object (
      Json.empty_obj |> update ("lhs",(OG.output (lhs,la)))
                     |> update ("rhs",(OG.output (rhs,ra)))
                     |> update ("annotation",(OA.output_rule_annotation ann))
    )
  end;
end;

functor OutputRuleJSON(
  structure Rule : RULE
  structure OutputGraph : OUTPUT_JSON
  sharing type OutputGraph.data = Rule.Graph.T
) : OUTPUT_JSON
= struct
  structure OG = OutputGraph;

  open JsonOutputUtils;
  type data = Rule.T;

  fun output r =
        Json.Object (
          Json.empty_obj |> update ("lhs",(OG.output (Rule.get_lhs r)))
                         |> update ("rhs",(OG.output (Rule.get_rhs r)))
        );
end;


