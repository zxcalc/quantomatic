(**
 * JSON ruleset parsing and writing
 *
 * RULESET ::=
 * {
 *   "rules": RULE_LIST,
 *   "active_rules": RULE_NAME_LIST,
 *   "tags": TAG_LIST
 *   "annotation": ANNDATA
 * }
 * RULE_LIST ::= { NAME : RULE (, NAME : RULE)* } | {}
 * TAG_LIST ::= { NAME : RULE_NAME_LIST (, NAME : RULE_NAME_LIST)* } | {}
 * RULE_NAME_LIST ::= [ NAME (, NAME)* ] | []
 *
 * See rule_json.ML and graph_json.ML for details.  All fields are optional,
 * but any rules references from "active_rules" or "tags" must be in "rules".
 *)

signature RULESET_ANN_INPUT_JSON
= sig
  structure RuleAnnInput : RULE_ANN_INPUT_JSON;
  exception bad_input_exp of string*string;
  type data;
  val empty_annotation : data;
  val set_rule_annotation : R.name -> RuleAnnInput.data -> data -> data;
  val input_ruleset_annotation : Json.json -> data -> data;
end;

structure EmptyRulesetAnnotationInputJson
  : RULESET_ANN_INPUT_JSON where type data = unit
= struct
  structure RuleAnnInput = EmptyRuleAnnotationInputJson;
  exception bad_input_exp = RuleAnnInput.bad_input_exp;
  type data = unit;
  val empty_annotation = ();
  fun set_rule_annotation _ _ = I;
  fun input_ruleset_annotation _ = I;
end;

signature RULESET_ANN_OUTPUT_JSON
= sig
  structure RuleAnnOutput : RULE_ANN_OUTPUT_JSON;
  type data;
  val empty_annotation : data;
  val get_rule_annotation : data -> R.name -> RuleAnnOutput.data;
  val output_ruleset_annotation : data -> Json.json;
end;

structure EmptyRulesetAnnotationOutputJson
  : RULESET_ANN_OUTPUT_JSON where type data = unit
= struct
  structure RuleAnnOutput = EmptyRuleAnnotationOutputJson;
  type data = unit;
  val empty_annotation = ();
  fun get_rule_annotation _ _ = RuleAnnOutput.empty_annotation;
  fun output_ruleset_annotation _ = Json.Null;
end;

functor InputAnnotatedRulesetJSON(
  structure Ruleset : RULESET
  structure InputAnnotation : RULESET_ANN_INPUT_JSON
  structure InputRule : INPUT_JSON where type data = (Ruleset.Rule.T * InputAnnotation.RuleAnnInput.data)
) : INPUT_JSON
= struct
  structure IA = InputAnnotation;

  open JsonInputUtils;
  type data = Ruleset.T * IA.data;

  fun get_ann obj prop (rs,ann) =
        (rs, IA.input_ruleset_annotation (get_easy Json.Null obj prop) ann)
        handle IA.bad_input_exp (m,l) =>
          raise bad_input_exp (m,prepend_prop prop l)

  fun map_rule (n,v) (tab,ann) = let
    val (rule,r_ann) = InputRule.input v
      handle InputRule.bad_input_exp (m,l) =>
        raise bad_input_exp (m,prepend_prop n l)
    val rn = R.mk n
  in
    (R.NTab.ins (rn,rule) tab,
     IA.set_rule_annotation rn r_ann ann)
  end

  fun input_rules obj prop (ruleset,ann) =
        case get_easy Json.Null obj prop
          of Json.Object obj =>
              let
                val (rules,ann') = (Json.fold map_rule obj (R.NTab.empty,ann)
                                    handle bad_input_exp (m,l) =>
                                      raise bad_input_exp (m,prepend_prop prop l))
              in
                (Ruleset.set_allrules rules ruleset,ann')
              end
           | Json.Null => (ruleset,ann)
           | _ => raise bad_input_exp ("Expected object","");

  fun map_tag (n,(Json.Array a)) =
        let
          fun map_rulename (Json.String s) = R.mk s
            | map_rulename _ = raise bad_input_exp ("Expected strings in array","")
        in
          TagRel.add_to_cod (TagName.mk n) (R.NSet.of_list (map map_rulename a))
        end
    | map_tag _ = raise bad_input_exp ("Expected array","")

  fun input_tags obj prop =
        case get_easy Json.Null obj prop
          of Json.Object obj =>
              let
                val tags = (Json.fold map_tag obj TagRel.empty
                           handle bad_input_exp (m,l) =>
                             raise bad_input_exp (m,prepend_prop prop l))
              in
                Ruleset.set_brel tags
              end
           | Json.Null => I
           | _ => raise bad_input_exp ("Expected object","");

  fun input_active obj prop = let
    fun map_rulename (Json.String s) = R.mk s
      | map_rulename _ = raise bad_input_exp ("Expected strings in array","")
    val a = get_array_easy obj prop
  in
    Ruleset.set_active (R.NSet.of_list (map map_rulename a))
  end

  fun input (Json.Object obj) =
        (Ruleset.empty,IA.empty_annotation)
            |> get_ann obj "annotation"
            |> input_rules obj "rules"
            |> apfst (input_tags obj "tags")
            |> apfst (input_active obj "active_rules")
    | input Json.Null = (Ruleset.empty,IA.empty_annotation)
    | input _ = raise bad_input_exp ("Expected object","");
end;

functor InputRulesetJSON(
  structure Ruleset : RULESET
  structure InputRule : INPUT_JSON
  sharing type InputRule.data = Ruleset.Rule.T
) : INPUT_JSON
= struct
  structure InputAnnRule : INPUT_JSON =
  struct
    open JsonInputUtils;
    type data = InputRule.data*unit;
    fun input json = (InputRule.input json,())
  end;

  structure SubInput = InputAnnotatedRulesetJSON(
    structure Ruleset = Ruleset
    structure InputAnnotation = EmptyRulesetAnnotationInputJson
    structure InputRule  = InputAnnRule
  );
  open SubInput;
  type data = Ruleset.T;
  val input = fst o SubInput.input;
end;

functor OutputAnnotatedRulesetJSON(
  structure Ruleset : RULESET
  structure OutputAnnotation : RULESET_ANN_OUTPUT_JSON
  structure OutputRule : OUTPUT_JSON where type data = (Ruleset.Rule.T * OutputAnnotation.RuleAnnOutput.data)
) : OUTPUT_JSON
= struct
  structure OA = OutputAnnotation;

  open JsonOutputUtils;
  type data = Ruleset.T * OA.data;

  val rule_list = Json.Array o (map (Json.String o R.dest)) o R.NSet.list_of;

  fun output_rules (rs,ann) = let
    val ruletab = Ruleset.get_allrules rs
    val get_ann = OA.get_rule_annotation ann
    fun upd_rule (r,v) = update (R.dest r,OutputRule.output (v,get_ann r))
  in
    Json.Object (R.NTab.fold upd_rule ruletab Json.empty_obj)
  end

  fun output_tags rs = let
    val tagtab = TagRel.get_codtab (Ruleset.get_tagrel rs)
    fun upd_tag (t,v) = update (TagName.dest t,rule_list v)
  in
    Json.Object (TagName.NTab.fold upd_tag tagtab Json.empty_obj)
  end

  val output_active_rules = rule_list o Ruleset.get_active;

  fun output (rs,ann) =
    Json.Object (
      Json.empty_obj |> update ("rules",output_rules (rs,ann))
                     |> update ("tags",output_tags rs)
                     |> update ("active_rules",output_active_rules rs)
                     |> update ("annotation",OA.output_ruleset_annotation ann)
    );
end;

functor OutputRulesetJSON(
  structure Ruleset : RULESET
  structure OutputRule : OUTPUT_JSON
  sharing type OutputRule.data = Ruleset.Rule.T
) : OUTPUT_JSON
= struct
  structure OutputAnnRule : OUTPUT_JSON =
  struct
    open JsonOutputUtils;
    type data = OutputRule.data*unit;
    val output = OutputRule.output o fst
  end;

  structure SubOutput = OutputAnnotatedRulesetJSON(
    structure Ruleset = Ruleset
    structure OutputAnnotation = EmptyRulesetAnnotationOutputJson
    structure OutputRule  = OutputAnnRule
  );
  open SubOutput;
  type data = Ruleset.T;
  fun output r = SubOutput.output (r,());
end;


