functor InputRulesetAnnotationsJSON(
  structure Annotations : RULESET_ANNOTATIONS
  val json_to_annotation : Json.json -> Annotations.data
) : RULESET_ANN_INPUT_JSON
= struct
  open JsonInputUtils;
  structure RA = Annotations;
  structure GA = RA.GraphAnnotations;
  
  type data = RA.T;
  val empty_annotation = RA.init;

  structure RuleAnnInput : RULE_ANN_INPUT_JSON
  = struct
    open JsonInputUtils;
    structure GraphAnnInput = InputGraphAnnotationsJSON(
      structure Annotations = GA
      val json_to_annotation = json_to_annotation
    );
    type data = GA.data * (GA.T * GA.T);
    val empty_annotation = (GA.empty_data,(GA.init,GA.init));
    fun set_lhs_annotation ga (ra,(_,rhs)) = (ra,(ga,rhs));
    fun set_rhs_annotation ga (ra,(lhs,_)) = (ra,(lhs,ga));
    fun input_rule_annotation js (_,ga) = (json_to_annotation js,ga);
  end;
  
  fun set_rule_annotation r (ra,(lhs,rhs)) =
    (RA.set_rule_annotation r ra)
    o (RA.set_rule_lhs_annotation r lhs)
    o (RA.set_rule_rhs_annotation r rhs);
  val input_ruleset_annotation = RA.set_ruleset_annotation o json_to_annotation;
end;

structure InputRulesetStringTableAnnotationsJSON
  : RULESET_ANN_INPUT_JSON where type data = RulesetStringTableAnnotations.T
= struct
  open JsonInputUtils;
  
  structure InputAnn = InputRulesetAnnotationsJSON(
    structure Annotations = RulesetStringTableAnnotations
    val json_to_annotation = input_string_table
  );
  open InputAnn;
end;

structure InputRulesetJsonObjectAnnotationsJSON
  : RULESET_ANN_INPUT_JSON where type data = RulesetJsonObjectAnnotations.T
= InputRulesetAnnotationsJSON(
  structure Annotations = RulesetJsonObjectAnnotations
  fun json_to_annotation Json.Null = Json.empty_obj
    | json_to_annotation (Json.Object obj) = obj
    | json_to_annotation _ = raise JsonInputUtils.bad_input_exp ("Expected object","")
)

functor OutputRulesetAnnotationsJSON(
  structure Annotations : RULESET_ANNOTATIONS
  val annotation_to_json : Annotations.data -> Json.json
) : RULESET_ANN_OUTPUT_JSON
= struct
  open JsonOutputUtils;
  structure RA = Annotations;
  structure GA = RA.GraphAnnotations;
  
  type data = RA.T;
  val empty_annotation = RA.init;
  
  structure RuleAnnOutput : RULE_ANN_OUTPUT_JSON
  = struct
    open JsonOutputUtils;
    structure GraphAnnOutput = OutputGraphAnnotationsJSON(
      structure Annotations = GA
      val annotation_to_json = annotation_to_json
    );
    type data = GA.data * (GA.T * GA.T);
    val empty_annotation = (GA.empty_data,(GA.init,GA.init));
    fun get_lhs_annotation (_,(ann,_)) = ann;
    fun get_rhs_annotation (_,(_,ann)) = ann;
    val output_rule_annotation = annotation_to_json o fst;
  end;
  
  fun get_rule_annotation ann r =
    (RA.get_rule_annotation ann r,
     (RA.get_rule_lhs_annotation ann r,RA.get_rule_rhs_annotation ann r));
  val output_ruleset_annotation =
    annotation_to_json o RA.get_ruleset_annotation;
end;

structure OutputRulesetStringTableAnnotationsJSON
  : RULESET_ANN_OUTPUT_JSON where type data = RulesetStringTableAnnotations.T
= struct
  open JsonOutputUtils;
  
  structure OutputAnn = OutputRulesetAnnotationsJSON(
    structure Annotations = RulesetStringTableAnnotations
    val annotation_to_json = output_string_table
  );
  open OutputAnn;
end;

structure OutputRulesetJsonObjectAnnotationsJSON
  : RULESET_ANN_OUTPUT_JSON where type data = RulesetJsonObjectAnnotations.T
= OutputRulesetAnnotationsJSON(
  structure Annotations = RulesetJsonObjectAnnotations
  fun annotation_to_json obj = Json.Object obj
)



