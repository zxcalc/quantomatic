structure InputRuleNameXML : INPUT_XML = InputSStrNameXML(R);
structure InputTagNameXML : INPUT_XML = InputSStrNameXML(TagName);

(* input a set of rules *)
functor InputRulesetXML(
  structure Ruleset : RULESET 
  structure InputRule : INPUT_XML
  sharing type InputRule.data = Ruleset.Rule.T
) : INPUT_XML
= struct   
  open XML_Parse_Utils;
  
  type T = XML.tree;
  type data = Ruleset.T;

  fun data_to_string _ = "[Ruleset]";

  structure InputAllRules = 
  InputNTabXML(
    val tagname = "allrules"
    val entry_tagname = "rule"
    val key_tagname = "name"
    val value_tagname = "definition"
    structure NTab = R.NTab
    structure InputName = InputUseChildXML(InputRuleNameXML)
    structure InputValue = InputRule
  );

  structure InputBRel = 
  InputNameBRelXML(
    val tagname = "tags"
    val rel_tagname = "tag"
    val cod_tagname = "name"
    val dom_tagname = "rule"
    structure BRel = TagRel
    structure InputDom = InputUseChildXML(InputRuleNameXML)
    structure InputCod = InputUseChildXML(InputTagNameXML)
  );

  structure InputRuleNameList = InputListXML(
    val tagname = "active" 
    val item_tagname = "rule" 
    structure Input1 = InputUseChildXML(InputRuleNameXML));
  
  fun input xml = 
      (let val () = assert_tag "ruleset" xml
           val children = get_children xml
           val rules = (InputAllRules.input (get_unique_elem "allrules" children)) 
             handle bad_input_exp (s,p) => raise bad_input_exp (s,"allrules/"^p);
           val tags = (InputBRel.input (get_unique_elem "tags" children))
             handle bad_input_exp (s,p) => raise bad_input_exp (s,"tags/"^p);
           val active_rules = (InputRuleNameList.input (get_unique_elem "active" children))
             handle bad_input_exp (s,p) => raise bad_input_exp (s,"active/"^p);
       in
         Ruleset.empty 
           |> Ruleset.set_allrules rules
           |> Ruleset.set_brel tags
           |> Ruleset.set_active (R.NSet.of_list active_rules)
       end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,"ruleset/"^p);
end;


(* vim:et:sts=2:sw=2:ts=2
*)
