(* input a set of rules *)
functor InputRulesetXML(
  structure Ruleset : RULESET
  structure InputRule : INPUT_XML
  sharing type InputRule.data = Ruleset.Rule.T
) : INPUT_XML
= struct
  open XML_Parse_Utils
  
  type T = XML.tree
  type data = Ruleset.T

  val input_rule_name = R.mk o get_unique_child_text
  val input_tag_name = TagName.mk o get_unique_child_text

  fun input_rule_defs xml =
    let val els = filter_elems "rule" (get_children xml)
    in
      fold
        (fn el =>
         let
           val children = get_children el
           val name_elem = get_unique_elem "name" children
           val name = input_rule_name name_elem
           val value = (apply_to_unique_elem InputRule.input "definition" children)
             handle InputRule.bad_input_exp e => raise bad_input_exp e
         in RTab.add (name, value) end)
        els
        RTab.empty
    end

  fun input_tags xml =
    let val rel_els = filter_elems "tag" (get_children xml)
    in
      fold
        (fn rel_el =>
         let
           val children = get_children rel_el
           val rule_els = filter_elems "rule" children
           val rule_names = map input_rule_name rule_els
           val tag_els = filter_elems "name" children
           val tag_names = map input_tag_name tag_els
         in
           fold (fn tag_n =>
                  fold (fn rule_n => RTagRel.add (rule_n,tag_n))
                       rule_names)
                tag_names
         end)
        rel_els
        RTagRel.empty
    end

  fun input_active_list xml =
    map input_rule_name (filter_elems "rule" (get_children xml))

  fun input xml =
      (let
           val is_wrapped= test_tag "ruleset_and_user_data" xml
           val xml' = if is_wrapped then hd (get_children xml)
                                    else xml
           val () = assert_tag "ruleset" xml'
           val children = get_children xml'
           val rules = (input_rule_defs (get_unique_elem "allrules" children))
             handle bad_input_exp (s,p) => raise bad_input_exp (s,"allrules/"^p)
           val tags = (input_tags (get_unique_elem "tags" children))
             handle bad_input_exp (s,p) => raise bad_input_exp (s,"tags/"^p)
           val active_rules = (input_active_list (get_unique_elem "active" children))
             handle bad_input_exp (s,p) => raise bad_input_exp (s,"active/"^p)
       in
         Ruleset.empty
           |> Ruleset.set_allrules rules
           |> Ruleset.set_brel tags
           |> Ruleset.set_active (R.NSet.of_list active_rules)
       end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,"ruleset/"^p)
end


(* vim:et:sts=2:sw=2:ts=2
*)
