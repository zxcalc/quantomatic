functor InputRulesetUIDataXML(
	structure UIData : USER_DATA
) : INPUT_XML
=struct

  open XML_Parse_Utils;

  structure UD = UIData;

  type T = XML.tree;
  type data = (UD.T * UD.T) R.NTab.T;

  fun data_to_string _ = "[RulesetUserData]";

  structure udinput = InputUIDataXML(structure UIData = UIData);

  fun input_rulename xml = 
    let
      val () = assert_tag "rulename" xml
    in
      R.mk (get_unique_child_text xml)
    end;

  fun input_rule_ud xml = 
    let
      val () = assert_tag "rule" xml
      val children = get_children xml
      val rulename = input_rulename (get_unique_elem "rulename" children) 
      val lhs_ud = (udinput.input o (get_unique_elem "user_data") o get_children o (get_unique_elem "lhs")) children
      val rhs_ud = (udinput.input o (get_unique_elem "user_data") o get_children o (get_unique_elem "rhs")) children
    in
      (rulename, (lhs_ud,rhs_ud))
    end
      handle bad_input_exp (s,p) => raise bad_input_exp (s, "rule/"^p);

  fun input_rset_ud xml = if not (test_tag "ruleset_and_user_data" xml)
   then R.NTab.empty
   else
     let 
        val xml' = List.last (get_children xml)
        val () = assert_tag "ruleset_user_data" xml'
        val children = get_children xml'
        val rules_ud = List.map input_rule_ud (filter_elems "rule" children)
     in
      R.NTab.of_list rules_ud
     end
      handle bad_input_exp (s,p) => raise bad_input_exp (s, "ruleset_user_data/"^p);

  val input = input_rset_ud;

end;
