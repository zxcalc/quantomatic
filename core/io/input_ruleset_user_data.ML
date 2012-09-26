functor InputRulesetUserDataXML(
	structure GraphUserData : GRAPH_USER_DATA
) : INPUT_XML
=struct

  open XML_Parse_Utils;

  structure UD = GraphUserData;

  type T = XML.tree;
  type ud_unit = string Symtab.table
  type data = (ud_unit * (UD.T * UD.T)) R.NTab.T;

  fun data_to_string _ = "[RulesetUserData]";

  structure udinput = InputGraphUserDataXML(structure UserData = GraphUserData);

  fun input_entry xml =
    let
     val _ = assert_tag "entry" xml
     val child = get_unique_child xml
     val k = (get_attribute xml "name") 
        handle bad_input_exp (s,p) => raise
                      bad_input_exp (s,"entry/"^p)
     val data = get_text child
    in
      (k, data) 
    end;

  fun input_data_on_rule xml =
    let
      val children = get_children xml
      val mk_tab = Symtab.make
    in
      mk_tab (map input_entry (filter_elems "entry" children))
    end;

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
      val rule_ud = (input_data_on_rule o (get_unique_elem "rule_userdata") o
                        get_children o (get_unique_elem "rule_data"))  children
      val lhs_ud = (udinput.input o (get_unique_elem "user_data") o get_children o (get_unique_elem "lhs")) children
      val rhs_ud = (udinput.input o (get_unique_elem "user_data") o get_children o (get_unique_elem "rhs")) children
    in
      (rulename, (rule_ud ,(lhs_ud,rhs_ud)))
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
