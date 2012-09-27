
structure InputRulesetStringTableAnnotationsXML : INPUT_XML
= struct

  open XML_Parse_Utils;

  structure RA = RulesetStringTableAnnotations;
  structure GA = GraphStringTableAnnotations;
  structure GAInput = InputGraphStringTableAnnotationsXML;

  type T = XML.tree;
  type data = RA.T;

  fun data_to_string _ = "[RulesetUserData]";

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

  fun input_rule xml rsann =
    let
      val () = assert_tag "rule" xml
      val children = get_children xml
      val rulename = input_rulename (get_unique_elem "rulename" children)
      val rule_ud = (input_data_on_rule o (get_unique_elem "rule_userdata") o
                        get_children o (get_unique_elem "rule_data"))  children
      val lhs_ud = (GAInput.input o (get_unique_elem "user_data") o get_children o (get_unique_elem "lhs")) children
      val rhs_ud = (GAInput.input o (get_unique_elem "user_data") o get_children o (get_unique_elem "rhs")) children
    in
      rsann |> RA.set_rule_annotation rulename rule_ud
            |> RA.set_rule_lhs_annotation rulename lhs_ud
            |> RA.set_rule_rhs_annotation rulename rhs_ud
    end
      handle bad_input_exp (s,p) => raise bad_input_exp (s, "rule/"^p);

  fun input xml =
    if not (test_tag "ruleset_and_user_data" xml)
    then RA.init
    else
      let 
        fun get_exactly_one_child tag =
          (get_unique_elem tag) o (filter_elems tag) o get_children
        val rud = get_exactly_one_child "ruleset_user_data" xml
        val children = get_children rud
      in
        (* we currently don't deal with ruleset annotation *)
        fold input_rule (filter_elems "rule" children) RA.init
      end
      handle bad_input_exp (s,p) => raise bad_input_exp (s, "ruleset_user_data/"^p);

end;
