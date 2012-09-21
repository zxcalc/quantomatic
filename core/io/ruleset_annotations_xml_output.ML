
structure OutputRulesetStringTableAnnotationsXML : OUTPUT_XML
= struct
  structure GAOut = OutputGraphStringTableAnnotationsXML;
  structure RA = RulesetStringTableAnnotations;

  type T = XML.tree;
  type data = RA.T;

  open XML;
  open XML_Output_Utils;

  (* WHY do we wrap up the user data in *another* tag? *)
  fun crazy_graph_wrap f = (wrap_el_simple "user_data") o f;
  fun crazy_rule_wrap f = (wrap_el_simple "rule_data") o f;

  fun output_rule (rn,(rann,(lhs,rhs))) anns =
    (wrap_els_simple "rule"
      [
        wrap_text_simple "rulename" (R.dest rn),
        crazy_rule_wrap (output_symtab "rule_userdata") rann,
        wrap_el_simple "lhs" (crazy_graph_wrap GAOut.output lhs),
        wrap_el_simple "rhs" (crazy_graph_wrap GAOut.output rhs)
      ]
    )::anns

  fun output rsa = Elem (("ruleset_user_data",[]),
    R.NTab.fold output_rule (RA.get_rule_annotation_tab rsa) []);
end;

