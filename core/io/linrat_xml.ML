structure InputLinratXML : INPUT_XML where type data = LinratAngleExpr.T
= struct

  open XML_Parse_Utils;
  
  type T = XML.tree;
  type data = LinratAngleExpr.T;
  
  fun input_rational xml =
      let val children = get_children xml;
          val num = get_unique_child (get_unique_elem "num" children)
          val denom = get_unique_child (get_unique_elem "denom" children)
      in (InputIntXML.input num, InputIntXML.input denom) end;
  
  fun input_constant xml =
      (let val () = assert_tag "constant" xml
       in input_rational xml end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,"constant/"^p);
  
  fun input_coeff xml =
      (let val () = assert_tag "coefficient" xml
       in input_rational xml end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,"coefficient/"^p);
  
  fun input_variable xml =
      (let val _ = assert_tag "variable" xml
           val children = get_children xml
           val name = get_unique_child_text (get_unique_elem "name" children)
           val coeff = get_unique_elem "coefficient" children
       in (name, input_coeff coeff) end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,"variable/"^p);
  
  fun input xml  =
      (let val () = assert_tag "angleexpr" xml
           val children = get_children xml
           val constant = case (lookup_unique_elem "constant" children) of
                            (NONE) => LinratAngleExpr.Coeff.zero
                          | (SOME const) => input_constant const
           val vars = List.map (apfst X.mk o input_variable) (filter_elems "variable" children)
       in LinratAngleExpr.mk constant vars end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,"angleexpr/"^p);

end;

structure OutputLinratXML : OUTPUT_XML where type data = LinratAngleExpr.T = 
struct 
  type T = XML.tree;
  type data = LinratAngleExpr.T;

  open XML;
  open XML_Output_Utils;
(* -- this section depends strongly on the linrat_expr type ----*)
  fun render_xml_rational (n,d) = 
      [wrap_int_simple "num" n,
       wrap_int_simple "denom" d];
  
  fun render_xml_constant_part k =
      if LinratAngleExpr.Coeff.is_zero k then
        []
      else
        [wrap_els_simple "constant" (render_xml_rational k)];
  
  fun render_xml_coeff_var (var, coeff) = 
      wrap_els_simple "variable"
          [wrap_text_simple "name" (X.string_of_name var),
           wrap_els_simple "coefficient" (render_xml_rational coeff)];
  
  fun render_xml_variable_part tab = 
      let fun f kv [] = [render_xml_coeff_var kv]
      | f kv rest = (render_xml_coeff_var kv)::rest
      in
        X.NTab.fold f tab []
      end;
  
  fun output (pi,tab) = 
      (* if LinratAngleExpr.is_zero (pi,tab) then 
        []
      else *)
        wrap_els_simple "angleexpr" 
          (wrap_text_simple "string_of" (Pretty.string_of (LinratAngleExpr.pretty (pi,tab)))
          ::((render_xml_constant_part pi)@(render_xml_variable_part tab)));
end; (* structure *)

