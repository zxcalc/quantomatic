(* *)

(*-------------------------------------------------------*)
structure OutputLinratXML : OUTPUT_XML_LIST where type data = LinratExpr.T = 
struct 
  type T = XML.tree list;
  type data = LinratExpr.T;

  structure VName = LinratExpr.VName;

  open XML;
  open XML_Output_Utils;
(* -- this section depends strongly on the linrat_expr type ----*)
  fun render_xml_rational (n,d) = 
      [wrap_int_simple "num" n,
       wrap_int_simple "denom" d];
  
  fun render_xml_constant_part k =
      if AngleExpr.Coeff.is_zero k then
        []
      else
        [wrap_els_simple "constant" (render_xml_rational k)];
  
  fun render_xml_coeff_var (var, coeff) = 
      wrap_els_simple "variable"
          [wrap_text_simple "name" (var),
           wrap_els_simple "coefficient" (render_xml_rational coeff)];
  
  fun render_xml_variable_part tab = 
      let fun f kv [] = [render_xml_coeff_var kv]
      | f kv rest = (render_xml_coeff_var kv)::rest
      in
        VName.NTab.fold f tab []
      end;
  
  fun output (pi,tab) = 
      if AngleExpr.is_zero (pi,tab) then 
        []
      else
        (wrap_text_simple "as_string" (Pretty.string_of (AngleExpr.pretty (pi,tab))))
        ::((render_xml_constant_part pi)@(render_xml_variable_part tab));
end; (* structure *)

(* vim:et:sts=2:sw=2
*)
