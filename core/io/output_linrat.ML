(* *)

(*-------------------------------------------------------*)
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

(* vim:et:sts=2:sw=2
*)
