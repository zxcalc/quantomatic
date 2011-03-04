(* *)

(*-------------------------------------------------------*)
structure OutputLinratXML : OUTPUT_PRETTY_XML where type data = LinratExpr.T = 
struct 
  type T = Pretty.T
  type data = LinratExpr.T

  structure VName = LinratExpr.VName;

(* -- this section depends strongly on the linrat_expr type ----*)
  fun render_xml_rational (n,d) = 
      Pretty.chunks [Pretty.str ("<num>"^ (string_of_int  n)^"</num>"),
         Pretty.str ("<denom>"^ (string_of_int  d)^"</denom>")];
  
  fun render_xml_constant_part k =
      if AngleExpr.Coeff.is_zero k then
        (Pretty.str "")
      else
        Pretty.chunks [Pretty.str "<constant>",
           Pretty.block [ Pretty.str "  ", (render_xml_rational k)],
           Pretty.str "</constant>"];
  
  fun render_xml_coeff_var (var, coeff) = 
      Pretty.chunks [Pretty.str "<variable>",
         Pretty.block [Pretty.str "  ",
           Pretty.chunks [
           Pretty.block [Pretty.str "<name>",
                   VName.pretty_name var,
                   Pretty.str "</name>"],
           Pretty.str "<coefficient>",
           Pretty.block [
           Pretty.str "  ",
           (render_xml_rational coeff)
           ],
           Pretty.str "</coefficient>"
           ]
          ],
         Pretty.str "</variable>"];
  
  fun render_xml_variable_part tab = 
      let fun f kv [] = [render_xml_coeff_var kv]
      | f kv rest = (render_xml_coeff_var kv)::rest
      in
        Pretty.chunks (VName.NTab.fold f tab [])
      end;
  
  fun output (pi,tab) = 
      Pretty.block
      (if AngleExpr.is_zero (pi,tab) then 
         []
       else
         [Pretty.str "<expr>",
          Pretty.block [Pretty.str "  ",
            Pretty.chunks [
            Pretty.block [Pretty.str "<as_string>",
              AngleExpr.pretty (pi,tab),
              Pretty.str "</as_string>"],
            render_xml_constant_part pi,
            render_xml_variable_part tab]
           ],
          Pretty.str "</expr>"]);
end; (* structure *)



