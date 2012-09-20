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

structure OutputLinratJSON : OUTPUT_JSON where type data = LinratAngleExpr.T =
struct
  open JsonUtils;
  type data = LinratAngleExpr.T;
  
  fun make_rational (num,denom) = let
    val add_num = Json.update ("num",(Json.Int num))
    val add_denom = Json.update ("denom",(Json.Int denom))
    val add_all = add_denom o add_num
  in
    Json.Object (add_all Json.empty_obj)
  end
  
  fun make_vars tab = let
    fun add_entry (v,c) = Json.update (X.string_of_name v,make_rational c)
  in
    Json.Object (X.NTab.fold add_entry tab Json.empty_obj)
  end
  
  fun output (pi,tab) = let
    (* we try to minimise the output *)
    val add_const = if LinratAngleExpr.Coeff.is_zero pi
                    then I
                    else Json.update ("pi",(make_rational pi))
    val add_vars = if X.NTab.is_name_empty tab
                   then I
                   else Json.update ("vars",(make_vars tab))
    (*
    val str = Pretty.string_of (LinratAngleExpr.pretty (pi,tab))
    val add_str = Json.update "expr" (Json.String str)
    *)
    val add_all = (* add_str o *) add_vars o add_const
  in
    Json.Object (add_all Json.empty_obj)
  end
end; (* structure *)

