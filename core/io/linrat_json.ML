structure InputLinratJSON : INPUT_JSON where type data = LinratAngleExpr.T
= struct
  open JsonInputUtils;
  structure C = LinratAngleExpr.Coeff;
  type data = LinratAngleExpr.T;
  
  fun parse_rational obj = let
    val num = get_int obj "num"
    val denom = get_int obj "denom"
  in
    C.mk (num, denom)
  end
  
  fun get_rational_easy dflt obj prop =
    case Json.lookup obj prop
      of SOME (Json.Object obj') => parse_rational obj'
       | SOME (Json.Null) => dflt
       | NONE => dflt
       | _ => raise bad_input_exp ("Property \""^prop^"\" was not an object","")
  
  fun parse_vars obj =
    map (fn n => (X.mk n,get_rational_easy C.zero obj n)) (Json.properties obj)
  
  fun input (Json.Object obj) =
        let
          val pi = get_rational_easy C.zero obj "pi"
          val vars = get_object_easy obj "vars"
        in
          LinratAngleExpr.mk pi (parse_vars vars)
        end
    | input (Json.String s) = LinratAngleExpr.parse s
    | input Json.Null = LinratAngleExpr.zero
    | input _ = raise bad_input_exp ("Expected object or string","");

end;

structure OutputLinratJSON : OUTPUT_JSON where type data = LinratAngleExpr.T =
struct
  open JsonOutputUtils;
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
    val str = Pretty.string_of (LinratAngleExpr.pretty (pi,tab))
    val add_str = Json.update ("pretty",(Json.String str))
    val add_all = add_str o add_vars o add_const
  in
    Json.Object (add_all Json.empty_obj)
  end
end; (* structure *)

