(**
 * JSON graph parsing and writing
 *
 * LINRAT = Null | String | LINRAT_OBJ
 * LINRAT_OBJ ::=
 * {
 *   "pi": RATIONAL,
 *   "vars": RATIONAL_MAP
 * }
 * RATIONAL_MAP ::=
 * {
 *   "num": Int,
 *   [, "denom": Int
 * }
 * RATIONAL_MAP   ::= { VARNAME : RATIONAL (, VARNAME : RATIONAL)* } | {} | Null
 * VARNAME ::= String
 * RATIONAL ::= Null |
 * {
 *   "num": Int,
 *   "denom": Int
 * }
 *
 * Json.Null is treated as zero.  Json.String is parsed in a "natural" way.
 *)

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

  fun parse_str s = LinratAngleExpr.parse s
        handle LinratAngleExpr.parse_exp =>
          raise bad_input_exp ("Could not parse \""^s^"\"","")
  
  fun input (Json.Object obj) =
        let
          val pi = get_rational_easy C.zero obj "pi"
          val vars = get_object_easy obj "vars"
        in
          LinratAngleExpr.mk pi (parse_vars vars)
        end
    | input (Json.String s) = parse_str s
    | input Json.Null = LinratAngleExpr.zero
    | input _ = raise bad_input_exp ("Expected object or string","")

end;

structure OutputLinratJSON : OUTPUT_JSON where type data = LinratAngleExpr.T =
struct
  open JsonOutputUtils;
  type data = LinratAngleExpr.T;
  
  fun make_rational r =
    Json.mk_object [("num",  (Json.Int (Rational.get_num r))),
                    ("denom",(Json.Int (Rational.get_denom r)))]
  
  fun make_vars expr = let
    fun add_entry x = Json.update (
      X.string_of_name x,
      make_rational (LinratAngleExpr.coeff_for_var expr (SOME x)))
  in
    Json.Object (X.NSet.fold add_entry (LinratAngleExpr.free_vars expr) Json.empty_obj)
  end
  
  fun output expr = let
    (* we try to minimise the output *)
    val pi = LinratAngleExpr.coeff_for_var expr NONE
    val add_const = if LinratAngleExpr.Coeff.is_zero pi
                    then I
                    else Json.update ("pi",(make_rational pi))
    val add_vars = if X.NSet.is_empty (LinratAngleExpr.free_vars expr)
                   then I
                   else Json.update ("vars",(make_vars expr))
    val str = Pretty.string_of (LinratAngleExpr.pretty expr)
    val add_str = Json.update ("pretty",(Json.String str))
    val add_all = add_str o add_vars o add_const
  in
    Json.Object (add_all Json.empty_obj)
  end
end; (* structure *)

