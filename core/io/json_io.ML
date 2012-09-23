signature INPUT_JSON = INPUT where type T = Json.json;
signature OUTPUT_JSON = OUTPUT where type T = Json.json;

structure JsonInputUtils =
struct
  type T = Json.json;
  exception bad_input_exp of string*string;

  fun prepend_prop prop loc =
    if String.size loc > 0
    then prop^"."^loc
    else prop

  fun fold_obj_easy f obj prop =
    (case Json.lookup obj prop
       of SOME (Json.Object obj') =>
            Json.fold (fn (k,v) => f (k,v)
                       handle bad_input_exp (m,l) =>
                         raise bad_input_exp (m,
                                prepend_prop (prop^"."^k) l))
                      obj'
        | SOME Json.Null => I
        | NONE => I
        | _ => raise bad_input_exp ("Expected an object",prop))

  fun fold_arr_easy f obj prop =
    (case Json.lookup obj prop
       of SOME (Json.Array arr) => fold f arr
        | SOME Json.Null => I
        | NONE => I
        | _ => raise bad_input_exp ("Expected an array",prop))

  fun fold_obj_or_arr_easy (fo,fa) obj prop =
    (case Json.lookup obj prop
       of SOME (Json.Array arr) => fold fa arr
        | SOME (Json.Object obj') =>
            Json.fold (fn (k,v) => fo (k,v)
                       handle bad_input_exp (m,l) =>
                         raise bad_input_exp (m,
                                prepend_prop (prop^"."^k) l))
                      obj'
        | SOME Json.Null => I
        | NONE => I
        | _ => raise bad_input_exp ("Expected an object or array",prop))

  (* wraps Json.get and converts Json.notfound_exn to bad_input_exp *)
  fun get obj prop =
    Json.get obj prop
    handle Json.notfound_exn prop =>
      raise bad_input_exp ("Missing property \""^prop^"\"","");

  (* wraps Json.get and demands an array *)
  fun get_array obj prop =
    case get obj prop
      of (Json.Array arr) => arr
       | _ => raise bad_input_exp ("Property \""^prop^"\" not an array","")

  (* wraps Json.get and requests an array, returns empty array if not found *)
  fun get_array_easy obj prop =
    case Json.lookup obj prop
      of SOME (Json.Array arr) => arr
       | SOME (Json.Null) => []
       | NONE => []
       | _ => raise bad_input_exp ("Property \""^prop^"\" not an array","")

  (* wraps Json.get and demands an object *)
  fun get_obj obj prop =
    case get obj prop
      of (Json.Object ob) => ob
       | _ => raise bad_input_exp ("Property \""^prop^"\" not an object","")

  (* wraps Json.get and requests an object, returns empty object if not found *)
  fun get_object_easy obj prop =
    case Json.lookup obj prop
      of SOME (Json.Object ob) => ob
       | SOME (Json.Null) => Json.empty_obj
       | NONE => Json.empty_obj
       | _ => raise bad_input_exp ("Property \""^prop^"\" not an object","")

  (* wraps Json.get and demands a real *)
  fun get_real obj prop =
    case get obj prop
      of (Json.Real n) => n
       | (Json.Int n) => Real.fromInt n
       | _ => raise bad_input_exp ("Property \""^prop^"\" not a number","")

  (* wraps Json.get and requests a real, returns dflt if not found *)
  fun get_real_easy dflt obj prop =
    case Json.lookup obj prop
      of SOME (Json.Real n) => n
       | SOME (Json.Int n) => Real.fromInt n
       | SOME (Json.Null) => dflt
       | NONE => dflt
       | _ => raise bad_input_exp ("Property \""^prop^"\" not a number","")

  fun coerce_int prop r =
    if (Real.isFinite r) andalso Real.==(Real.realMod r,0.0)
    then Real.trunc r
    else raise bad_input_exp ("Property \""^prop^"\": "^
                              (Real.toString r)^" is not an integer","")

  (* wraps Json.get and demands an integer *)
  fun get_int obj prop =
    case get obj prop
      of (Json.Int n) => n
       | (Json.Real n) => coerce_int prop n
       | _ => raise bad_input_exp ("Property \""^prop^"\" not a number","")

  (* wraps Json.get and requests an integer, returns dflt if not found *)
  fun get_int_easy dflt obj prop =
    case Json.lookup obj prop
      of SOME (Json.Int n) => n
       | SOME (Json.Real n) => coerce_int prop n
       | SOME (Json.Null) => dflt
       | NONE => dflt
       | _ => raise bad_input_exp ("Property \""^prop^"\" not a number","")

  (* wraps Json.get and demands a string *)
  fun get_string obj prop =
    case get obj prop
       of (Json.String s) => s
        | _ => raise bad_input_exp ("Property \""^prop^"\" not a string","")

  (* wraps Json.get and requests a string, returns dflt if not found *)
  fun get_string_easy dflt obj prop =
    case Json.lookup obj prop
      of SOME (Json.String s) => s
       | SOME (Json.Null) => dflt
       | NONE => dflt
       | _ => raise bad_input_exp ("Property \""^prop^"\" not a string","")

  (* wraps Json.get and demands a boolean *)
  fun get_bool obj prop =
    case get obj prop
       of (Json.Bool b) => b
        | _ => raise bad_input_exp ("Property \""^prop^"\" not a boolean","")

  (* wraps Json.get and requests a boolean, returns dflt if not found *)
  fun get_bool_easy dflt obj prop =
    case Json.lookup obj prop
      of SOME (Json.Bool b) => b
       | SOME (Json.Null) => dflt
       | NONE => dflt
       | _ => raise bad_input_exp ("Property \""^prop^"\" not a boolean","")
end;

structure JsonOutputUtils =
struct
  type T = Json.json;

  (* Does not write null values *)
  fun update (_,Json.Null) = I
    | update d = Json.update d;

  (* Does not write empty values *)
  fun opt_update (_,Json.Null) = I
    | opt_update (_,Json.Array []) = I
    | opt_update (v as (_,Json.Object obj)) = (case Json.properties obj
                                                 of [] => I
                                                  | _ => Json.update v)
    | opt_update d = Json.update d;
end

functor InputNTabJSON (
  structure Name : SNAME where type src = string
  structure InputValue : INPUT_JSON
) : INPUT_JSON
= struct
  structure N = Name;
  open JsonInputUtils;
  type data = InputValue.data N.NTab.T;

  fun map_entry (n,v) = (N.mk n,InputValue.input v)
      handle InputValue.bad_input_exp (m,l) =>
        raise bad_input_exp (m,prepend_prop n l)

  fun input (Json.Object obj) =
          Json.fold (N.NTab.ins o map_entry) obj N.NTab.empty
    | input Json.Null = N.NTab.empty
    | input _ = raise bad_input_exp ("Expected object","");
end;

functor OutputNTabJSON (
  structure Name : SNAME where type src = string
  structure OutputValue : OUTPUT_JSON
) : OUTPUT_JSON
= struct
  structure N = Name;
  open JsonOutputUtils;
  type data = OutputValue.data N.NTab.T;

  fun update_value (n,v) = Json.update (N.dest n,OutputValue.output v)

  fun output tab =
    Json.Object (N.NTab.fold update_value tab Json.empty_obj)
end;

structure InputUnitJSON : INPUT_JSON
= struct
  open JsonInputUtils;
  type data = unit;
  fun input Json.Null = ()
    | input _ = raise bad_input_exp ("Unexpected data","");
end;

structure OutputUnitJSON : OUTPUT_JSON
= struct
  open JsonOutputUtils;
  type data = unit;
  fun output () = Json.Null;
end;

