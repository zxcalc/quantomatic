signature INPUT_JSON = INPUT where type T = Json.json;
signature OUTPUT_JSON = OUTPUT where type T = Json.json;

structure JsonUtils =
struct
  type T = Json.json;
  exception bad_input_exp of string*string;

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

functor InputNTabJSON (
  structure Name : SNAME where type src = string
  structure InputValue : INPUT_JSON
) : INPUT_JSON
= struct
  structure N = Name;
  open JsonUtils;
  type data = InputValue.data N.NTab.T;

  val get_value = InputValue.input oo Json.get

  fun entry_list obj =
        map (fn n => (N.mk n,get_value obj n)) (Json.properties obj)

  fun input (Json.Object obj) =
          fold N.NTab.ins (entry_list obj) N.NTab.empty
    | input Json.Null = N.NTab.empty
    | input _ = raise bad_input_exp ("Expected object","");
end;

functor OutputNTabJSON (
  structure Name : SNAME where type src = string
  structure OutputValue : OUTPUT_JSON
) : OUTPUT_JSON
= struct
  structure N = Name;
  open JsonUtils;
  type data = OutputValue.data N.NTab.T;

  fun update_value (n,v) = Json.update (N.dest n,OutputValue.output v)

  fun output tab =
    Json.Object (N.NTab.fold update_value tab Json.empty_obj)
end;

