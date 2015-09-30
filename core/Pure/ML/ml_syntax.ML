(*  Title:      Pure/ML/ml_syntax.ML
    Author:     Makarius

Basic ML syntax operations.
*)

signature ML_SYNTAX =
sig
  val reserved_names: string list
  val reserved: Name.context
  val is_reserved: string -> bool
  val is_identifier: string -> bool
  val atomic: string -> string
  val print_int: int -> string
  val print_pair: ('a -> string) -> ('b -> string) -> 'a * 'b -> string
  val print_list: ('a -> string) -> 'a list -> string
  val print_option: ('a -> string) -> 'a option -> string
  val print_char: string -> string
  val print_string: string -> string
  val print_strings: string list -> string
  val print_properties: Properties.T -> string
  val print_position: Position.T -> string
  val print_range: Position.range -> string
  val make_binding: string * Position.T -> string
  val print_indexname: indexname -> string
  val print_class: class -> string
  val print_sort: sort -> string
  val print_typ: typ -> string
  val print_term: term -> string
  val pretty_string: int -> string -> Pretty.T
end;

structure ML_Syntax: ML_SYNTAX =
struct

(* reserved words *)

val reserved_names = filter Symbol.is_ascii_identifier ML_Lex.keywords;
val reserved = Name.make_context reserved_names;
val is_reserved = Name.is_declared reserved;


(* identifiers *)

fun is_identifier name =
  not (is_reserved name) andalso Symbol.is_ascii_identifier name;


(* literal output -- unformatted *)

val atomic = enclose "(" ")";

val print_int = string_of_int;

fun print_pair f1 f2 (x, y) = "(" ^ f1 x ^ ", " ^ f2 y ^ ")";

fun print_list f = enclose "[" "]" o commas o map f;

fun print_option f NONE = "NONE"
  | print_option f (SOME x) = "SOME (" ^ f x ^ ")";

fun print_char s =
  if not (Symbol.is_char s) then s
  else if s = "\"" then "\\\""
  else if s = "\\" then "\\\\"
  else if s = "\t" then "\\t"
  else if s = "\n" then "\\n"
  else if s = "\f" then "\\f"
  else if s = "\r" then "\\r"
  else
    let val c = ord s in
      if c < 32 then "\\^" ^ chr (c + ord "@")
      else if c < 127 then s
      else "\\" ^ string_of_int c
    end;

val print_string = quote o implode o map print_char o Symbol.explode;
val print_strings = print_list print_string;

val print_properties = print_list (print_pair print_string print_string);

fun print_position pos =
  "Position.of_properties " ^ print_properties (Position.properties_of pos);

fun print_range range =
  "Position.range_of_properties " ^ print_properties (Position.properties_of_range range);

fun make_binding (name, pos) =
  "Binding.make " ^ print_pair print_string print_position (name, pos);

val print_indexname = print_pair print_string print_int;

val print_class = print_string;
val print_sort = print_list print_class;

fun print_typ (Type arg) = "Term.Type " ^ print_pair print_string (print_list print_typ) arg
  | print_typ (TFree arg) = "Term.TFree " ^ print_pair print_string print_sort arg
  | print_typ (TVar arg) = "Term.TVar " ^ print_pair print_indexname print_sort arg;

fun print_term (Const arg) = "Term.Const " ^ print_pair print_string print_typ arg
  | print_term (Free arg) = "Term.Free " ^ print_pair print_string print_typ arg
  | print_term (Var arg) = "Term.Var " ^ print_pair print_indexname print_typ arg
  | print_term (Bound i) = "Term.Bound " ^ print_int i
  | print_term (Abs (s, T, t)) =
      "Term.Abs (" ^ print_string s ^ ", " ^ print_typ T ^ ", " ^ print_term t ^ ")"
  | print_term (t1 $ t2) = "Term.$ " ^ print_pair print_term print_term (t1, t2);


(* toplevel pretty printing *)

fun pretty_string max_len str =
  let
    val body =
      maps (fn XML.Elem _ => ["<markup>"] | XML.Text s => Symbol.explode s) (YXML.parse_body str)
        handle Fail _ => Symbol.explode str;
    val body' =
      if length body <= max_len then body
      else take (Int.max (max_len, 0)) body @ ["..."];
  in Pretty.str (quote (implode (map print_char body'))) end;

end;
