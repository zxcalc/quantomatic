(*  Title:      Pure/General/value.ML
    Author:     Makarius

Plain values, represented as string.
*)

signature VALUE =
sig
  val parse_bool: string -> bool
  val print_bool: bool -> string
  val parse_nat: string -> int
  val parse_int: string -> int
  val print_int: int -> string
  val parse_real: string -> real
  val print_real: real -> string
end;

structure Value: VALUE =
struct

fun parse_bool "true" = true
  | parse_bool "false" = false
  | parse_bool s = raise Fail ("Bad boolean " ^ quote s);

val print_bool = Bool.toString;


fun parse_nat s =
  let val i = Int.fromString s in
    if is_none i orelse the i < 0
    then raise Fail ("Bad natural number " ^ quote s)
    else the i
  end;

fun parse_int s =
  let val i = Int.fromString s in
    if is_none i orelse String.isPrefix "~" s
    then raise Fail ("Bad integer " ^ quote s)
    else the i
  end;

val print_int = signed_string_of_int;


fun parse_real s =
  (case Real.fromString s of
    SOME x => x
  | NONE => raise Fail ("Bad real " ^ quote s));

fun print_real x =
  let val s = signed_string_of_real x in
    (case space_explode "." s of
      [a, b] => if forall_string (fn c => c = "0") b then a else s
    | _ => s)
  end;

end;
