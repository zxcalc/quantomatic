(*  Title:      Pure/General/properties.ML
    Author:     Makarius

Property lists.
*)

signature PROPERTIES =
sig
  type entry = string * string
  type T = entry list
  val defined: T -> string -> bool
  val get: T -> string -> string option
  val put: entry -> T -> T
  val remove: string -> T -> T
  val seconds: T -> string -> Time.time
end;

structure Properties: PROPERTIES =
struct

type entry = string * string;
type T = entry list;

fun defined (props: T) name = AList.defined (op =) props name;
fun get (props: T) name = AList.lookup (op =) props name;
fun put entry (props: T) = AList.update (op =) entry props;
fun remove name (props: T) = AList.delete (op =) name props;

fun seconds props name =
  (case AList.lookup (op =) props name of
    NONE => Time.zeroTime
  | SOME s => Time.fromReal (the_default 0.0 (Real.fromString s)));

end;
