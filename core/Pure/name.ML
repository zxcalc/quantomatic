(*  Title:      Pure/name.ML
    Author:     Makarius

Names of basic logical entities (variables etc.).
*)

signature NAME =
sig
  val uu: string
  val uu_: string
  val aT: string
  val bound: int -> string
  val is_bound: string -> bool
  val internal: string -> string
  val dest_internal: string -> string
  val is_internal: string -> bool
  val reject_internal: string * Position.T list -> unit
  val skolem: string -> string
  val dest_skolem: string -> string
  val is_skolem: string -> bool
  val reject_skolem: string * Position.T list -> unit
  val clean_index: string * int -> string * int
  val clean: string -> string
  type context
  val context: context
  val make_context: string list -> context
  val declare: string -> context -> context
  val is_declared: context -> string -> bool
  val invent: context -> string -> int -> string list
  val invent_names: context -> string -> 'a list -> (string * 'a) list
  val invent_list: string list -> string -> int -> string list
  val variant: string -> context -> string * context
  val variant_list: string list -> string list -> string list
  val enforce_case: bool -> string -> string
  val desymbolize: bool option -> string -> string
end;

structure Name: NAME =
struct

(** common defaults **)

val uu = "uu";
val uu_ = "uu_";
val aT = "'a";



(** special variable names **)

(* encoded bounds *)

(*names for numbered variables --
  preserves order wrt. int_ord vs. string_ord, avoids allocating new strings*)

val small_int = Vector.tabulate (1000, fn i =>
  let val leading = if i < 10 then "00" else if i < 100 then "0" else ""
  in ":" ^ leading ^ string_of_int i end);

fun bound n =
  if n < 1000 then Vector.sub (small_int, n)
  else ":" ^ bound (n div 1000) ^ Vector.sub (small_int, n mod 1000);

val is_bound = String.isPrefix ":";


(* internal names -- NB: internal subsumes skolem *)

val internal = suffix "_";
val dest_internal = unsuffix "_";
val is_internal = String.isSuffix "_";
fun reject_internal (x, ps) =
  if is_internal x then error ("Bad name: " ^ quote x ^ Position.here_list ps) else ();

val skolem = suffix "__";
val dest_skolem = unsuffix "__";
val is_skolem = String.isSuffix "__";
fun reject_skolem (x, ps) =
  if is_skolem x then error ("Bad name: " ^ quote x ^ Position.here_list ps) else ();

fun clean_index (x, i) =
  (case try dest_internal x of
    NONE => (x, i)
  | SOME x' => clean_index (x', i + 1));

fun clean x = #1 (clean_index (x, 0));



(** generating fresh names **)

(* context *)

datatype context =
  Context of string option Symtab.table;    (*declared names with latest renaming*)

fun declare x (Context tab) =
  Context (Symtab.default (clean x, NONE) tab);

fun declare_renaming (x, x') (Context tab) =
  Context (Symtab.update (clean x, SOME (clean x')) tab);

fun is_declared (Context tab) = Symtab.defined tab;
fun declared (Context tab) = Symtab.lookup tab;

val context = Context Symtab.empty |> fold declare ["", "'"];
fun make_context used = fold declare used context;


(* invent names *)

fun invent ctxt =
  let
    fun invs _ 0 = []
      | invs x n =
          let val x' = Symbol.bump_string x
          in if is_declared ctxt x then invs x' n else x :: invs x' (n - 1) end;
  in invs o clean end;

fun invent_names ctxt x xs = invent ctxt x (length xs) ~~ xs;

val invent_list = invent o make_context;


(* variants *)

(*makes a variant of a name distinct from already used names in a
  context; preserves a suffix of underscores "_"*)
fun variant name ctxt =
  let
    fun vary x =
      (case declared ctxt x of
        NONE => x
      | SOME x' => vary (Symbol.bump_string (the_default x x')));

    val (x, n) = clean_index (name, 0);
    val (x', ctxt') =
      if not (is_declared ctxt x) then (x, declare x ctxt)
      else
        let
          val x0 = Symbol.bump_init x;
          val x' = vary x0;
          val ctxt' = ctxt
            |> x0 <> x' ? declare_renaming (x0, x')
            |> declare x';
        in (x', ctxt') end;
  in (x' ^ replicate_string n "_", ctxt') end;

fun variant_list used names = #1 (make_context used |> fold_map variant names);


(* names conforming to typical requirements of identifiers in the world outside *)

fun enforce_case' false cs = 
      (if forall Symbol.is_ascii_upper cs then map else nth_map 0) Symbol.to_ascii_lower cs
  | enforce_case' true cs = 
      nth_map 0 Symbol.to_ascii_upper cs;

fun enforce_case upper = implode o enforce_case' upper o raw_explode;

fun desymbolize perhaps_upper "" =
      if the_default false perhaps_upper then "X" else "x"
  | desymbolize perhaps_upper s =
      let
        val xs as (x :: _) = Symbol.explode s;
        val ys =
          if Symbol.is_ascii_letter x orelse Symbol.is_symbolic x then xs
          else "x" :: xs;
        fun is_valid x =
          Symbol.is_ascii_letter x orelse Symbol.is_ascii_digit x;
        fun sep [] = []
          | sep (xs as "_" :: _) = xs
          | sep xs = "_" :: xs;
        fun desep ("_" :: xs) = xs
          | desep xs = xs;
        fun desymb x xs =
          if is_valid x then x :: xs
          else
            (case Symbol.decode x of
              Symbol.Sym name => "_" :: raw_explode name @ sep xs
            | _ => sep xs);
        val upper_lower = Option.map enforce_case' perhaps_upper |> the_default I;
      in fold_rev desymb ys [] |> desep |> upper_lower |> implode end;

end;
