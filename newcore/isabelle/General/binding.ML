(*  Title:      Pure/General/binding.ML
    Author:     Florian Haftmann, TU Muenchen
    Author:     Makarius

Structured name bindings.
*)

type bstring = string;    (*primitive names to be bound*)

signature BINDING =
sig
  type binding
  val dest: binding -> bool * (string * bool) list * bstring
  val make: bstring * Position.T -> binding
  val pos_of: binding -> Position.T
  val name: bstring -> binding
  val name_of: binding -> bstring
  val map_name: (bstring -> bstring) -> binding -> binding
  val prefix_name: string -> binding -> binding
  val suffix_name: string -> binding -> binding
  val eq_name: binding * binding -> bool
  val empty: binding
  val is_empty: binding -> bool
  val qualify: bool -> string -> binding -> binding
  val qualified: bool -> string -> binding -> binding
  val qualified_name: string -> binding
  val prefix_of: binding -> (string * bool) list
  val map_prefix: ((string * bool) list -> (string * bool) list) -> binding -> binding
  val prefix: bool -> string -> binding -> binding
  val conceal: binding -> binding
  val pretty: binding -> Pretty.T
  val print: binding -> string
  val bad: binding -> string
  val check: binding -> unit
end;

structure Binding: BINDING =
struct

(** representation **)

(* datatype *)

abstype binding = Binding of
 {conceal: bool,                    (*internal -- for foundational purposes only*)
  prefix: (string * bool) list,     (*system prefix*)
  qualifier: (string * bool) list,  (*user qualifier*)
  name: bstring,                    (*base name*)
  pos: Position.T}                  (*source position*)
with

fun make_binding (conceal, prefix, qualifier, name, pos) =
  Binding {conceal = conceal, prefix = prefix, qualifier = qualifier, name = name, pos = pos};

fun map_binding f (Binding {conceal, prefix, qualifier, name, pos}) =
  make_binding (f (conceal, prefix, qualifier, name, pos));

fun dest (Binding {conceal, prefix, qualifier, name, ...}) =
  (conceal, prefix @ qualifier, name);



(** basic operations **)

(* name and position *)

fun make (name, pos) = make_binding (false, [], [], name, pos);
fun name name = make (name, Position.none);

fun pos_of (Binding {pos, ...}) = pos;
fun name_of (Binding {name, ...}) = name;

fun eq_name (b, b') = name_of b = name_of b';

fun map_name f =
  map_binding (fn (conceal, prefix, qualifier, name, pos) =>
    (conceal, prefix, qualifier, f name, pos));

val prefix_name = map_name o prefix;
val suffix_name = map_name o suffix;

val empty = name "";
fun is_empty b = name_of b = "";


(* user qualifier *)

fun qualify _ "" = I
  | qualify mandatory qual =
      map_binding (fn (conceal, prefix, qualifier, name, pos) =>
        (conceal, prefix, (qual, mandatory) :: qualifier, name, pos));

fun qualified mandatory name' = map_binding (fn (conceal, prefix, qualifier, name, pos) =>
  let val qualifier' = if name = "" then qualifier else qualifier @ [(name, mandatory)]
  in (conceal, prefix, qualifier', name', pos) end);

fun qualified_name "" = empty
  | qualified_name s =
      let val (qualifier, name) = split_last (Long_Name.explode s)
      in make_binding (false, [], map (rpair false) qualifier, name, Position.none) end;


(* system prefix *)

fun prefix_of (Binding {prefix, ...}) = prefix;

fun map_prefix f =
  map_binding (fn (conceal, prefix, qualifier, name, pos) =>
    (conceal, f prefix, qualifier, name, pos));

fun prefix _ "" = I
  | prefix mandatory prfx = map_prefix (cons (prfx, mandatory));


(* conceal *)

val conceal =
  map_binding (fn (_, prefix, qualifier, name, pos) =>
    (true, prefix, qualifier, name, pos));


(* print *)

fun pretty (Binding {prefix, qualifier, name, pos, ...}) =
  if name = "" then Pretty.str "\"\""
  else
    Pretty.markup (Position.markup pos Markup.binding)
      [Pretty.str (Long_Name.implode (map #1 (prefix @ qualifier) @ [name]))]
    |> Pretty.quote;

val print = Pretty.str_of o pretty;


(* check *)

fun bad binding = "Bad name binding: " ^ print binding ^ Position.here (pos_of binding);

fun check binding =
  if Symbol_Pos.is_identifier (name_of binding) then ()
  else legacy_feature (bad binding);

end;
end;

type binding = Binding.binding;

