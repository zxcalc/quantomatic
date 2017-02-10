(*  Title:      Pure/General/binding.ML
    Author:     Florian Haftmann, TU Muenchen
    Author:     Makarius

Structured name bindings.
*)

type bstring = string;    (*primitive names to be bound*)

signature BINDING =
sig
  eqtype scope
  val new_scope: unit -> scope
  eqtype binding
  val path_of: binding -> (string * bool) list
  val make: bstring * Position.T -> binding
  val pos_of: binding -> Position.T
  val set_pos: Position.T -> binding -> binding
  val reset_pos: binding -> binding
  val default_pos: binding -> binding
  val name: bstring -> binding
  val name_of: binding -> bstring
  val map_name: (bstring -> bstring) -> binding -> binding
  val prefix_name: string -> binding -> binding
  val suffix_name: string -> binding -> binding
  val eq_name: binding * binding -> bool
  val empty: binding
  val is_empty: binding -> bool
  val empty_atts: binding * 'a list
  val is_empty_atts: binding * 'a list -> bool
  val conglomerate: binding list -> binding
  val qualify: bool -> string -> binding -> binding
  val qualify_name: bool -> binding -> string -> binding
  val qualified_name: string -> binding
  val prefix_of: binding -> (string * bool) list
  val map_prefix: ((string * bool) list -> (string * bool) list) -> binding -> binding
  val prefix: bool -> string -> binding -> binding
  val restricted: (bool * scope) option -> binding -> binding
  val concealed: binding -> binding
  val pretty: binding -> Pretty.T
  val print: binding -> string
  val bad: binding -> string
  val check: binding -> unit
  val name_spec: scope list -> (string * bool) list -> binding ->
    {restriction: bool option, concealed: bool, spec: (string * bool) list}
end;

structure Binding: BINDING =
struct

(** representation **)

(* scope of restricted entries *)

datatype scope = Scope of serial;

fun new_scope () = Scope (serial ());


(* binding *)

datatype binding = Binding of
 {restricted: (bool * scope) option,  (*entry is private (true) or qualified (false) wrt. scope*)
  concealed: bool,  (*entry is for foundational purposes -- please ignore*)
  prefix: (string * bool) list,  (*system prefix*)
  qualifier: (string * bool) list,  (*user qualifier*)
  name: bstring,  (*base name*)
  pos: Position.T};  (*source position*)

fun make_binding (restricted, concealed, prefix, qualifier, name, pos) =
  Binding {restricted = restricted, concealed = concealed, prefix = prefix,
    qualifier = qualifier, name = name, pos = pos};

fun map_binding f (Binding {restricted, concealed, prefix, qualifier, name, pos}) =
  make_binding (f (restricted, concealed, prefix, qualifier, name, pos));

fun path_of (Binding {prefix, qualifier, ...}) = prefix @ qualifier;

fun make (name, pos) = make_binding (NONE, false, [], [], name, pos);



(** basic operations **)

(* position *)

fun pos_of (Binding {pos, ...}) = pos;

fun set_pos pos =
  map_binding (fn (restricted, concealed, prefix, qualifier, name, _) =>
    (restricted, concealed, prefix, qualifier, name, pos));

val reset_pos = set_pos Position.none;

fun default_pos b =
  if pos_of b = Position.none then set_pos (Position.thread_data ()) b else b;


(* name *)

fun name name = make (name, Position.none);
fun name_of (Binding {name, ...}) = name;

fun eq_name (b, b') = name_of b = name_of b';

fun map_name f =
  map_binding (fn (restricted, concealed, prefix, qualifier, name, pos) =>
    (restricted, concealed, prefix, qualifier, f name, pos));

val prefix_name = map_name o prefix;
val suffix_name = map_name o suffix;

val empty = name "";
fun is_empty b = name_of b = "";

val empty_atts = (empty, []);
fun is_empty_atts (b, atts) = is_empty b andalso null atts;

fun conglomerate [b] = b
  | conglomerate bs = name (space_implode "_" (map name_of bs));


(* user qualifier *)

fun qualify _ "" = I
  | qualify mandatory qual =
      map_binding (fn (restricted, concealed, prefix, qualifier, name, pos) =>
        (restricted, concealed, prefix, (qual, mandatory) :: qualifier, name, pos));

fun qualify_name mandatory binding name' =
  binding |> map_binding (fn (restricted, concealed, prefix, qualifier, name, pos) =>
    let val qualifier' = if name = "" then qualifier else qualifier @ [(name, mandatory)]
    in (restricted, concealed, prefix, qualifier', name', pos) end);

fun qualified_name "" = empty
  | qualified_name s =
      let val (qualifier, name) = split_last (Long_Name.explode s)
      in make_binding (NONE, false, [], map (rpair false) qualifier, name, Position.none) end;


(* system prefix *)

fun prefix_of (Binding {prefix, ...}) = prefix;

fun map_prefix f =
  map_binding (fn (restricted, concealed, prefix, qualifier, name, pos) =>
    (restricted, concealed, f prefix, qualifier, name, pos));

fun prefix _ "" = I
  | prefix mandatory prfx = map_prefix (cons (prfx, mandatory));


(* visibility flags *)

fun restricted default =
  map_binding (fn (restricted, concealed, prefix, qualifier, name, pos) =>
    (if is_some restricted then restricted else default, concealed, prefix, qualifier, name, pos));

val concealed =
  map_binding (fn (restricted, _, prefix, qualifier, name, pos) =>
    (restricted, true, prefix, qualifier, name, pos));


(* print *)

fun pretty (Binding {prefix, qualifier, name, pos, ...}) =
  if name = "" then Pretty.str "\"\""
  else
    Pretty.markup (Position.markup pos Markup.binding)
      [Pretty.str (Long_Name.implode (map #1 (prefix @ qualifier) @ [name]))]
    |> Pretty.quote;

val print = Pretty.unformatted_string_of o pretty;

val _ = ML_system_pp (fn _ => fn _ => Pretty.to_polyml o pretty o reset_pos);


(* check *)

fun bad binding = "Bad name binding: " ^ print binding ^ Position.here (pos_of binding);

fun check binding =
  if Symbol_Pos.is_identifier (name_of binding) then ()
  else legacy_feature (bad binding);



(** resulting name_spec **)

val bad_specs = ["", "??", "__"];

fun name_spec scopes path binding =
  let
    val Binding {restricted, concealed, prefix, qualifier, name, ...} = binding;
    val _ = Long_Name.is_qualified name andalso error (bad binding);

    val restriction =
      (case restricted of
        NONE => NONE
      | SOME (strict, scope) => if member (op =) scopes scope then NONE else SOME strict);

    val spec1 =
      maps (fn (a, b) => map (rpair b) (Long_Name.explode a)) (path @ prefix @ qualifier);
    val spec2 = if name = "" then [] else [(name, true)];
    val spec = spec1 @ spec2;
    val _ =
      exists (fn (a, _) => member (op =) bad_specs a orelse exists_string (fn s => s = "\"") a) spec
      andalso error (bad binding);
  in {restriction = restriction, concealed = concealed, spec = if null spec2 then [] else spec} end;


end;

type binding = Binding.binding;
