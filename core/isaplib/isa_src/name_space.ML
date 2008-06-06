(*  Title:      Pure/General/name_space.ML
    ID:         $Id: name_space.ML,v 1.49 2008/04/15 14:12:16 wenzelm Exp $
    Author:     Markus Wenzel, TU Muenchen

Generic name spaces with declared and hidden entries.  Unknown names
are considered global; no support for absolute addressing.
*)

type bstring = string;    (*names to be bound*)
type xstring = string;    (*external names*)

signature BASIC_NAME_SPACE =
sig
  val long_names: bool ref
  val short_names: bool ref
  val unique_names: bool ref
end;

signature NAME_SPACE =
sig
  include BASIC_NAME_SPACE
  val hidden: string -> string
  val is_hidden: string -> bool
  val separator: string                 (*single char*)
  val is_qualified: string -> bool
  val implode: string list -> string
  val explode: string -> string list
  val append: string -> string -> string
  val qualified: string -> string -> string
  val base: string -> string
  val qualifier: string -> string
  val map_base: (string -> string) -> string -> string
  type T
  val empty: T
  val intern: T -> xstring -> string
  val extern: T -> string -> xstring
  val hide: bool -> string -> T -> T
  val merge: T * T -> T
  type naming
  val path_of: naming -> string
  val external_names: naming -> string -> string list
  val full: naming -> bstring -> string
  val declare: naming -> string -> T -> T
  val default_naming: naming
  val add_path: string -> naming -> naming
  val no_base_names: naming -> naming
  val qualified_names: naming -> naming
  val sticky_prefix: string -> naming -> naming
  type 'a table = T * 'a Symtab.table
  val empty_table: 'a table
  val extend_table: naming -> (bstring * 'a) list -> 'a table -> 'a table
  val merge_tables: ('a * 'a -> bool) -> 'a table * 'a table -> 'a table
  val dest_table: 'a table -> (string * 'a) list
  val extern_table: 'a table -> (xstring * 'a) list
end;

structure NameSpace: NAME_SPACE =
struct

(** long identifiers **)

fun hidden name = "??." ^ name;
val is_hidden = String.isPrefix "??."

val separator = ".";
val is_qualified = exists_string (fn s => s = separator);

val implode_name = space_implode separator;
val explode_name = space_explode separator;

fun append name1 "" = name1
  | append "" name2 = name2
  | append name1 name2 = name1 ^ separator ^ name2;

fun qualified path name =
  if path = "" orelse name = "" then name
  else path ^ separator ^ name;

fun base "" = ""
  | base name = List.last (explode_name name);

fun qualifier "" = ""
  | qualifier name = implode_name (#1 (split_last (explode_name name)));

fun map_base _ "" = ""
  | map_base f name =
      let val names = explode_name name
      in implode_name (nth_map (length names - 1) f names) end;


(* standard accesses *)

infixr 6 @@;
fun ([] @@ yss) = []
  | ((xs :: xss) @@ yss) = map (fn ys => xs @ ys) yss @ (xss @@ yss);

fun suffixes_prefixes list =
  let
    val (xs, ws) = chop (length list - 1) list;
    val sfxs = suffixes xs @@ [ws];
    val pfxs = prefixes1 xs @@ [ws];
  in (sfxs @ pfxs, sfxs) end;

fun suffixes_prefixes_split i k list =
  let
    val (((xs, ys), zs), ws) = list |> chop i ||>> chop k ||>> chop (length list - (i + k + 1));
    val sfxs =
      [ys] @@ suffixes zs @@ [ws] @
      suffixes1 xs @@ [ys @ zs @ ws];
    val pfxs =
      prefixes1 xs @@ [ys @ ws] @
      [xs @ ys] @@ prefixes1 zs @@ [ws];
  in (sfxs @ pfxs, sfxs) end;



(** name spaces **)

(* datatype T *)

datatype T =
  NameSpace of
    ((string list * string list) * stamp) Symtab.table *   (*internals, hidden internals*)
    (string list * stamp) Symtab.table;                    (*externals*)

val empty = NameSpace (Symtab.empty, Symtab.empty);

fun lookup (NameSpace (tab, _)) xname =
  (case Symtab.lookup tab xname of
    NONE => (xname, true)
  | SOME (([], []), _) => (xname, true)
  | SOME (([name], _), _) => (name, true)
  | SOME ((name :: _, _), _) => (name, false)
  | SOME (([], name' :: _), _) => (hidden name', true));

fun get_accesses (NameSpace (_, tab)) name =
  (case Symtab.lookup tab name of
    NONE => [name]
  | SOME (xnames, _) => xnames);

fun put_accesses name xnames (NameSpace (tab, xtab)) =
  NameSpace (tab, Symtab.update (name, (xnames, stamp ())) xtab);

fun valid_accesses (NameSpace (tab, _)) name = Symtab.fold (fn (xname, ((names, _), _)) =>
  if not (null names) andalso hd names = name then cons xname else I) tab [];


(* intern and extern *)

fun intern space xname = #1 (lookup space xname);

val long_names = ref false;
val short_names = ref false;
val unique_names = ref true;

fun extern space name =
  let
    fun valid unique xname =
      let val (name', uniq) = lookup space xname
      in name = name' andalso (uniq orelse not unique) end;

    fun ext [] = if valid false name then name else hidden name
      | ext (nm :: nms) = if valid (! unique_names) nm then nm else ext nms;
  in
    if ! long_names then name
    else if ! short_names then base name
    else ext (get_accesses space name)
  end;


(* basic operations *)

local

fun map_space f xname (NameSpace (tab, xtab)) =
  NameSpace (Symtab.map_default (xname, (([], []), stamp ()))
    (fn (entry, _) => (f entry, stamp ())) tab, xtab);

in

val del_name = map_space o apfst o remove (op =);
val add_name = map_space o apfst o update (op =);
val add_name' = map_space o apsnd o update (op =);

end;


(* hide *)

fun hide fully name space =
  if not (is_qualified name) then
    error ("Attempt to hide global name " ^ quote name)
  else if is_hidden name then
    error ("Attempt to hide hidden name " ^ quote name)
  else
    let val names = valid_accesses space name in
      space
      |> add_name' name name
      |> fold (del_name name) (if fully then names else names inter_string [base name])
    end;


(* merge *)

fun merge (NameSpace (tab1, xtab1), NameSpace (tab2, xtab2)) =
  let
    val tab' = (tab1, tab2) |> Symtab.join
      (K (fn (((names1, names1'), stamp1), ((names2, names2'), stamp2)) =>
        if stamp1 = stamp2 then raise Symtab.SAME
        else
          ((Library.merge (op =) (names1, names2),
            Library.merge (op =) (names1', names2')), stamp ())));
    val xtab' = (xtab1, xtab2) |> Symtab.join
      (K (fn ((xnames1, stamp1), (xnames2, stamp2)) =>
        if stamp1 = stamp2 then raise Symtab.SAME
        else (Library.merge (op =) (xnames1, xnames2), stamp ())));
  in NameSpace (tab', xtab') end;



(** naming contexts **)

(* datatype naming *)

datatype naming = Naming of
  string *                                                 (*path*)
  ((string -> string -> string) *                          (*qualify*)
   (string list -> string list list * string list list));  (*accesses*)

fun path_of (Naming (path, _)) = path;
fun accesses (Naming (_, (_, accs))) = accs;

fun external_names naming = map implode_name o #2 o accesses naming o explode_name;


(* declarations *)

fun full (Naming (path, (qualify, _))) = qualify path;

fun declare naming name space =
  if is_hidden name then
    error ("Attempt to declare hidden name " ^ quote name)
  else
    let
      val names = explode_name name;
      val _ = (null names orelse exists (fn s => s = "") names
          orelse exists_string (fn s => s = "\"") name) andalso
        error ("Bad name declaration " ^ quote name);
      val (accs, accs') = pairself (map implode_name) (accesses naming names);
    in space |> fold (add_name name) accs |> put_accesses name accs' end;


(* manipulate namings *)

fun reject_qualified name =
  if is_qualified name then
    error ("Attempt to declare qualified name " ^ quote name)
  else name;

val default_naming =
  Naming ("", (fn path => qualified path o reject_qualified, suffixes_prefixes));

fun add_path elems (Naming (path, policy)) =
  if elems = "//" then Naming ("", (qualified, #2 policy))
  else if elems = "/" then Naming ("", policy)
  else if elems = ".." then Naming (qualifier path, policy)
  else Naming (append path elems, policy);

fun no_base_names (Naming (path, (qualify, accs))) =
  Naming (path, (qualify, pairself (filter_out (fn [_] => true | _ => false)) o accs));

fun qualified_names (Naming (path, (_, accs))) = Naming (path, (qualified, accs));

fun sticky_prefix prfx (Naming (path, (qualify, _))) =
  Naming (append path prfx,
    (qualify, suffixes_prefixes_split (length (explode_name path)) (length (explode_name prfx))));



(** name spaces coupled with symbol tables **)

type 'a table = T * 'a Symtab.table;

val empty_table = (empty, Symtab.empty);

fun extend_table naming bentries (space, tab) =
  let val entries = map (apfst (full naming)) bentries
  in (fold (declare naming o #1) entries space, Symtab.extend (tab, entries)) end;

fun merge_tables eq ((space1, tab1), (space2, tab2)) =
  (merge (space1, space2), Symtab.merge eq (tab1, tab2));

fun ext_table (space, tab) =
  Symtab.fold (fn (name, x) => cons ((name, extern space name), x)) tab []
  |> Library.sort_wrt (#2 o #1);

fun dest_table tab = map (apfst #1) (ext_table tab);
fun extern_table tab = map (apfst #2) (ext_table tab);


(*final declarations of this structure!*)
val implode = implode_name;
val explode = explode_name;

end;

structure BasicNameSpace: BASIC_NAME_SPACE = NameSpace;
open BasicNameSpace;
