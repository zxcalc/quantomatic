(*  Title:      Pure/General/name_space.ML
    Author:     Markus Wenzel, TU Muenchen

Generic name spaces with declared and hidden entries.  Unknown names
are considered global; no support for absolute addressing.
*)

type xstring = string;    (*external names*)

signature NAME_SPACE =
sig
  val hidden: string -> string
  val is_hidden: string -> bool
  type T
  val empty: string -> T
  val kind_of: T -> string
  val defined_entry: T -> string -> bool
  val the_entry: T -> string ->
    {concealed: bool, group: serial option, theory_name: string, pos: Position.T, id: serial}
  val entry_ord: T -> string * string -> order
  val markup: T -> string -> Markup.T
  val is_concealed: T -> string -> bool
  val intern: T -> xstring -> string
  val names_long_default: bool Unsynchronized.ref
  val names_long_raw: Config.raw
  val names_long: bool Config.T
  val names_short_default: bool Unsynchronized.ref
  val names_short_raw: Config.raw
  val names_short: bool Config.T
  val names_unique_default: bool Unsynchronized.ref
  val names_unique_raw: Config.raw
  val names_unique: bool Config.T
  val extern: Proof.context -> T -> string -> xstring
  val hide: bool -> string -> T -> T
  val merge: T * T -> T
  type naming
  val conceal: naming -> naming
  val get_group: naming -> serial option
  val set_group: serial option -> naming -> naming
  val set_theory_name: string -> naming -> naming
  val new_group: naming -> naming
  val reset_group: naming -> naming
  val add_path: string -> naming -> naming
  val root_path: naming -> naming
  val parent_path: naming -> naming
  val mandatory_path: string -> naming -> naming
  val qualified_path: bool -> binding -> naming -> naming
  val default_naming: naming
  val local_naming: naming
  val transform_binding: naming -> binding -> binding
  val full_name: naming -> binding -> string
  val base_name: binding -> string
  val alias: naming -> binding -> string -> T -> T
  val naming_of: Context.generic -> naming
  val map_naming: (naming -> naming) -> Context.generic -> Context.generic
  val declare: Context.generic -> bool -> binding -> T -> string * T
  type 'a table = T * 'a Symtab.table
  val check: Context.generic -> 'a table -> xstring * Position.T -> string * 'a
  val get: 'a table -> string -> 'a
  val define: Context.generic -> bool -> binding * 'a -> 'a table -> string * 'a table
  val empty_table: string -> 'a table
  val merge_tables: 'a table * 'a table -> 'a table
  val join_tables: (string -> 'a * 'a -> 'a) (*Symtab.SAME*) ->
    'a table * 'a table -> 'a table
  val dest_table: Proof.context -> 'a table -> (string * 'a) list
  val extern_table: Proof.context -> 'a table -> ((Markup.T * xstring) * 'a) list
end;

structure Name_Space: NAME_SPACE =
struct


(** name spaces **)

(* hidden entries *)

fun hidden name = "??." ^ name;
val is_hidden = String.isPrefix "??.";


(* datatype entry *)

type entry =
 {concealed: bool,
  group: serial option,
  theory_name: string,
  pos: Position.T,
  id: serial};

fun entry_markup def kind (name, {pos, id, ...}: entry) =
  Markup.properties (Position.entity_properties_of def id pos) (Markup.entity kind name);

fun print_entry_ref kind (name, entry) =
  quote (Markup.markup (entry_markup false kind (name, entry)) name);

fun err_dup kind entry1 entry2 pos =
  error ("Duplicate " ^ kind ^ " declaration " ^
    print_entry_ref kind entry1 ^ " vs. " ^ print_entry_ref kind entry2 ^ Position.here pos);

fun undefined kind name = "Undefined " ^ kind ^ ": " ^ quote name;


(* datatype T *)

datatype T =
  Name_Space of
   {kind: string,
    internals: (string list * string list) Symtab.table,  (*visible, hidden*)
    entries: (xstring list * entry) Symtab.table};        (*externals, entry*)

fun make_name_space (kind, internals, entries) =
  Name_Space {kind = kind, internals = internals, entries = entries};

fun map_name_space f (Name_Space {kind = kind, internals = internals, entries = entries}) =
  make_name_space (f (kind, internals, entries));

fun map_internals f xname = map_name_space (fn (kind, internals, entries) =>
  (kind, Symtab.map_default (xname, ([], [])) f internals, entries));


fun empty kind = make_name_space (kind, Symtab.empty, Symtab.empty);

fun kind_of (Name_Space {kind, ...}) = kind;

fun defined_entry (Name_Space {entries, ...}) = Symtab.defined entries;

fun the_entry (Name_Space {kind, entries, ...}) name =
  (case Symtab.lookup entries name of
    NONE => error ("Unknown " ^ kind ^ " " ^ quote name)
  | SOME (_, entry) => entry);

fun entry_ord space = int_ord o pairself (#id o the_entry space);

fun markup (Name_Space {kind, entries, ...}) name =
  (case Symtab.lookup entries name of
    NONE => Markup.intensify
  | SOME (_, entry) => entry_markup false kind (name, entry));

fun is_concealed space name = #concealed (the_entry space name);


(* name accesses *)

fun lookup (Name_Space {internals, ...}) xname =
  (case Symtab.lookup internals xname of
    NONE => (xname, true)
  | SOME ([], []) => (xname, true)
  | SOME ([name], _) => (name, true)
  | SOME (name :: _, _) => (name, false)
  | SOME ([], name' :: _) => (hidden name', true));

fun get_accesses (Name_Space {entries, ...}) name =
  (case Symtab.lookup entries name of
    NONE => [name]
  | SOME (externals, _) => externals);

fun valid_accesses (Name_Space {internals, ...}) name =
  Symtab.fold (fn (xname, (names, _)) =>
    if not (null names) andalso hd names = name then cons xname else I) internals [];


(* intern and extern *)

fun intern space xname = #1 (lookup space xname);


val names_long_default = Unsynchronized.ref false;
val names_long_raw = Config.declare "names_long" (fn _ => Config.Bool (! names_long_default));
val names_long = Config.bool names_long_raw;

val names_short_default = Unsynchronized.ref false;
val names_short_raw = Config.declare "names_short" (fn _ => Config.Bool (! names_short_default));
val names_short = Config.bool names_short_raw;

val names_unique_default = Unsynchronized.ref true;
val names_unique_raw = Config.declare "names_unique" (fn _ => Config.Bool (! names_unique_default));
val names_unique = Config.bool names_unique_raw;

fun extern ctxt space name =
  let
    val names_long = Config.get ctxt names_long;
    val names_short = Config.get ctxt names_short;
    val names_unique = Config.get ctxt names_unique;

    fun valid require_unique xname =
      let val (name', is_unique) = lookup space xname
      in name = name' andalso (not require_unique orelse is_unique) end;

    fun ext [] = if valid false name then name else hidden name
      | ext (nm :: nms) = if valid names_unique nm then nm else ext nms;
  in
    if names_long then name
    else if names_short then Long_Name.base_name name
    else ext (get_accesses space name)
  end;


(* modify internals *)

val del_name = map_internals o apfst o remove (op =);
fun del_name_extra name =
  map_internals (apfst (fn [] => [] | x :: xs => x :: remove (op =) name xs));
val add_name = map_internals o apfst o update (op =);
val add_name' = map_internals o apsnd o update (op =);


(* hide *)

fun hide fully name space =
  if not (Long_Name.is_qualified name) then
    error ("Attempt to hide global name " ^ quote name)
  else if is_hidden name then
    error ("Attempt to hide hidden name " ^ quote name)
  else
    let val names = valid_accesses space name in
      space
      |> add_name' name name
      |> fold (del_name name)
        (if fully then names else inter (op =) [Long_Name.base_name name] names)
      |> fold (del_name_extra name) (get_accesses space name)
    end;


(* merge *)

fun merge
  (Name_Space {kind = kind1, internals = internals1, entries = entries1},
    Name_Space {kind = kind2, internals = internals2, entries = entries2}) =
  let
    val kind' =
      if kind1 = kind2 then kind1
      else error ("Attempt to merge different kinds of name spaces " ^
        quote kind1 ^ " vs. " ^ quote kind2);
    val internals' = (internals1, internals2) |> Symtab.join
      (K (fn ((names1, names1'), (names2, names2')) =>
        if pointer_eq (names1, names2) andalso pointer_eq (names1', names2')
        then raise Symtab.SAME
        else (Library.merge (op =) (names1, names2), Library.merge (op =) (names1', names2'))));
    val entries' = (entries1, entries2) |> Symtab.join
      (fn name => fn ((_, entry1), (_, entry2)) =>
        if #id entry1 = #id entry2 then raise Symtab.SAME
        else err_dup kind' (name, entry1) (name, entry2) Position.none);
  in make_name_space (kind', internals', entries') end;



(** naming context **)

(* datatype naming *)

datatype naming = Naming of
 {conceal: bool,
  group: serial option,
  theory_name: string,
  path: (string * bool) list};

fun make_naming (conceal, group, theory_name, path) =
  Naming {conceal = conceal, group = group, theory_name = theory_name, path = path};

fun map_naming f (Naming {conceal, group, theory_name, path}) =
  make_naming (f (conceal, group, theory_name, path));

fun map_path f = map_naming (fn (conceal, group, theory_name, path) =>
  (conceal, group, theory_name, f path));


val conceal = map_naming (fn (_, group, theory_name, path) =>
  (true, group, theory_name, path));

fun set_theory_name theory_name = map_naming (fn (conceal, group, _, path) =>
  (conceal, group, theory_name, path));


fun get_group (Naming {group, ...}) = group;

fun set_group group = map_naming (fn (conceal, _, theory_name, path) =>
  (conceal, group, theory_name, path));

fun new_group naming = set_group (SOME (serial ())) naming;
val reset_group = set_group NONE;

fun add_path elems = map_path (fn path => path @ [(elems, false)]);
val root_path = map_path (fn _ => []);
val parent_path = map_path (perhaps (try (#1 o split_last)));
fun mandatory_path elems = map_path (fn path => path @ [(elems, true)]);

fun qualified_path mandatory binding = map_path (fn path =>
  path @ #2 (Binding.dest (Binding.qualified mandatory "" binding)));

val default_naming = make_naming (false, NONE, "", []);
val local_naming = default_naming |> add_path "local";


(* full name *)

fun err_bad binding = error (Binding.bad binding);

fun transform_binding (Naming {conceal = true, ...}) = Binding.conceal
  | transform_binding _ = I;

fun name_spec (naming as Naming {path, ...}) raw_binding =
  let
    val binding = transform_binding naming raw_binding;
    val (concealed, prefix, name) = Binding.dest binding;
    val _ = Long_Name.is_qualified name andalso err_bad binding;

    val spec1 = maps (fn (a, b) => map (rpair b) (Long_Name.explode a)) (path @ prefix);
    val spec2 = if name = "" then [] else [(name, true)];
    val spec = spec1 @ spec2;
    val _ =
      exists (fn (a, _) => a = "" orelse a = "??" orelse exists_string (fn s => s = "\"") a) spec
      andalso err_bad binding;
  in (concealed, if null spec2 then [] else spec) end;

fun full_name naming =
  name_spec naming #> #2 #> map #1 #> Long_Name.implode;

val base_name = full_name default_naming #> Long_Name.base_name;


(* accesses *)

fun mandatory xs = map_filter (fn (x, true) => SOME x | _ => NONE) xs;

fun mandatory_prefixes xs = mandatory xs :: mandatory_prefixes1 xs
and mandatory_prefixes1 [] = []
  | mandatory_prefixes1 ((x, true) :: xs) = map (cons x) (mandatory_prefixes1 xs)
  | mandatory_prefixes1 ((x, false) :: xs) = map (cons x) (mandatory_prefixes xs);

fun mandatory_suffixes xs = map rev (mandatory_prefixes (rev xs));

fun accesses naming binding =
  let
    val spec = #2 (name_spec naming binding);
    val sfxs = mandatory_suffixes spec;
    val pfxs = mandatory_prefixes spec;
  in pairself (map Long_Name.implode) (sfxs @ pfxs, sfxs) end;


(* alias *)

fun alias naming binding name space =
  let
    val (accs, accs') = accesses naming binding;
    val space' = space
      |> fold (add_name name) accs
      |> map_name_space (fn (kind, internals, entries) =>
        let
          val _ = Symtab.defined entries name orelse error (undefined kind name);
          val entries' = entries
            |> Symtab.map_entry name (fn (externals, entry) =>
              (Library.merge (op =) (externals, accs'), entry))
        in (kind, internals, entries') end);
  in space' end;



(** context naming **)

structure Data_Args =
struct
  type T = naming;
  val empty = default_naming;
  fun extend _ = default_naming;
  fun merge _ = default_naming;
  fun init _ = local_naming;
end;

structure Global_Naming = Theory_Data(Data_Args);
structure Local_Naming = Proof_Data(Data_Args);

fun naming_of (Context.Theory thy) = Global_Naming.get thy
  | naming_of (Context.Proof ctxt) = Local_Naming.get ctxt;

fun map_naming f (Context.Theory thy) = Context.Theory (Global_Naming.map f thy)
  | map_naming f (Context.Proof ctxt) = Context.Proof (Local_Naming.map f ctxt);



(** entry definition **)

(* declaration *)

fun new_entry strict (name, (externals, entry)) =
  map_name_space (fn (kind, internals, entries) =>
    let
      val entries' =
        (if strict then Symtab.update_new else Symtab.update) (name, (externals, entry)) entries
          handle Symtab.DUP dup =>
            err_dup kind (dup, #2 (the (Symtab.lookup entries dup))) (name, entry) (#pos entry);
    in (kind, internals, entries') end);

fun declare context strict binding space =
  let
    val naming = naming_of context;
    val Naming {group, theory_name, ...} = naming;
    val (concealed, spec) = name_spec naming binding;
    val (accs, accs') = accesses naming binding;

    val name = Long_Name.implode (map fst spec);
    val _ = name = "" andalso err_bad binding;

    val (proper_pos, pos) = Position.default (Binding.pos_of binding);
    val entry =
     {concealed = concealed,
      group = group,
      theory_name = theory_name,
      pos = pos,
      id = serial ()};
    val space' = space
      |> fold (add_name name) accs
      |> new_entry strict (name, (accs', entry));
    val _ =
      if proper_pos then
        Context_Position.report_generic context pos
          (entry_markup true (kind_of space) (name, entry))
      else ();
  in (name, space') end;


(* definition in symbol table *)

type 'a table = T * 'a Symtab.table;

fun check context (space, tab) (xname, pos) =
  let val name = intern space xname in
    (case Symtab.lookup tab name of
      SOME x => (Context_Position.report_generic context pos (markup space name); (name, x))
    | NONE => error (undefined (kind_of space) name ^ Position.here pos))
  end;

fun get (space, tab) name =
  (case Symtab.lookup tab name of
    SOME x => x
  | NONE => error (undefined (kind_of space) name));

fun define context strict (binding, x) (space, tab) =
  let val (name, space') = declare context strict binding space
  in (name, (space', Symtab.update (name, x) tab)) end;

fun empty_table kind = (empty kind, Symtab.empty);

fun merge_tables ((space1, tab1), (space2, tab2)) =
  (merge (space1, space2), Symtab.merge (K true) (tab1, tab2));

fun join_tables f ((space1, tab1), (space2, tab2)) =
  (merge (space1, space2), Symtab.join f (tab1, tab2));

fun ext_table ctxt (space, tab) =
  Symtab.fold (fn (name, x) => cons ((name, extern ctxt space name), x)) tab []
  |> Library.sort_wrt (#2 o #1);

fun dest_table ctxt table = map (apfst #1) (ext_table ctxt table);

fun extern_table ctxt (space, tab) =
  map (fn ((name, xname), x) => ((markup space name, xname), x)) (ext_table ctxt (space, tab));

end;

