(*  Title:      Pure/General/name_space.ML
    Author:     Markus Wenzel, TU Muenchen

Generic name spaces with declared and hidden entries; no support for
absolute addressing.
*)

type xstring = string;    (*external names*)

signature NAME_SPACE =
sig
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
  val names_long_raw: Config.raw
  val names_long: bool Config.T
  val names_short_raw: Config.raw
  val names_short: bool Config.T
  val names_unique_raw: Config.raw
  val names_unique: bool Config.T
  val extern: Proof.context -> T -> string -> xstring
  val extern_ord: Proof.context -> T -> string * string -> order
  val extern_shortest: Proof.context -> T -> string -> xstring
  val markup_extern: Proof.context -> T -> string -> Markup.T * xstring
  val pretty: Proof.context -> T -> string -> Pretty.T
  val completion: Context.generic -> T -> xstring * Position.T -> Completion.T
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
  val hide: bool -> string -> T -> T
  val alias: naming -> binding -> string -> T -> T
  val naming_of: Context.generic -> naming
  val map_naming: (naming -> naming) -> Context.generic -> Context.generic
  val declare: Context.generic -> bool -> binding -> T -> string * T
  type 'a table
  val change_base: bool -> 'a table -> 'a table
  val change_ignore: 'a table -> 'a table
  val space_of_table: 'a table -> T
  val check_reports: Context.generic -> 'a table ->
    xstring * Position.T list -> (string * Position.report list) * 'a
  val check: Context.generic -> 'a table -> xstring * Position.T -> string * 'a
  val lookup_key: 'a table -> string -> (string * 'a) option
  val get: 'a table -> string -> 'a
  val define: Context.generic -> bool -> binding * 'a -> 'a table -> string * 'a table
  val alias_table: naming -> binding -> string -> 'a table -> 'a table
  val hide_table: bool -> string -> 'a table -> 'a table
  val del_table: string -> 'a table -> 'a table
  val map_table_entry: string -> ('a -> 'a) -> 'a table -> 'a table
  val fold_table: (string * 'a -> 'b -> 'b) -> 'a table -> 'b -> 'b
  val empty_table: string -> 'a table
  val merge_tables: 'a table * 'a table -> 'a table
  val join_tables: (string -> 'a * 'a -> 'a) (*exception Change_Table.SAME*) ->
    'a table * 'a table -> 'a table
  val extern_entries: Proof.context -> T -> (string * 'a) list -> ((string * xstring) * 'a) list
  val markup_entries: Proof.context -> T -> (string * 'a) list -> ((Markup.T * xstring) * 'a) list
  val extern_table: Proof.context -> 'a table -> ((string * xstring) * 'a) list
  val markup_table: Proof.context -> 'a table -> ((Markup.T * xstring) * 'a) list
end;

structure Name_Space: NAME_SPACE =
struct


(** name spaces **)

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
  error ("Duplicate " ^ plain_words kind ^ " declaration " ^
    print_entry_ref kind entry1 ^ " vs. " ^ print_entry_ref kind entry2 ^ Position.here pos);

fun undefined kind name = "Undefined " ^ plain_words kind ^ ": " ^ quote name;


(* internal names *)

type internals = (string list * string list) Change_Table.T;  (*xname -> visible, hidden*)

fun map_internals f xname : internals -> internals =
  Change_Table.map_default (xname, ([], [])) f;

val del_name = map_internals o apfst o remove (op =);
fun del_name_extra name =
  map_internals (apfst (fn [] => [] | x :: xs => x :: remove (op =) name xs));
val add_name = map_internals o apfst o update (op =);
val add_name' = map_internals o apsnd o update (op =);


(* datatype T *)

datatype T =
  Name_Space of
   {kind: string, internals: internals,
    entries: (xstring list * entry) Change_Table.T};  (*name -> externals, entry*)

fun make_name_space (kind, internals, entries) =
  Name_Space {kind = kind, internals = internals, entries = entries};

fun map_name_space f (Name_Space {kind = kind, internals = internals, entries = entries}) =
  make_name_space (f (kind, internals, entries));

fun change_base_space begin = map_name_space (fn (kind, internals, entries) =>
  (kind, Change_Table.change_base begin internals, Change_Table.change_base begin entries));

val change_ignore_space = map_name_space (fn (kind, internals, entries) =>
  (kind, Change_Table.change_ignore internals, Change_Table.change_ignore entries));


fun empty kind = make_name_space (kind, Change_Table.empty, Change_Table.empty);

fun kind_of (Name_Space {kind, ...}) = kind;

fun defined_entry (Name_Space {entries, ...}) = Change_Table.defined entries;

fun the_entry (Name_Space {kind, entries, ...}) name =
  (case Change_Table.lookup entries name of
    NONE => error (undefined kind name)
  | SOME (_, entry) => entry);

fun entry_ord space = int_ord o pairself (#id o the_entry space);

fun markup (Name_Space {kind, entries, ...}) name =
  (case Change_Table.lookup entries name of
    NONE => Markup.intensify
  | SOME (_, entry) => entry_markup false kind (name, entry));

fun is_concealed space name = #concealed (the_entry space name);


(* name accesses *)

fun lookup (Name_Space {internals, ...}) xname =
  (case Change_Table.lookup internals xname of
    NONE => (xname, true)
  | SOME ([], []) => (xname, true)
  | SOME ([name], _) => (name, true)
  | SOME (name :: _, _) => (name, false)
  | SOME ([], name' :: _) => (Long_Name.hidden name', true));

fun get_accesses (Name_Space {entries, ...}) name =
  (case Change_Table.lookup entries name of
    NONE => [name]
  | SOME (externals, _) => externals);

fun valid_accesses (Name_Space {internals, ...}) name =
  Change_Table.fold (fn (xname, (names, _)) =>
    if not (null names) andalso hd names = name then cons xname else I) internals [];


(* intern *)

fun intern space xname = #1 (lookup space xname);


(* extern *)

val names_long_raw = Config.declare_option ("names_long", @{here});
val names_long = Config.bool names_long_raw;

val names_short_raw = Config.declare_option ("names_short", @{here});
val names_short = Config.bool names_short_raw;

val names_unique_raw = Config.declare_option ("names_unique", @{here});
val names_unique = Config.bool names_unique_raw;

fun extern ctxt space name =
  let
    val names_long = Config.get ctxt names_long;
    val names_short = Config.get ctxt names_short;
    val names_unique = Config.get ctxt names_unique;

    fun valid require_unique xname =
      let val (name', is_unique) = lookup space xname
      in name = name' andalso (not require_unique orelse is_unique) end;

    fun ext [] = if valid false name then name else Long_Name.hidden name
      | ext (nm :: nms) = if valid names_unique nm then nm else ext nms;
  in
    if names_long then name
    else if names_short then Long_Name.base_name name
    else ext (get_accesses space name)
  end;

fun extern_ord ctxt space = string_ord o pairself (extern ctxt space);

fun extern_shortest ctxt =
  extern
    (ctxt
      |> Config.put names_long false
      |> Config.put names_short false
      |> Config.put names_unique false);

fun markup_extern ctxt space name = (markup space name, extern ctxt space name);
fun pretty ctxt space name = Pretty.mark_str (markup_extern ctxt space name);


(* completion *)

fun completion context space (xname, pos) =
  if Position.is_reported pos andalso xname <> "" andalso xname <> "_" then
    let
      fun result_ord ((xname1, (_, name1)), (xname2, (_, name2))) =
        (case bool_ord (pairself Long_Name.is_local (name2, name1)) of
          EQUAL =>
            (case int_ord (pairself Long_Name.qualification (xname1, xname2)) of
              EQUAL => string_ord (xname1, xname2)
            | ord => ord)
        | ord => ord);
      val x = Name.clean xname;
      val Name_Space {kind, internals, ...} = space;
      val ext = extern_shortest (Context.proof_of context) space;
      val names =
        Change_Table.fold
          (fn (a, (name :: _, _)) =>
              if String.isPrefix x a andalso not (is_concealed space name)
              then
                let val a' = ext name
                in if a = a' then cons (a', (kind, name)) else I end
              else I
            | _ => I) internals []
        |> sort_distinct result_ord;
    in Completion.names pos names end
  else Completion.none;


(* merge *)

fun merge
  (Name_Space {kind = kind1, internals = internals1, entries = entries1},
    Name_Space {kind = kind2, internals = internals2, entries = entries2}) =
  let
    val kind' =
      if kind1 = kind2 then kind1
      else error ("Attempt to merge different kinds of name spaces " ^
        quote kind1 ^ " vs. " ^ quote kind2);
    val internals' = (internals1, internals2) |> Change_Table.join
      (K (fn ((names1, names1'), (names2, names2')) =>
        if pointer_eq (names1, names2) andalso pointer_eq (names1', names2')
        then raise Change_Table.SAME
        else (Library.merge (op =) (names1, names2), Library.merge (op =) (names1', names2'))));
    val entries' = (entries1, entries2) |> Change_Table.join
      (fn name => fn ((_, entry1), (_, entry2)) =>
        if #id entry1 = #id entry2 then raise Change_Table.SAME
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
val local_naming = default_naming |> add_path Long_Name.localN;


(* full name *)

fun err_bad binding = error (Binding.bad binding);

fun transform_binding (Naming {conceal = true, ...}) = Binding.conceal
  | transform_binding _ = I;

val bad_specs = ["", "??", "__"];

fun name_spec (naming as Naming {path, ...}) raw_binding =
  let
    val binding = transform_binding naming raw_binding;
    val (concealed, prefix, name) = Binding.dest binding;
    val _ = Long_Name.is_qualified name andalso err_bad binding;

    val spec1 = maps (fn (a, b) => map (rpair b) (Long_Name.explode a)) (path @ prefix);
    val spec2 = if name = "" then [] else [(name, true)];
    val spec = spec1 @ spec2;
    val _ =
      exists (fn (a, _) => member (op =) bad_specs a orelse exists_string (fn s => s = "\"") a) spec
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


(* hide *)

fun hide fully name space =
  space |> map_name_space (fn (kind, internals, entries) =>
    let
      val _ = Change_Table.defined entries name orelse error (undefined kind name);
      val names = valid_accesses space name;
      val internals' = internals
        |> add_name' name name
        |> fold (del_name name)
          (if fully then names else inter (op =) [Long_Name.base_name name] names)
        |> fold (del_name_extra name) (get_accesses space name);
    in (kind, internals', entries) end);


(* alias *)

fun alias naming binding name space =
  space |> map_name_space (fn (kind, internals, entries) =>
    let
      val _ = Change_Table.defined entries name orelse error (undefined kind name);
      val (accs, accs') = accesses naming binding;
      val internals' = internals |> fold (add_name name) accs;
      val entries' = entries
        |> Change_Table.map_entry name (fn (externals, entry) =>
          (Library.merge (op =) (externals, accs'), entry))
    in (kind, internals', entries') end);



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
    val space' =
      space |> map_name_space (fn (kind, internals, entries) =>
        let
          val internals' = internals |> fold (add_name name) accs;
          val entries' =
            (if strict then Change_Table.update_new else Change_Table.update)
              (name, (accs', entry)) entries
            handle Change_Table.DUP dup =>
              err_dup kind (dup, #2 (the (Change_Table.lookup entries dup)))
                (name, entry) (#pos entry);
        in (kind, internals', entries') end);
    val _ =
      if proper_pos andalso Context_Position.is_reported_generic context pos then
        Position.report pos (entry_markup true (kind_of space) (name, entry))
      else ();
  in (name, space') end;


(* definition in symbol table *)

datatype 'a table = Table of T * 'a Change_Table.T;

fun change_base begin (Table (space, tab)) =
  Table (change_base_space begin space, Change_Table.change_base begin tab);

fun change_ignore (Table (space, tab)) =
  Table (change_ignore_space space, Change_Table.change_ignore tab);

fun space_of_table (Table (space, _)) = space;

fun check_reports context (Table (space, tab)) (xname, ps) =
  let val name = intern space xname in
    (case Change_Table.lookup tab name of
      SOME x =>
        let
          val reports =
            filter (Context_Position.is_reported_generic context) ps
            |> map (fn pos => (pos, markup space name));
        in ((name, reports), x) end
    | NONE =>
        let
          val completions = map (fn pos => completion context space (xname, pos)) ps;
        in
          error (undefined (kind_of space) name ^ Position.here_list ps ^
            Markup.markup_report (implode (map Completion.reported_text completions)))
        end)
  end;

fun check context table (xname, pos) =
  let
    val ((name, reports), x) = check_reports context table (xname, [pos]);
    val _ = Position.reports reports;
  in (name, x) end;

fun lookup_key (Table (_, tab)) name = Change_Table.lookup_key tab name;

fun get table name =
  (case lookup_key table name of
    SOME (_, x) => x
  | NONE => error (undefined (kind_of (space_of_table table)) name));

fun define context strict (binding, x) (Table (space, tab)) =
  let
    val (name, space') = declare context strict binding space;
    val tab' = Change_Table.update (name, x) tab;
  in (name, Table (space', tab')) end;


(* derived table operations *)

fun alias_table naming binding name (Table (space, tab)) =
  Table (alias naming binding name space, tab);

fun hide_table fully name (Table (space, tab)) =
  Table (hide fully name space, tab);

fun del_table name (Table (space, tab)) =
  let
    val space' = hide true name space handle ERROR _ => space;
    val tab' = Change_Table.delete_safe name tab;
  in Table (space', tab') end;

fun map_table_entry name f (Table (space, tab)) =
  Table (space, Change_Table.map_entry name f tab);

fun fold_table f (Table (_, tab)) = Change_Table.fold f tab;

fun empty_table kind = Table (empty kind, Change_Table.empty);

fun merge_tables (Table (space1, tab1), Table (space2, tab2)) =
  Table (merge (space1, space2), Change_Table.merge (K true) (tab1, tab2));

fun join_tables f (Table (space1, tab1), Table (space2, tab2)) =
  Table (merge (space1, space2), Change_Table.join f (tab1, tab2));


(* present table content *)

fun extern_entries ctxt space entries =
  fold (fn (name, x) => cons ((name, extern ctxt space name), x)) entries []
  |> Library.sort_wrt (#2 o #1);

fun markup_entries ctxt space entries =
  extern_entries ctxt space entries
  |> map (fn ((name, xname), x) => ((markup space name, xname), x));

fun extern_table ctxt (Table (space, tab)) = extern_entries ctxt space (Change_Table.dest tab);
fun markup_table ctxt (Table (space, tab)) = markup_entries ctxt space (Change_Table.dest tab);

end;

