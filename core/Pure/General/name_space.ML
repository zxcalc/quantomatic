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
  val markup: T -> string -> Markup.T
  val markup_def: T -> string -> Markup.T
  val the_entry: T -> string ->
    {concealed: bool, group: serial option, theory_name: string, pos: Position.T, serial: serial}
  val entry_ord: T -> string * string -> order
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
  val get_scopes: naming -> Binding.scope list
  val get_scope: naming -> Binding.scope option
  val new_scope: naming -> Binding.scope * naming
  val restricted: bool -> Position.T -> naming -> naming
  val private_scope: Binding.scope -> naming -> naming
  val private: Position.T -> naming -> naming
  val qualified_scope: Binding.scope -> naming -> naming
  val qualified: Position.T -> naming -> naming
  val concealed: naming -> naming
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
  val global_naming: naming
  val local_naming: naming
  val transform_naming: naming -> naming -> naming
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
  val defined: 'a table -> string -> bool
  val lookup: 'a table -> string -> 'a option
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
  val extern_entries: bool -> Proof.context -> T -> (string * 'a) list ->
    ((string * xstring) * 'a) list
  val markup_entries: bool -> Proof.context -> T -> (string * 'a) list ->
    ((Markup.T * xstring) * 'a) list
  val extern_table: bool -> Proof.context -> 'a table -> ((string * xstring) * 'a) list
  val markup_table: bool -> Proof.context -> 'a table -> ((Markup.T * xstring) * 'a) list
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
  serial: serial};

fun entry_markup def kind (name, {pos, serial, ...}: entry) =
  Markup.properties (Position.entity_properties_of def serial pos) (Markup.entity kind name);

fun print_entry_ref kind (name, entry) =
  quote (Markup.markup (entry_markup false kind (name, entry)) name);

fun err_dup kind entry1 entry2 pos =
  error ("Duplicate " ^ plain_words kind ^ " declaration " ^
    print_entry_ref kind entry1 ^ " vs. " ^ print_entry_ref kind entry2 ^ Position.here pos);


(* internal names *)

type internals = (string list * string list) Change_Table.T;  (*xname -> visible, hidden*)

fun map_internals f xname : internals -> internals =
  Change_Table.map_default (xname, ([], [])) f;

val del_name = map_internals o apfst o remove (op =);
fun del_name_extra name =
  map_internals (apfst (fn [] => [] | x :: xs => x :: remove (op =) name xs));
val add_name = map_internals o apfst o update (op =);
fun hide_name name = map_internals (apsnd (update (op =) name)) name;


(* external accesses *)

type accesses = (xstring list * xstring list);  (*input / output fragments*)
type entries = (accesses * entry) Change_Table.T;  (*name -> accesses, entry*)


(* datatype T *)

datatype T = Name_Space of {kind: string, internals: internals, entries: entries};

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

fun gen_markup def (Name_Space {kind, entries, ...}) name =
  (case Change_Table.lookup entries name of
    NONE => Markup.intensify
  | SOME (_, entry) => entry_markup def kind (name, entry));

val markup = gen_markup false;
val markup_def = gen_markup true;

fun undefined (space as Name_Space {kind, entries, ...}) bad =
  let
    val (prfx, sfx) =
      (case Long_Name.dest_hidden bad of
        SOME name =>
          if Change_Table.defined entries name
          then ("Inaccessible", Markup.markup (markup space name) (quote name))
          else ("Undefined", quote name)
      | NONE => ("Undefined", quote bad));
  in prfx ^ " " ^ plain_words kind ^ ": " ^ sfx end;

fun the_entry (space as Name_Space {entries, ...}) name =
  (case Change_Table.lookup entries name of
    NONE => error (undefined space name)
  | SOME (_, entry) => entry);

fun entry_ord space = int_ord o apply2 (#serial o the_entry space);

fun is_concealed space name =
  #concealed (the_entry space name) handle ERROR _ => false;


(* intern *)

fun intern' (Name_Space {internals, ...}) xname =
  (case the_default ([], []) (Change_Table.lookup internals xname) of
    ([name], _) => (name, true)
  | (name :: _, _) => (name, false)
  | ([], []) => (Long_Name.hidden xname, true)
  | ([], name' :: _) => (Long_Name.hidden name', true));

val intern = #1 oo intern';

fun get_accesses (Name_Space {entries, ...}) name =
  (case Change_Table.lookup entries name of
    NONE => ([], [])
  | SOME (accesses, _) => accesses);

fun is_valid_access (Name_Space {internals, ...}) name xname =
  (case Change_Table.lookup internals xname of
    SOME (name' :: _, _) => name = name'
  | _ => false);


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
      let val (name', is_unique) = intern' space xname
      in name = name' andalso (not require_unique orelse is_unique) end;

    fun ext [] = if valid false name then name else Long_Name.hidden name
      | ext (nm :: nms) = if valid names_unique nm then nm else ext nms;
  in
    if names_long then name
    else if names_short then Long_Name.base_name name
    else ext (#2 (get_accesses space name))
  end;

fun extern_ord ctxt space = string_ord o apply2 (extern ctxt space);

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
  Completion.make (xname, pos) (fn completed =>
    let
      fun result_ord ((precise1, (xname1, (_, name1))), (precise2, (xname2, (_, name2)))) =
        (case bool_ord (precise2, precise1) of
          EQUAL =>
            (case bool_ord (apply2 (is_some o Long_Name.dest_local) (name2, name1)) of
              EQUAL =>
                (case int_ord (apply2 Long_Name.qualification (xname1, xname2)) of
                  EQUAL => string_ord (xname1, xname2)
                | ord => ord)
            | ord => ord)
        | ord => ord);
      val Name_Space {kind, internals, ...} = space;
      val ext = extern_shortest (Context.proof_of context) space;
      val full = Name.clean xname = "";
    in
      Change_Table.fold
        (fn (xname', (name :: _, _)) =>
              if completed xname' andalso not (is_concealed space name) then
                let val xname'' = ext name in
                  if xname' <> xname'' andalso full then I
                  else cons (xname' = xname'', (xname', (kind, name)))
                end
              else I
          | _ => I) internals []
      |> sort_distinct result_ord
      |> map #2
    end);


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
        if #serial entry1 = #serial entry2 then raise Change_Table.SAME
        else err_dup kind' (name, entry1) (name, entry2) Position.none);
  in make_name_space (kind', internals', entries') end;



(** naming context **)

(* datatype naming *)

datatype naming = Naming of
 {scopes: Binding.scope list,
  restricted: (bool * Binding.scope) option,
  concealed: bool,
  group: serial option,
  theory_name: string,
  path: (string * bool) list};

fun make_naming (scopes, restricted, concealed, group, theory_name, path) =
  Naming {scopes = scopes, restricted = restricted, concealed = concealed,
    group = group, theory_name = theory_name, path = path};

fun map_naming f (Naming {scopes, restricted, concealed, group, theory_name, path}) =
  make_naming (f (scopes, restricted, concealed, group, theory_name, path));


(* scope and access restriction *)

fun get_scopes (Naming {scopes, ...}) = scopes;
val get_scope = try hd o get_scopes;

fun new_scope naming =
  let
    val scope = Binding.new_scope ();
    val naming' =
      naming |> map_naming (fn (scopes, restricted, concealed, group, theory_name, path) =>
        (scope :: scopes, restricted, concealed, group, theory_name, path));
  in (scope, naming') end;

fun restricted_scope strict scope =
  map_naming (fn (scopes, _, concealed, group, theory_name, path) =>
    (scopes, SOME (strict, scope), concealed, group, theory_name, path));

fun restricted strict pos naming =
  (case get_scope naming of
    SOME scope => restricted_scope strict scope naming
  | NONE => error ("Missing local scope -- cannot restrict name space accesses" ^ Position.here pos));

val private_scope = restricted_scope true;
val private = restricted true;

val qualified_scope = restricted_scope false;
val qualified = restricted false;

val concealed = map_naming (fn (scopes, restricted, _, group, theory_name, path) =>
  (scopes, restricted, true, group, theory_name, path));


(* additional structural info *)

fun set_theory_name theory_name = map_naming (fn (scopes, restricted, concealed, group, _, path) =>
  (scopes, restricted, concealed, group, theory_name, path));

fun get_group (Naming {group, ...}) = group;

fun set_group group = map_naming (fn (scopes, restricted, concealed, _, theory_name, path) =>
  (scopes, restricted, concealed, group, theory_name, path));

fun new_group naming = set_group (SOME (serial ())) naming;
val reset_group = set_group NONE;


(* name entry path *)

fun get_path (Naming {path, ...}) = path;

fun map_path f = map_naming (fn (scopes, restricted, concealed, group, theory_name, path) =>
  (scopes, restricted, concealed, group, theory_name, f path));

fun add_path elems = map_path (fn path => path @ [(elems, false)]);
val root_path = map_path (fn _ => []);
val parent_path = map_path (perhaps (try (#1 o split_last)));
fun mandatory_path elems = map_path (fn path => path @ [(elems, true)]);

fun qualified_path mandatory binding = map_path (fn path =>
  path @ Binding.path_of (Binding.qualify_name mandatory binding ""));

val global_naming = make_naming ([], NONE, false, NONE, "", []);
val local_naming = global_naming |> add_path Long_Name.localN;


(* transform *)

fun transform_naming (Naming {restricted = restricted', concealed = concealed', ...}) =
  (case restricted' of
    SOME (strict, scope) => restricted_scope strict scope
  | NONE => I) #>
  concealed' ? concealed;

fun transform_binding (Naming {restricted, concealed, ...}) =
  Binding.restricted restricted #>
  concealed ? Binding.concealed;


(* full name *)

fun name_spec naming binding =
  Binding.name_spec (get_scopes naming) (get_path naming) (transform_binding naming binding);

fun full_name naming =
  name_spec naming #> #spec #> map #1 #> Long_Name.implode;

val base_name = full_name global_naming #> Long_Name.base_name;


(* accesses *)

fun mandatory xs = map_filter (fn (x, true) => SOME x | _ => NONE) xs;

fun mandatory_prefixes xs = mandatory xs :: mandatory_prefixes1 xs
and mandatory_prefixes1 [] = []
  | mandatory_prefixes1 ((x, true) :: xs) = map (cons x) (mandatory_prefixes1 xs)
  | mandatory_prefixes1 ((x, false) :: xs) = map (cons x) (mandatory_prefixes xs);

fun mandatory_suffixes xs = map rev (mandatory_prefixes (rev xs));

fun make_accesses naming binding =
  (case name_spec naming binding of
    {restriction = SOME true, ...} => ([], [])
  | {restriction, spec, ...} =>
      let
        val restrict = is_some restriction ? filter (fn [_] => false | _ => true);
        val sfxs = restrict (mandatory_suffixes spec);
        val pfxs = restrict (mandatory_prefixes spec);
      in apply2 (map Long_Name.implode) (sfxs @ pfxs, sfxs) end);


(* hide *)

fun hide fully name space =
  space |> map_name_space (fn (kind, internals, entries) =>
    let
      val _ = the_entry space name;
      val (accs, accs') = get_accesses space name;
      val xnames = filter (is_valid_access space name) accs;
      val internals' = internals
        |> hide_name name
        |> fold (del_name name)
          (if fully then xnames else inter (op =) [Long_Name.base_name name] xnames)
        |> fold (del_name_extra name) accs';
    in (kind, internals', entries) end);


(* alias *)

fun alias naming binding name space =
  space |> map_name_space (fn (kind, internals, entries) =>
    let
      val _ = the_entry space name;
      val (more_accs, more_accs') = make_accesses naming binding;
      val internals' = internals |> fold (add_name name) more_accs;
      val entries' = entries
        |> Change_Table.map_entry name (apfst (fn (accs, accs') =>
            (fold_rev (update op =) more_accs accs,
             fold_rev (update op =) more_accs' accs')))
    in (kind, internals', entries') end);



(** context naming **)

structure Data_Args =
struct
  type T = naming;
  val empty = global_naming;
  fun extend _ = global_naming;
  fun merge _ = global_naming;
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
    val {concealed, spec, ...} = name_spec naming binding;
    val accesses = make_accesses naming binding;

    val name = Long_Name.implode (map fst spec);
    val _ = name = "" andalso error (Binding.bad binding);

    val (proper_pos, pos) = Position.default (Binding.pos_of binding);
    val entry =
     {concealed = concealed,
      group = group,
      theory_name = theory_name,
      pos = pos,
      serial = serial ()};
    val space' =
      space |> map_name_space (fn (kind, internals, entries) =>
        let
          val internals' = internals |> fold (add_name name) (#1 accesses);
          val entries' =
            (if strict then Change_Table.update_new else Change_Table.update)
              (name, (accesses, entry)) entries
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
          error (undefined space name ^ Position.here_list ps ^
            Markup.markup_report (implode (map Completion.reported_text completions)))
        end)
  end;

fun check context table (xname, pos) =
  let
    val ((name, reports), x) = check_reports context table (xname, [pos]);
    val _ = Position.reports reports;
  in (name, x) end;

fun defined (Table (_, tab)) name = Change_Table.defined tab name;
fun lookup (Table (_, tab)) name = Change_Table.lookup tab name;
fun lookup_key (Table (_, tab)) name = Change_Table.lookup_key tab name;

fun get table name =
  (case lookup_key table name of
    SOME (_, x) => x
  | NONE => error (undefined (space_of_table table) name));

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

fun extern_entries verbose ctxt space entries =
  fold (fn (name, x) =>
    (verbose orelse not (is_concealed space name)) ?
      cons ((name, extern ctxt space name), x)) entries []
  |> sort_by (#2 o #1);

fun markup_entries verbose ctxt space entries =
  extern_entries verbose ctxt space entries
  |> map (fn ((name, xname), x) => ((markup space name, xname), x));

fun extern_table verbose ctxt (Table (space, tab)) =
  extern_entries verbose ctxt space (Change_Table.dest tab);

fun markup_table verbose ctxt (Table (space, tab)) =
  markup_entries verbose ctxt space (Change_Table.dest tab);

end;
