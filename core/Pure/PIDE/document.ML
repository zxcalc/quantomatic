(*  Title:      Pure/PIDE/document.ML
    Author:     Makarius

Document as collection of named nodes, each consisting of an editable
list of commands, associated with asynchronous execution process.
*)

signature DOCUMENT =
sig
  val timing: bool Unsynchronized.ref
  type node_header = string * Thy_Header.header * string list
  type overlay = Document_ID.command * (string * string list)
  datatype node_edit =
    Edits of (Document_ID.command option * Document_ID.command option) list |
    Deps of node_header |
    Perspective of bool * Document_ID.command list * overlay list
  type edit = string * node_edit
  type state
  val init_state: state
  val define_blob: string -> string -> state -> state
  type blob_digest = (string * string option) Exn.result
  val define_command: Document_ID.command -> string -> blob_digest list -> string ->
    state -> state
  val remove_versions: Document_ID.version list -> state -> state
  val start_execution: state -> state
  val update: Document_ID.version -> Document_ID.version -> edit list -> state ->
    Document_ID.exec list * (Document_ID.command * Document_ID.exec list) list * state
  val state: unit -> state
  val change_state: (state -> state) -> unit
end;

structure Document: DOCUMENT =
struct

val timing = Unsynchronized.ref false;
fun timeit msg e = cond_timeit (! timing) msg e;



(** document structure **)

fun err_dup kind id = error ("Duplicate " ^ kind ^ ": " ^ Document_ID.print id);
fun err_undef kind id = error ("Undefined " ^ kind ^ ": " ^ Document_ID.print id);

type node_header = string * Thy_Header.header * string list;

type perspective =
 {required: bool,  (*required node*)
  visible: Inttab.set,  (*visible commands*)
  visible_last: Document_ID.command option,  (*last visible command*)
  overlays: (string * string list) list Inttab.table};  (*command id -> print functions with args*)

structure Entries = Linear_Set(type key = Document_ID.command val ord = int_ord);

abstype node = Node of
 {header: node_header,  (*master directory, theory header, errors*)
  perspective: perspective,  (*command perspective*)
  entries: Command.exec option Entries.T,  (*command entries with excecutions*)
  result: Command.eval option}  (*result of last execution*)
and version = Version of node String_Graph.T  (*development graph wrt. static imports*)
with

fun make_node (header, perspective, entries, result) =
  Node {header = header, perspective = perspective, entries = entries, result = result};

fun map_node f (Node {header, perspective, entries, result}) =
  make_node (f (header, perspective, entries, result));

fun make_perspective (required, command_ids, overlays) : perspective =
 {required = required,
  visible = Inttab.make_set command_ids,
  visible_last = try List.last command_ids,
  overlays = Inttab.make_list overlays};

val no_header: node_header = ("", Thy_Header.make ("", Position.none) [] [], []);
val no_perspective = make_perspective (false, [], []);

val empty_node = make_node (no_header, no_perspective, Entries.empty, NONE);

fun is_no_perspective ({required, visible, visible_last, overlays}: perspective) =
  not required andalso
  Inttab.is_empty visible andalso
  is_none visible_last andalso
  Inttab.is_empty overlays;

fun is_empty_node (Node {header, perspective, entries, result}) =
  header = no_header andalso
  is_no_perspective perspective andalso
  Entries.is_empty entries andalso
  is_none result;


(* basic components *)

fun master_directory (Node {header = (master, _, _), ...}) =
  (case try Url.explode master of
    SOME (Url.File path) => path
  | _ => Path.current);

fun set_header header =
  map_node (fn (_, perspective, entries, result) => (header, perspective, entries, result));

fun get_header (Node {header = (master, header, errors), ...}) =
  if null errors then (master, header)
  else error (cat_lines errors);

fun read_header node span =
  let
    val {name = (name, _), imports, keywords} = #2 (get_header node);
    val {name = (_, pos), imports = imports', ...} = Thy_Header.read_tokens span;
  in Thy_Header.make (name, pos) (map #1 imports ~~ map #2 imports') keywords end;

fun get_perspective (Node {perspective, ...}) = perspective;
fun set_perspective args =
  map_node (fn (header, _, entries, result) => (header, make_perspective args, entries, result));

val required_node = #required o get_perspective;
val visible_command = Inttab.defined o #visible o get_perspective;
val visible_last = #visible_last o get_perspective;
val visible_node = is_some o visible_last
val overlays = Inttab.lookup_list o #overlays o get_perspective;

fun map_entries f =
  map_node (fn (header, perspective, entries, result) => (header, perspective, f entries, result));
fun get_entries (Node {entries, ...}) = entries;

fun iterate_entries f = Entries.iterate NONE f o get_entries;
fun iterate_entries_after start f (Node {entries, ...}) =
  (case Entries.get_after entries start of
    NONE => I
  | SOME id => Entries.iterate (SOME id) f entries);

fun get_result (Node {result, ...}) = result;
fun set_result result =
  map_node (fn (header, perspective, entries, _) => (header, perspective, entries, result));

fun changed_result node node' =
  (case (get_result node, get_result node') of
    (SOME eval, SOME eval') => not (Command.eval_eq (eval, eval'))
  | (NONE, NONE) => false
  | _ => true);

fun pending_result node =
  (case get_result node of
    SOME eval => not (Command.eval_finished eval)
  | NONE => false);

fun get_node nodes name = String_Graph.get_node nodes name
  handle String_Graph.UNDEF _ => empty_node;
fun default_node name = String_Graph.default_node (name, empty_node);
fun update_node name f = default_node name #> String_Graph.map_node name f;


(* node edits and associated executions *)

type overlay = Document_ID.command * (string * string list);

datatype node_edit =
  Edits of (Document_ID.command option * Document_ID.command option) list |
  Deps of node_header |
  Perspective of bool * Document_ID.command list * overlay list;

type edit = string * node_edit;

val after_entry = Entries.get_after o get_entries;

fun lookup_entry node id =
  (case Entries.lookup (get_entries node) id of
    NONE => NONE
  | SOME (exec, _) => exec);

fun the_entry node id =
  (case Entries.lookup (get_entries node) id of
    NONE => err_undef "command entry" id
  | SOME (exec, _) => exec);

fun the_default_entry node (SOME id) = (id, the_default Command.no_exec (the_entry node id))
  | the_default_entry _ NONE = (Document_ID.none, Command.no_exec);

fun assign_entry (command_id, exec) node =
  if is_none (Entries.lookup (get_entries node) command_id) then node
  else map_entries (Entries.update (command_id, exec)) node;

fun reset_after id entries =
  (case Entries.get_after entries id of
    NONE => entries
  | SOME next => Entries.update (next, NONE) entries);

val edit_node = map_entries o fold
  (fn (id, SOME id2) => Entries.insert_after id (id2, NONE)
    | (id, NONE) => Entries.delete_after id #> reset_after id);


(* version operations *)

val empty_version = Version String_Graph.empty;

fun nodes_of (Version nodes) = nodes;
val node_of = get_node o nodes_of;

fun cycle_msg names = "Cyclic dependency of " ^ space_implode " via " (map quote names);

fun edit_nodes (name, node_edit) (Version nodes) =
  Version
    (case node_edit of
      Edits edits => update_node name (edit_node edits) nodes
    | Deps (master, header, errors) =>
        let
          val imports = map fst (#imports header);
          val errors1 =
            (Thy_Header.define_keywords header; errors)
              handle ERROR msg => errors @ [msg];
          val nodes1 = nodes
            |> default_node name
            |> fold default_node imports;
          val nodes2 = nodes1
            |> String_Graph.Keys.fold
                (fn dep => String_Graph.del_edge (dep, name)) (String_Graph.imm_preds nodes1 name);
          val (nodes3, errors2) =
            (String_Graph.add_deps_acyclic (name, imports) nodes2, errors1)
              handle String_Graph.CYCLES cs => (nodes2, errors1 @ map cycle_msg cs);
        in String_Graph.map_node name (set_header (master, header, errors2)) nodes3 end
    | Perspective perspective => update_node name (set_perspective perspective) nodes);

fun put_node (name, node) (Version nodes) =
  let
    val nodes1 = update_node name (K node) nodes;
    val nodes2 =
      if String_Graph.is_maximal nodes1 name andalso is_empty_node node
      then String_Graph.del_node name nodes1
      else nodes1;
  in Version nodes2 end;

end;



(** main state -- document structure and execution process **)

type blob_digest = (string * string option) Exn.result;  (*file node name, raw digest*)

type execution =
 {version_id: Document_ID.version,  (*static version id*)
  execution_id: Document_ID.execution,  (*dynamic execution id*)
  delay_request: unit future,  (*pending event timer request*)
  frontier: Future.task Symtab.table};  (*node name -> running execution task*)

val no_execution: execution =
  {version_id = Document_ID.none, execution_id = Document_ID.none,
   delay_request = Future.value (), frontier = Symtab.empty};

fun new_execution version_id delay_request frontier : execution =
  {version_id = version_id, execution_id = Execution.start (),
   delay_request = delay_request, frontier = frontier};

abstype state = State of
 {versions: version Inttab.table,  (*version id -> document content*)
  blobs: (SHA1.digest * string list) Symtab.table,  (*raw digest -> digest, lines*)
  commands: (string * blob_digest list * Token.T list lazy) Inttab.table,
    (*command id -> name, inlined files, command span*)
  execution: execution}  (*current execution process*)
with

fun make_state (versions, blobs, commands, execution) =
  State {versions = versions, blobs = blobs, commands = commands, execution = execution};

fun map_state f (State {versions, blobs, commands, execution}) =
  make_state (f (versions, blobs, commands, execution));

val init_state =
  make_state (Inttab.make [(Document_ID.none, empty_version)],
    Symtab.empty, Inttab.empty, no_execution);


(* document versions *)

fun define_version version_id version =
  map_state (fn (versions, blobs, commands, {delay_request, frontier, ...}) =>
    let
      val versions' = Inttab.update_new (version_id, version) versions
        handle Inttab.DUP dup => err_dup "document version" dup;
      val execution' = new_execution version_id delay_request frontier;
    in (versions', blobs, commands, execution') end);

fun the_version (State {versions, ...}) version_id =
  (case Inttab.lookup versions version_id of
    NONE => err_undef "document version" version_id
  | SOME version => version);

fun delete_version version_id versions =
  Inttab.delete version_id versions
    handle Inttab.UNDEF _ => err_undef "document version" version_id;


(* inlined files *)

fun define_blob digest text =
  map_state (fn (versions, blobs, commands, execution) =>
    let val blobs' = Symtab.update (digest, (SHA1.fake digest, split_lines text)) blobs
    in (versions, blobs', commands, execution) end);

fun the_blob (State {blobs, ...}) digest =
  (case Symtab.lookup blobs digest of
    NONE => error ("Undefined blob: " ^ digest)
  | SOME content => content);

fun resolve_blob state (blob_digest: blob_digest) =
  blob_digest |> Exn.map_result (fn (file_node, raw_digest) =>
    (file_node, Option.map (the_blob state) raw_digest));


(* commands *)

fun define_command command_id name command_blobs text =
  map_state (fn (versions, blobs, commands, execution) =>
    let
      val id = Document_ID.print command_id;
      val span =
        Lazy.lazy (fn () =>
          Position.setmp_thread_data (Position.id_only id)
            (fn () => Thy_Syntax.parse_tokens (Keyword.get_lexicons ()) (Position.id id) text) ());
      val _ =
        Position.setmp_thread_data (Position.id_only id)
          (fn () => Output.status (Markup.markup_only Markup.accepted)) ();
      val commands' =
        Inttab.update_new (command_id, (name, command_blobs, span)) commands
          handle Inttab.DUP dup => err_dup "command" dup;
    in (versions, blobs, commands', execution) end);

fun the_command (State {commands, ...}) command_id =
  (case Inttab.lookup commands command_id of
    NONE => err_undef "command" command_id
  | SOME command => command);

val the_command_name = #1 oo the_command;

end;


(* remove_versions *)

fun remove_versions version_ids state = state |> map_state (fn (versions, _, _, execution) =>
  let
    val _ =
      member (op =) version_ids (#version_id execution) andalso
        error ("Attempt to remove execution version " ^ Document_ID.print (#version_id execution));

    val versions' = fold delete_version version_ids versions;
    val commands' =
      (versions', Inttab.empty) |->
        Inttab.fold (fn (_, version) => nodes_of version |>
          String_Graph.fold (fn (_, (node, _)) => node |>
            iterate_entries (fn ((_, command_id), _) =>
              SOME o Inttab.insert (K true) (command_id, the_command state command_id))));
    val blobs' =
      (commands', Symtab.empty) |->
        Inttab.fold (fn (_, (_, blobs, _)) => blobs |>
          fold (fn Exn.Res (_, SOME b) => Symtab.update (b, the_blob state b) | _ => I));

  in (versions', blobs', commands', execution) end);


(* document execution *)

fun start_execution state = state |> map_state (fn (versions, blobs, commands, execution) =>
  timeit "Document.start_execution" (fn () =>
    let
      val {version_id, execution_id, delay_request, frontier} = execution;

      val delay = seconds (Options.default_real "editor_execution_delay");

      val _ = Future.cancel delay_request;
      val delay_request' = Event_Timer.future (Time.+ (Time.now (), delay));

      val new_tasks =
        nodes_of (the_version state version_id) |> String_Graph.schedule
          (fn deps => fn (name, node) =>
            if visible_node node orelse pending_result node then
              let
                val more_deps =
                  Future.task_of delay_request' :: the_list (Symtab.lookup frontier name);
                fun body () =
                  iterate_entries (fn (_, opt_exec) => fn () =>
                    (case opt_exec of
                      SOME exec =>
                        if Execution.is_running execution_id
                        then SOME (Command.exec execution_id exec)
                        else NONE
                    | NONE => NONE)) node ()
                  handle exn => if Exn.is_interrupt exn then () (*sic!*) else reraise exn;
                val future =
                  (singleton o Future.forks)
                    {name = "theory:" ^ name, group = SOME (Future.new_group NONE),
                      deps = more_deps @ map #2 (maps #2 deps),
                      pri = 0, interrupts = false} body;
              in [(name, Future.task_of future)] end
            else []);
      val frontier' = (fold o fold) Symtab.update new_tasks frontier;
      val execution' =
        {version_id = version_id, execution_id = execution_id,
         delay_request = delay_request', frontier = frontier'};
    in (versions, blobs, commands, execution') end));



(** document update **)

(* exec state assignment *)

type assign_update = Command.exec option Inttab.table;  (*command id -> exec*)

val assign_update_empty: assign_update = Inttab.empty;
fun assign_update_defined (tab: assign_update) command_id = Inttab.defined tab command_id;
fun assign_update_apply (tab: assign_update) node = Inttab.fold assign_entry tab node;

fun assign_update_new upd (tab: assign_update) =
  Inttab.update_new upd tab
    handle Inttab.DUP dup => err_dup "exec state assignment" dup;

fun assign_update_result (tab: assign_update) =
  Inttab.fold (fn (command_id, exec) => cons (command_id, Command.exec_ids exec)) tab [];


(* update *)

local

fun make_required nodes =
  let
    fun all_preds P =
      String_Graph.fold (fn (a, (node, _)) => P node ? cons a) nodes []
      |> String_Graph.all_preds nodes
      |> Symtab.make_set;

    val all_visible = all_preds visible_node;
    val all_required = all_preds required_node;
  in
    Symtab.fold (fn (a, ()) =>
      exists (Symtab.defined all_visible) (String_Graph.immediate_succs nodes a) ?
        Symtab.update (a, ())) all_visible all_required
  end;

fun loaded_theory name =
  (case try (unsuffix ".thy") name of
    SOME a => get_first Thy_Info.lookup_theory [a, Long_Name.base_name a]
  | NONE => NONE);

fun init_theory deps node span =
  let
    val master_dir = master_directory node;
    val header = read_header node span;
    val imports = #imports header;
    val parents =
      imports |> map (fn (import, _) =>
        (case loaded_theory import of
          SOME thy => thy
        | NONE =>
            Toplevel.end_theory (Position.file_only import)
              (case get_result (snd (the (AList.lookup (op =) deps import))) of
                NONE => Toplevel.toplevel
              | SOME eval => Command.eval_result_state eval)));
    val _ = Position.reports (map #2 imports ~~ map Theory.get_markup parents);
  in Resources.begin_theory master_dir header parents end;

fun check_theory full name node =
  is_some (loaded_theory name) orelse
  can get_header node andalso (not full orelse is_some (get_result node));

fun last_common state node_required node0 node =
  let
    fun update_flags prev (visible, initial) =
      let
        val visible' = visible andalso prev <> visible_last node;
        val initial' = initial andalso
          (case prev of
            NONE => true
          | SOME command_id => not (Keyword.is_theory_begin (the_command_name state command_id)));
      in (visible', initial') end;

    fun get_common ((prev, command_id), opt_exec) (_, ok, flags, assign_update) =
      if ok then
        let
          val flags' as (visible', _) = update_flags prev flags;
          val ok' =
            (case (lookup_entry node0 command_id, opt_exec) of
              (SOME (eval0, _), SOME (eval, _)) =>
                Command.eval_eq (eval0, eval) andalso
                  (visible' orelse node_required orelse Command.eval_running eval)
            | _ => false);
          val assign_update' = assign_update |> ok' ?
            (case opt_exec of
              SOME (eval, prints) =>
                let
                  val command_visible = visible_command node command_id;
                  val command_overlays = overlays node command_id;
                  val command_name = the_command_name state command_id;
                in
                  (case Command.print command_visible command_overlays command_name eval prints of
                    SOME prints' => assign_update_new (command_id, SOME (eval, prints'))
                  | NONE => I)
                end
            | NONE => I);
        in SOME (prev, ok', flags', assign_update') end
      else NONE;
    val (common, ok, flags, assign_update') =
      iterate_entries get_common node (NONE, true, (true, true), assign_update_empty);
    val (common', flags') =
      if ok then
        let val last = Entries.get_after (get_entries node) common
        in (last, update_flags last flags) end
      else (common, flags);
  in (assign_update', common', flags') end;

fun illegal_init _ = error "Illegal theory header after end of theory";

fun new_exec state node proper_init command_id' (assign_update, command_exec, init) =
  if not proper_init andalso is_none init then NONE
  else
    let
      val (_, (eval, _)) = command_exec;

      val command_visible = visible_command node command_id';
      val command_overlays = overlays node command_id';
      val (command_name, blob_digests, span0) = the_command state command_id';
      val blobs = map (resolve_blob state) blob_digests;
      val span = Lazy.force span0;

      val eval' =
        Command.eval (fn () => the_default illegal_init init span)
          (master_directory node) blobs span eval;
      val prints' = perhaps (Command.print command_visible command_overlays command_name eval') [];
      val exec' = (eval', prints');

      val assign_update' = assign_update_new (command_id', SOME exec') assign_update;
      val init' = if Keyword.is_theory_begin command_name then NONE else init;
    in SOME (assign_update', (command_id', (eval', prints')), init') end;

fun removed_execs node0 (command_id, exec_ids) =
  subtract (op =) exec_ids (Command.exec_ids (lookup_entry node0 command_id));

in

fun update old_version_id new_version_id edits state =
  let
    val old_version = the_version state old_version_id;
    val new_version = timeit "Document.edit_nodes" (fn () => fold edit_nodes edits old_version);

    val nodes = nodes_of new_version;
    val required = make_required nodes;
    val required0 = make_required (nodes_of old_version);
    val edited = fold (fn (name, _) => Symtab.update (name, ())) edits Symtab.empty;

    val updated = timeit "Document.update" (fn () =>
      nodes |> String_Graph.schedule
        (fn deps => fn (name, node) =>
          (singleton o Future.forks)
            {name = "Document.update", group = NONE,
              deps = map (Future.task_of o #2) deps, pri = 1, interrupts = false}
            (fn () => timeit ("Document.update " ^ name) (fn () =>
              let
                val imports = map (apsnd Future.join) deps;
                val imports_result_changed = exists (#4 o #1 o #2) imports;
                val node_required = Symtab.defined required name;
              in
                if Symtab.defined edited name orelse visible_node node orelse
                  imports_result_changed orelse Symtab.defined required0 name <> node_required
                then
                  let
                    val node0 = node_of old_version name;
                    val init = init_theory imports node;
                    val proper_init =
                      check_theory false name node andalso
                      forall (fn (name, (_, node)) => check_theory true name node) imports;

                    val (print_execs, common, (still_visible, initial)) =
                      if imports_result_changed then (assign_update_empty, NONE, (true, true))
                      else last_common state node_required node0 node;
                    val common_command_exec = the_default_entry node common;

                    val (updated_execs, (command_id', (eval', _)), _) =
                      (print_execs, common_command_exec, if initial then SOME init else NONE)
                      |> (still_visible orelse node_required) ?
                        iterate_entries_after common
                          (fn ((prev, id), _) => fn res =>
                            if not node_required andalso prev = visible_last node then NONE
                            else new_exec state node proper_init id res) node;

                    val assigned_execs =
                      (node0, updated_execs) |-> iterate_entries_after common
                        (fn ((_, command_id0), exec0) => fn res =>
                          if is_none exec0 then NONE
                          else if assign_update_defined updated_execs command_id0 then SOME res
                          else SOME (assign_update_new (command_id0, NONE) res));

                    val last_exec =
                      if command_id' = Document_ID.none then NONE else SOME command_id';
                    val result =
                      if is_none last_exec orelse is_some (after_entry node last_exec) then NONE
                      else SOME eval';

                    val assign_update = assign_update_result assigned_execs;
                    val removed = maps (removed_execs node0) assign_update;
                    val _ = List.app Execution.cancel removed;

                    val node' = node
                      |> assign_update_apply assigned_execs
                      |> set_result result;
                    val assigned_node = SOME (name, node');
                    val result_changed = changed_result node0 node';
                  in ((removed, assign_update, assigned_node, result_changed), node') end
                else (([], [], NONE, false), node)
              end)))
      |> Future.joins |> map #1);

    val removed = maps #1 updated;
    val assign_update = maps #2 updated;
    val assigned_nodes = map_filter #3 updated;

    val state' = state
      |> define_version new_version_id (fold put_node assigned_nodes new_version);

  in (removed, assign_update, state') end;

end;



(** global state **)

val global_state = Synchronized.var "Document.global_state" init_state;

fun state () = Synchronized.value global_state;
val change_state = Synchronized.change global_state;

end;

