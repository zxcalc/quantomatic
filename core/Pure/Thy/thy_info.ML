(*  Title:      Pure/Thy/thy_info.ML
    Author:     Markus Wenzel, TU Muenchen

Global theory info database, with auto-loading according to theory and
file dependencies.
*)

signature THY_INFO =
sig
  datatype action = Update | Remove
  val add_hook: (action -> string -> unit) -> unit
  val get_names: unit -> string list
  val lookup_theory: string -> theory option
  val get_theory: string -> theory
  val is_finished: string -> bool
  val master_directory: string -> Path.T
  val loaded_files: string -> Path.T list
  val remove_thy: string -> unit
  val kill_thy: string -> unit
  val use_theories:
    {document: bool, last_timing: Toplevel.transition -> Time.time option, master_dir: Path.T} ->
    (string * Position.T) list -> unit
  val use_thys: (string * Position.T) list -> unit
  val use_thy: string * Position.T -> unit
  val script_thy: Position.T -> string -> theory
  val toplevel_begin_theory: Path.T -> Thy_Header.header -> theory
  val register_thy: theory -> unit
  val finish: unit -> unit
end;

structure Thy_Info: THY_INFO =
struct

(** theory loader actions and hooks **)

datatype action = Update | Remove;

local
  val hooks = Synchronized.var "Thy_Info.hooks" ([]: (action -> string -> unit) list);
in
  fun add_hook f = Synchronized.change hooks (cons f);
  fun perform action name =
    List.app (fn f => (try (fn () => f action name) (); ())) (Synchronized.value hooks);
end;



(** thy database **)

(* messages *)

val show_path = space_implode " via " o map quote;

fun cycle_msg names = "Cyclic dependency of " ^ show_path names;


(* derived graph operations *)

fun add_deps name parents G = String_Graph.add_deps_acyclic (name, parents) G
  handle String_Graph.CYCLES namess => error (cat_lines (map cycle_msg namess));

fun new_entry name parents entry =
  String_Graph.new_node (name, entry) #> add_deps name parents;


(* thy database *)

type deps =
 {master: (Path.T * SHA1.digest),  (*master dependencies for thy file*)
  imports: (string * Position.T) list};  (*source specification of imports (partially qualified)*)

fun make_deps master imports : deps = {master = master, imports = imports};

fun master_dir (d: deps option) = the_default Path.current (Option.map (Path.dir o #1 o #master) d);
fun base_name s = Path.implode (Path.base (Path.explode s));

local
  val database =
    Unsynchronized.ref (String_Graph.empty: (deps option * theory option) String_Graph.T);
in
  fun get_thys () = ! database;
  fun change_thys f = NAMED_CRITICAL "Thy_Info" (fn () => Unsynchronized.change database f);
end;


(* access thy graph *)

fun thy_graph f x = f (get_thys ()) x;

fun get_names () = String_Graph.topological_order (get_thys ());


(* access thy *)

fun lookup_thy name =
  SOME (thy_graph String_Graph.get_node name) handle String_Graph.UNDEF _ => NONE;

val known_thy = is_some o lookup_thy;

fun get_thy name =
  (case lookup_thy name of
    SOME thy => thy
  | NONE => error ("Theory loader: nothing known about theory " ^ quote name));


(* access deps *)

val lookup_deps = Option.map #1 o lookup_thy;
val get_deps = #1 o get_thy;

val is_finished = is_none o get_deps;
val master_directory = master_dir o get_deps;


(* access theory *)

fun lookup_theory name =
  (case lookup_thy name of
    SOME (_, SOME theory) => SOME theory
  | _ => NONE);

fun get_theory name =
  (case lookup_theory name of
    SOME theory => theory
  | _ => error ("Theory loader: undefined entry for theory " ^ quote name));

val get_imports = Resources.imports_of o get_theory;

(*Proof General legacy*)
fun loaded_files name = NAMED_CRITICAL "Thy_Info" (fn () =>
  (case get_deps name of
    NONE => []
  | SOME {master = (thy_path, _), ...} => thy_path :: Resources.loaded_files (get_theory name)));



(** thy operations **)

(* main loader actions *)

fun remove_thy name = NAMED_CRITICAL "Thy_Info" (fn () =>
  if is_finished name then error ("Cannot update finished theory " ^ quote name)
  else
    let
      val succs = thy_graph String_Graph.all_succs [name];
      val _ = Output.urgent_message ("Theory loader: removing " ^ commas_quote succs);
      val _ = List.app (perform Remove) succs;
      val _ = change_thys (fold String_Graph.del_node succs);
    in () end);

fun kill_thy name = NAMED_CRITICAL "Thy_Info" (fn () =>
  if known_thy name then remove_thy name
  else ());

fun update_thy deps theory = NAMED_CRITICAL "Thy_Info" (fn () =>
  let
    val name = Context.theory_name theory;
    val parents = map Context.theory_name (Theory.parents_of theory);
    val _ = kill_thy name;
    val _ = map get_theory parents;
    val _ = change_thys (new_entry name parents (SOME deps, SOME theory));
    val _ = perform Update name;
  in () end);


(* scheduling loader tasks *)

datatype result =
  Result of {theory: theory, exec_id: Document_ID.exec,
    present: unit -> unit, commit: unit -> unit, weight: int};

fun theory_result theory =
  Result {theory = theory, exec_id = Document_ID.none, present = I, commit = I, weight = 0};

fun result_theory (Result {theory, ...}) = theory;
fun result_present (Result {present, ...}) = present;
fun result_commit (Result {commit, ...}) = commit;
fun result_ord (Result {weight = i, ...}, Result {weight = j, ...}) = int_ord (j, i);

fun join_theory (Result {theory, exec_id, ...}) =
  let
    (*toplevel proofs and diags*)
    val _ = Future.join_tasks (maps Future.group_snapshot (Execution.peek exec_id));
    (*fully nested proofs*)
    val res = Exn.capture Thm.join_theory_proofs theory;
  in res :: map Exn.Exn (maps Task_Queue.group_status (Execution.peek exec_id)) end;

datatype task =
  Task of Path.T * string list * (theory list -> result) |
  Finished of theory;

fun task_finished (Task _) = false
  | task_finished (Finished _) = true;

fun task_parents deps (parents: string list) = map (the o AList.lookup (op =) deps) parents;

local

val schedule_seq =
  String_Graph.schedule (fn deps => fn (_, task) =>
    (case task of
      Task (_, parents, body) =>
        let
          val result = body (task_parents deps parents);
          val _ = Par_Exn.release_all (join_theory result);
          val _ = result_present result ();
          val _ = result_commit result ();
        in result_theory result end
    | Finished thy => thy)) #> ignore;

val schedule_futures = uninterruptible (fn _ => fn tasks =>
  let
    val futures = tasks
      |> String_Graph.schedule (fn deps => fn (name, task) =>
        (case task of
          Task (_, parents, body) =>
            (singleton o Future.forks)
              {name = "theory:" ^ name, group = NONE,
                deps = map (Future.task_of o #2) deps, pri = 0, interrupts = true}
              (fn () =>
                (case filter (not o can Future.join o #2) deps of
                  [] => body (map (result_theory o Future.join) (task_parents deps parents))
                | bad =>
                    error
                      ("Failed to load theory " ^ quote name ^
                        " (unresolved " ^ commas_quote (map #1 bad) ^ ")")))
        | Finished theory => Future.value (theory_result theory)));

    val results1 = futures
      |> maps (fn future =>
          (case Future.join_result future of
            Exn.Res result => join_theory result
          | Exn.Exn exn => [Exn.Exn exn]));

    val results2 = futures
      |> map_filter (Exn.get_res o Future.join_result)
      |> sort result_ord
      |> Par_List.map (fn result => Exn.capture (result_present result) ());

    (* FIXME more precise commit order (!?) *)
    val results3 = futures
      |> map (fn future => Exn.capture (fn () => result_commit (Future.join future) ()) ());

    (* FIXME avoid global Execution.reset (!??) *)
    val results4 = map Exn.Exn (maps Task_Queue.group_status (Execution.reset ()));

    val _ = Par_Exn.release_all (results1 @ results2 @ results3 @ results4);
  in () end);

in

fun schedule_tasks tasks =
  if not (Multithreading.enabled ()) then schedule_seq tasks
  else if Multithreading.self_critical () then
     (warning "Theory loader: no multithreading within critical section";
      schedule_seq tasks)
  else schedule_futures tasks;

end;


(* require_thy -- checking database entries wrt. the file-system *)

local

fun required_by _ [] = ""
  | required_by s initiators = s ^ "(required by " ^ show_path (rev initiators) ^ ")";

fun load_thy document last_timing initiators update_time deps text (name, pos) keywords parents =
  let
    val _ = kill_thy name;
    val _ = Output.urgent_message ("Loading theory " ^ quote name ^ required_by " " initiators);
    val _ = Output.try_protocol_message (Markup.loading_theory name) [];

    val {master = (thy_path, _), imports} = deps;
    val dir = Path.dir thy_path;
    val header = Thy_Header.make (name, pos) imports keywords;

    val _ = Position.reports (map #2 imports ~~ map Theory.get_markup parents);

    val exec_id = Document_ID.make ();
    val _ =
      Execution.running Document_ID.none exec_id [] orelse
        raise Fail ("Failed to register execution: " ^ Document_ID.print exec_id);

    val text_pos = Position.put_id (Document_ID.print exec_id) (Path.position thy_path);
    val (theory, present, weight) =
      Resources.load_thy document last_timing update_time dir header text_pos text
        (if name = Context.PureN then [ML_Context.the_global_context ()] else parents);
    fun commit () = update_thy deps theory;
  in
    Result {theory = theory, exec_id = exec_id, present = present, commit = commit, weight = weight}
  end;

fun check_deps dir name =
  (case lookup_deps name of
    SOME NONE => (true, NONE, Position.none, get_imports name, [])
  | NONE =>
      let val {master, text, theory_pos, imports, keywords} = Resources.check_thy dir name
      in (false, SOME (make_deps master imports, text), theory_pos, imports, keywords) end
  | SOME (SOME {master, ...}) =>
      let
        val {master = master', text = text', theory_pos = theory_pos', imports = imports',
          keywords = keywords'} = Resources.check_thy dir name;
        val deps' = SOME (make_deps master' imports', text');
        val current =
          #2 master = #2 master' andalso
            (case lookup_theory name of
              NONE => false
            | SOME theory => Resources.loaded_files_current theory);
      in (current, deps', theory_pos', imports', keywords') end);

in

fun require_thys document last_timing initiators dir strs tasks =
      fold_map (require_thy document last_timing initiators dir) strs tasks |>> forall I
and require_thy document last_timing initiators dir (str, require_pos) tasks =
  let
    val path = Path.expand (Path.explode str);
    val name = Path.implode (Path.base path);
    val node_name = File.full_path dir (Resources.thy_path path);
    fun check_entry (Task (node_name', _, _)) =
          if node_name = node_name' then ()
          else
            error ("Incoherent imports for theory " ^ quote name ^
              Position.here require_pos ^ ":\n" ^
              "  " ^ Path.print node_name ^ "\n" ^
              "  " ^ Path.print node_name')
      | check_entry _ = ();
  in
    (case try (String_Graph.get_node tasks) name of
      SOME task => (check_entry task; (task_finished task, tasks))
    | NONE =>
        let
          val dir' = Path.append dir (Path.dir path);
          val _ = member (op =) initiators name andalso error (cycle_msg initiators);

          val (current, deps, theory_pos, imports, keywords) = check_deps dir' name
            handle ERROR msg =>
              cat_error msg
                ("The error(s) above occurred for theory " ^ quote name ^
                  Position.here require_pos ^ required_by "\n" initiators);

          val parents = map (base_name o #1) imports;
          val (parents_current, tasks') =
            require_thys document last_timing (name :: initiators)
              (Path.append dir (master_dir (Option.map #1 deps))) imports tasks;

          val all_current = current andalso parents_current;
          val task =
            if all_current then Finished (get_theory name)
            else
              (case deps of
                NONE => raise Fail "Malformed deps"
              | SOME (dep, text) =>
                  let
                    val update_time = serial ();
                    val load =
                      load_thy document last_timing initiators update_time dep
                        text (name, theory_pos) keywords;
                  in Task (node_name, parents, load) end);

          val tasks'' = new_entry name parents task tasks';
        in (all_current, tasks'') end)
  end;

end;


(* use_thy *)

fun use_theories {document, last_timing, master_dir} imports =
  schedule_tasks (snd (require_thys document last_timing [] master_dir imports String_Graph.empty));

val use_thys = use_theories {document = false, last_timing = K NONE, master_dir = Path.current};
val use_thy = use_thys o single;


(* toplevel scripting -- without maintaining database *)

fun script_thy pos txt =
  let
    val trs = Outer_Syntax.parse pos txt;
    val end_pos = if null trs then pos else Toplevel.pos_of (List.last trs);
    val end_state = fold (Toplevel.command_exception true) trs Toplevel.toplevel;
  in Toplevel.end_theory end_pos end_state end;

fun toplevel_begin_theory master_dir (header: Thy_Header.header) =
  let
    val {name = (name, _), imports, ...} = header;
    val _ = kill_thy name;
    val _ = use_theories {document = false, last_timing = K NONE, master_dir = master_dir} imports;
    val _ = Thy_Header.define_keywords header;
    val parents = map (get_theory o base_name o fst) imports;
  in Resources.begin_theory master_dir header parents end;


(* register theory *)

fun register_thy theory =
  let
    val name = Context.theory_name theory;
    val {master, ...} = Resources.check_thy (Resources.master_directory theory) name;
    val imports = Resources.imports_of theory;
  in
    NAMED_CRITICAL "Thy_Info" (fn () =>
     (kill_thy name;
      Output.urgent_message ("Registering theory " ^ quote name);
      update_thy (make_deps master imports) theory))
  end;


(* finish all theories *)

fun finish () = change_thys (String_Graph.map (fn _ => fn (_, entry) => (NONE, entry)));

end;
