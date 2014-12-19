(*  Title:      Pure/Tools/proof_general.ML
    Author:     David Aspinall
    Author:     Makarius

Isabelle/Isar configuration for Proof General / Emacs.
See also http://proofgeneral.inf.ed.ac.uk
*)

(*Proof General legacy*)

signature PROOF_GENERAL =
sig
  type category = string
  val category_display: category
  val category_advanced_display: category
  val category_tracing: category
  val category_proof: category
  type pgiptype = string
  val pgipbool: pgiptype
  val pgipint: pgiptype
  val pgipfloat: pgiptype
  val pgipstring: pgiptype
  val preference: category -> string option ->
    (unit -> string) -> (string -> unit) -> string -> string -> string -> unit
  val preference_bool: category -> string option ->
    bool Unsynchronized.ref -> string -> string -> unit
  val preference_int: category -> string option ->
    int Unsynchronized.ref -> string -> string -> unit
  val preference_real: category -> string option ->
    real Unsynchronized.ref -> string -> string -> unit
  val preference_string: category -> string option ->
    string Unsynchronized.ref -> string -> string -> unit
  val preference_option: category -> string option -> string -> string -> string -> unit
  val process_pgip: string -> unit
  val tell_clear_goals: unit -> unit
  val tell_clear_response: unit -> unit
  val inform_file_processed: string -> unit
  val inform_file_retracted: string -> unit
  val master_path: Path.T Unsynchronized.ref
  structure ThyLoad: sig val add_path: string -> unit end
  val thm_deps: bool Unsynchronized.ref
  val proof_generalN: string
  val init: unit -> unit
  val restart: unit -> unit
end;

structure ProofGeneral: PROOF_GENERAL =
struct

(** preferences **)

(* type preference *)

type category = string;
val category_display = "Display";
val category_advanced_display = "Advanced Display";
val category_tracing = "Tracing";
val category_proof = "Proof";

type pgiptype = string;
val pgipbool = "pgipbool";
val pgipint = "pgipint";
val pgipfloat = "pgipint";  (*NB: PG 3.7.x and 4.0 lack pgipfloat, but accept floats as pgipint*)
val pgipstring = "pgipstring";

type preference =
 {category: string,
  override: string option,
  descr: string,
  pgiptype: pgiptype,
  get: unit -> string,
  set: string -> unit};


(* global preferences *)

local
  val preferences =
    Synchronized.var "ProofGeneral.preferences" ([]: (string * preference) list);
in

fun add_preference name pref =
  Synchronized.change preferences (fn prefs =>
   (if not (AList.defined (op =) prefs name) then ()
    else warning ("Redefining ProofGeneral preference: " ^ quote name);
    AList.update (op =) (name, pref) prefs));

fun set_preference name value =
  (case AList.lookup (op =) (Synchronized.value preferences) name of
    SOME {set, ...} => set value
  | NONE => error ("Unknown ProofGeneral preference: " ^ quote name));

fun all_preferences () =
  rev (Synchronized.value preferences)
  |> map (fn (name, {category, descr, pgiptype, get, ...}) =>
    (category, {name = name, descr = descr, default = get (), pgiptype = pgiptype}))
  |> AList.group (op =);

fun init_preferences () =
  Synchronized.value preferences
  |> List.app (fn (_, {set, override = SOME value, ...}) => set value | _ => ());

end;



(* raw preferences *)

fun preference category override get set typ name descr =
  add_preference name
    {category = category, override = override, descr = descr, pgiptype = typ, get = get, set = set};

fun preference_ref category override read write typ r =
  preference category override (fn () => read (! r)) (fn x => r := write x) typ;

fun preference_bool x y = preference_ref x y Markup.print_bool Markup.parse_bool pgipbool;
fun preference_int x y = preference_ref x y Markup.print_int Markup.parse_int pgipint;
fun preference_real x y = preference_ref x y Markup.print_real Markup.parse_real pgipfloat;
fun preference_string x y = preference_ref x y I I pgipstring;


(* system options *)

fun preference_option category override option_name pgip_name descr =
  let
    val typ = Options.default_typ option_name;
    val pgiptype =
      if typ = Options.boolT then pgipbool
      else if typ = Options.intT then pgipint
      else if typ = Options.realT then pgipfloat
      else pgipstring;
  in
    add_preference pgip_name
     {category = category,
      override = override,
      descr = descr,
      pgiptype = pgiptype,
      get = fn () => Options.get_default option_name,
      set = Options.put_default option_name}
  end;


(* minimal PGIP support for <askprefs>, <haspref>, <setpref> *)

local

fun get_attr attrs name =
  (case Properties.get attrs name of
    SOME value => value
  | NONE => raise Fail ("Missing attribute: " ^ quote name));

fun attr x y = [(x, y)] : XML.attributes;

fun opt_attr _ NONE = []
  | opt_attr name (SOME value) = attr name value;

val pgip_id = "dummy";
val pgip_serial = Counter.make ();

fun output_pgip refid refseq content =
  XML.Elem (("pgip",
    attr "tag" "Isabelle/Isar" @
    attr "id" pgip_id @
    opt_attr "destid" refid @
    attr "class" "pg" @
    opt_attr "refid" refid @
    attr "refseq" refseq @
    attr "seq" (string_of_int (pgip_serial ()))), content)
  |> XML.string_of
  |> Output.urgent_message;


fun invalid_pgip () = raise Fail "Invalid PGIP packet";

fun haspref {name, descr, default, pgiptype} =
  XML.Elem (("haspref", [("name", name), ("descr", descr), ("default", default)]),
    [XML.Elem ((pgiptype, []), [])]);

fun process_element refid refseq (XML.Elem (("askprefs", _), _)) =
      all_preferences () |> List.app (fn (category, prefs) =>
        output_pgip refid refseq
          [XML.Elem (("hasprefs", [("prefcategory", category)]), map haspref prefs)])
  | process_element _ _ (XML.Elem (("setpref", attrs), data)) =
      let
        val name =
          (case Properties.get attrs "name" of
            SOME name => name
          | NONE => invalid_pgip ());
        val value = XML.content_of data;
      in set_preference name value end
  | process_element _ _ _ = invalid_pgip ();

in

fun process_pgip str =
  (case XML.parse str of
    XML.Elem (("pgip", attrs), pgips) =>
      let
        val class = get_attr attrs "class";
        val dest = Properties.get attrs "destid";
        val refid = Properties.get attrs "id";
        val refseq = get_attr attrs "seq";
        val processit =
          (case dest of
            NONE => class = "pa"
          | SOME id => id = pgip_id);
       in if processit then List.app (process_element refid refseq) pgips else () end
  | _ => invalid_pgip ())
  handle Fail msg => raise Fail (msg ^ "\n" ^ str);

end;


(** messages **)

(* render markup *)

fun special ch = chr 1 ^ ch;

local

fun render_trees ts = fold render_tree ts
and render_tree t =
  (case XML.unwrap_elem t of
    SOME (_, ts) => render_trees ts
  | NONE =>
      (case t of
        XML.Text s => Buffer.add s
      | XML.Elem ((name, props), ts) =>
          let
            val (bg, en) =
              if null ts then Markup.no_output
              else if name = Markup.stateN then (special "O" ^ "\n", "\n" ^ special "P")
              else if name = Markup.sendbackN then (special "W", special "X")
              else if name = Markup.intensifyN then (special "0", special "1")
              else if name = Markup.informationN then ("\n" ^ special "0", special "1")
              else if name = Markup.tfreeN then (special "C", special "A")
              else if name = Markup.tvarN then (special "D", special "A")
              else if name = Markup.freeN then (special "E", special "A")
              else if name = Markup.boundN then (special "F", special "A")
              else if name = Markup.varN then (special "G", special "A")
              else if name = Markup.skolemN then (special "H", special "A")
              else
                (case Markup.get_entity_kind (name, props) of
                  SOME kind =>
                    if kind = Markup.classN then (special "B", special "A")
                    else Markup.no_output
                | NONE => Markup.no_output);
          in Buffer.add bg #> render_trees ts #> Buffer.add en end));

in

fun render text =
  Buffer.content (render_trees (YXML.parse_body text) Buffer.empty);

end;


(* hooks *)

fun message bg en prfx body =
  (case render (implode body) of
    "" => ()
  | s => Output.physical_writeln (enclose bg en (prefix_lines prfx s)));

fun setup_messages () =
 (Output.writeln_fn := message "" "" "";
  Output.status_fn := (fn _ => ());
  Output.report_fn := (fn _ => ());
  Output.urgent_message_fn := message (special "I") (special "J") "";
  Output.tracing_fn := message (special "I" ^ special "V") (special "J") "";
  Output.warning_fn := message (special "K") (special "L") "### ";
  Output.error_message_fn := (fn (_, s) => message (special "M") (special "N") "*** " s);
  Output.prompt_fn := (fn s => Output.physical_stdout (render s ^ special "S")));


(* notification *)

fun emacs_notify s = message (special "I") (special "J") "" [s];

fun tell_clear_goals () =
  emacs_notify "Proof General, please clear the goals buffer.";

fun tell_clear_response () =
  emacs_notify "Proof General, please clear the response buffer.";

fun tell_file_loaded path =
  emacs_notify ("Proof General, this file is loaded: " ^ quote (File.platform_path path));

fun tell_file_retracted path =
  emacs_notify ("Proof General, you can unlock the file " ^ quote (File.platform_path path));



(** theory loader **)

(* global master path *)

val master_path = Unsynchronized.ref Path.current;

(*fake old ThyLoad -- with new semantics*)
structure ThyLoad =
struct
  fun add_path path = master_path := Path.explode path;
end;


(* actions *)

local

fun trace_action action name =
  if action = Thy_Info.Update then
    List.app tell_file_loaded (Thy_Info.loaded_files name)
  else if action = Thy_Info.Remove then
    List.app tell_file_retracted (Thy_Info.loaded_files name)
  else ();

in
  fun setup_thy_loader () = Thy_Info.add_hook trace_action;
  fun sync_thy_loader () = List.app (trace_action Thy_Info.Update) (Thy_Info.get_names ());
end;


(* get informed about files *)

(*liberal low-level version*)
val thy_name = perhaps (try (unsuffix ".thy")) o List.last o space_explode "/";

val inform_file_retracted = Thy_Info.kill_thy o thy_name;

fun inform_file_processed file =
  let
    val name = thy_name file;
    val _ = name = "" andalso error ("Bad file name: " ^ quote file);
    val _ =
      Thy_Info.register_thy (Toplevel.end_theory Position.none (Isar.state ()))
        handle ERROR msg =>
          (warning (cat_lines ["Failed to register theory: " ^ quote name, msg]);
            tell_file_retracted (Resources.thy_path (Path.basic name)))
    val _ = Isar.init ();
  in () end;



(** theorem dependencies **)

(* thm_deps *)

local

fun add_proof_body (PBody {thms, ...}) =
  thms |> fold (fn (_, (name, _, _)) => name <> "" ? Symtab.update (name, ()));

fun add_thm th =
  (case Thm.proof_body_of th of
    PBody {proof = PThm (_, ((name, _, _), body)), ...} =>
      if Thm.has_name_hint th andalso Thm.get_name_hint th = name
      then add_proof_body (Future.join body)
      else I
  | body => add_proof_body body);

in

fun get_thm_deps ths =
  let
    (* FIXME proper derivation names!? *)
    val names = map Thm.get_name_hint (filter Thm.has_name_hint ths);
    val deps = Symtab.keys (fold add_thm ths Symtab.empty);
  in (names, deps) end;

end;


(* report via hook *)

val thm_deps = Unsynchronized.ref false;

local

val spaces_quote = space_implode " " o map quote;

fun thm_deps_message (thms, deps) =
  emacs_notify ("Proof General, theorem dependencies of " ^ thms ^ " are " ^ deps);

in

fun setup_present_hook () = Toplevel.add_hook (fn _ => fn state => fn state' =>
  if ! thm_deps andalso can Toplevel.theory_of state andalso Toplevel.is_theory state'
  then
    let
      val prev_facts = Global_Theory.facts_of (Toplevel.theory_of state);
      val facts = Global_Theory.facts_of (Toplevel.theory_of state');
      val (names, deps) = get_thm_deps (maps #2 (Facts.dest_static true [prev_facts] facts));
    in
      if null names orelse null deps then ()
      else thm_deps_message (spaces_quote names, spaces_quote deps)
    end
  else ());

end;



(** startup **)

(* init *)

val proof_generalN = "ProofGeneral";

val initialized = Unsynchronized.ref false;

fun init () =
  (if ! initialized then ()
   else
    (Output.add_mode Symbol.xsymbolsN Symbol.output Output.default_escape;
     Output.add_mode proof_generalN Output.default_output Output.default_escape;
     Markup.add_mode proof_generalN YXML.output_markup;
     setup_messages ();
     setup_thy_loader ();
     setup_present_hook ();
     initialized := true);
   init_preferences ();
   sync_thy_loader ();
   Unsynchronized.change print_mode (update (op =) proof_generalN);
   Secure.PG_setup ();
   Isar.toplevel_loop TextIO.stdIn
    {init = true, welcome = true, sync = true, secure = Secure.is_secure ()});


(* restart *)

val welcome = Output.urgent_message o Session.welcome;

fun restart () =
 (sync_thy_loader ();
  tell_clear_goals ();
  tell_clear_response ();
  Isar.init ();
  welcome ());

end;

