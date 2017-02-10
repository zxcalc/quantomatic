(*  Title:      Pure/PIDE/session.ML
    Author:     Makarius

Prover session: persistent state of logic image.
*)

signature SESSION =
sig
  val get_name: unit -> string
  val welcome: unit -> string
  val get_keywords: unit -> Keyword.keywords
  val init: HTML.symbols -> bool -> bool -> Path.T -> string -> string -> (string * string) list ->
    (Path.T * Path.T) list -> Path.T -> string -> string * string -> bool -> unit
  val shutdown: unit -> unit
  val finish: unit -> unit
  val protocol_handler: string -> unit
  val init_protocol_handlers: unit -> unit
end;

structure Session: SESSION =
struct

(** persistent session information **)

val session = Synchronized.var "Session.session" ({chapter = "", name = ""}, true);

fun get_name () = #name (#1 (Synchronized.value session));

fun description () = "Isabelle/" ^ get_name ();

fun welcome () =
  if Distribution.is_identified then
    "Welcome to " ^ description () ^ " (" ^ Distribution.version ^ ")"
  else "Unofficial version of " ^ description () ^ " (" ^ Distribution.version ^ ")";


(* base syntax *)

val keywords = Synchronized.var "Session.keywords" Keyword.empty_keywords;

fun get_keywords () = Synchronized.value keywords;

fun update_keywords () =
  Synchronized.change keywords
    (K (fold (curry Keyword.merge_keywords o Thy_Header.get_keywords o Thy_Info.get_theory)
      (Thy_Info.get_names ()) Keyword.empty_keywords));


(* init *)

fun init symbols build info info_path doc doc_output doc_variants doc_files graph_file
    parent (chapter, name) verbose =
  (Synchronized.change session (fn ({name = parent_name, ...}, parent_finished) =>
    if parent_name <> parent orelse not parent_finished then
      error ("Unfinished parent session " ^ quote parent ^ " for " ^ quote name)
    else ({chapter = chapter, name = name}, false));
    Present.init symbols build info info_path (if doc = "false" then "" else doc)
      doc_output doc_variants doc_files graph_file (chapter, name) verbose);


(* finish *)

fun shutdown () =
 (Execution.shutdown ();
  Event_Timer.shutdown ();
  Future.shutdown ());

fun finish () =
 (shutdown ();
  Thy_Info.finish ();
  Present.finish ();
  shutdown ();
  update_keywords ();
  Synchronized.change session (apsnd (K true)));



(** protocol handlers **)

val protocol_handlers = Synchronized.var "protocol_handlers" ([]: string list);

fun protocol_handler name =
  if Thread_Data.is_virtual then ()
  else
    Synchronized.change protocol_handlers (fn handlers =>
     (Output.try_protocol_message (Markup.protocol_handler name) [];
      if not (member (op =) handlers name) then ()
      else warning ("Redefining protocol handler: " ^ quote name);
      update (op =) name handlers));

fun init_protocol_handlers () =
  if Thread_Data.is_virtual then ()
  else
    Synchronized.value protocol_handlers
    |> List.app (fn name => Output.try_protocol_message (Markup.protocol_handler name) []);

end;
