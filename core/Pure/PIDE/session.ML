(*  Title:      Pure/PIDE/session.ML
    Author:     Makarius

Prover session: persistent state of logic image.
*)

signature SESSION =
sig
  val get_name: unit -> string
  val welcome: unit -> string
  val get_keywords: unit -> Keyword.keywords
  val init: bool -> bool -> Path.T -> string -> string -> (string * string) list ->
    (Path.T * Path.T) list -> Path.T -> string -> string * string -> bool -> unit
  val shutdown: unit -> unit
  val finish: unit -> unit
  val save: string -> unit
  val protocol_handler: string -> unit
  val init_protocol_handlers: unit -> unit
end;

structure Session: SESSION =
struct

(** session identification -- not thread-safe **)

val session = Unsynchronized.ref {chapter = "Pure", name = "Pure"};
val session_finished = Unsynchronized.ref false;

fun get_name () = #name (! session);

fun description () = "Isabelle/" ^ get_name ();

fun welcome () =
  if Distribution.is_identified then
    "Welcome to " ^ description () ^ " (" ^ Distribution.version ^ ")"
  else "Unofficial version of " ^ description () ^ " (" ^ Distribution.version ^ ")";


(* base syntax *)

val keywords = Unsynchronized.ref Keyword.empty_keywords;
fun get_keywords () = ! keywords;


(* init *)

fun init build info info_path doc doc_output doc_variants doc_files graph_file
    parent (chapter, name) verbose =
  if get_name () <> parent orelse not (! session_finished) then
    error ("Unfinished parent session " ^ quote parent ^ " for " ^ quote name)
  else
    let
      val _ = session := {chapter = chapter, name = name};
      val _ = session_finished := false;
    in
      Present.init build info info_path (if doc = "false" then "" else doc)
        doc_output doc_variants doc_files graph_file (chapter, name) verbose
    end;


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
  keywords :=
    fold (curry Keyword.merge_keywords o Thy_Header.get_keywords o Thy_Info.get_theory)
      (Thy_Info.get_names ()) Keyword.empty_keywords;
  session_finished := true);

fun save heap =
 (shutdown ();
  ML_System.share_common_data ();
  ML_System.save_state heap);



(** protocol handlers **)

val protocol_handlers = Synchronized.var "protocol_handlers" ([]: string list);

fun protocol_handler name =
  Synchronized.change protocol_handlers (fn handlers =>
   (Output.try_protocol_message (Markup.protocol_handler name) [];
    if not (member (op =) handlers name) then ()
    else warning ("Redefining protocol handler: " ^ quote name);
    update (op =) name handlers));

fun init_protocol_handlers () =
  Synchronized.value protocol_handlers
  |> List.app (fn name => Output.try_protocol_message (Markup.protocol_handler name) []);

end;
