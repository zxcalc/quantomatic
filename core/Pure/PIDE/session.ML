(*  Title:      Pure/PIDE/session.ML
    Author:     Makarius

Prover session: persistent state of logic image.
*)

signature SESSION =
sig
  val name: unit -> string
  val welcome: unit -> string
  val init: bool -> bool -> Path.T -> string -> bool -> string -> (string * string) list ->
    (Path.T * Path.T) list -> string -> string * string -> bool -> unit
  val finish: unit -> unit
  val protocol_handler: string -> unit
  val init_protocol_handlers: unit -> unit
end;

structure Session: SESSION =
struct

(** session identification -- not thread-safe **)

val session = Unsynchronized.ref {chapter = "Pure", name = "Pure"};
val session_finished = Unsynchronized.ref false;

fun name () = "Isabelle/" ^ #name (! session);

fun welcome () =
  if Distribution.is_identified then
    "Welcome to " ^ name () ^ " (" ^ Distribution.version ^ ")"
  else "Unofficial version of " ^ name () ^ " (" ^ Distribution.version ^ ")";


(* init *)

fun init build info info_path doc doc_graph doc_output doc_variants doc_files
    parent (chapter, name) verbose =
  if #name (! session) <> parent orelse not (! session_finished) then
    error ("Unfinished parent session " ^ quote parent ^ " for " ^ quote name)
  else
    let
      val _ = session := {chapter = chapter, name = name};
      val _ = session_finished := false;
    in
      Present.init build info info_path (if doc = "false" then "" else doc)
        doc_graph doc_output doc_variants doc_files (chapter, name)
        verbose (map Thy_Info.get_theory (Thy_Info.get_names ()))
    end;


(* finish *)

fun finish () =
 (Execution.shutdown ();
  Thy_Info.finish ();
  Present.finish ();
  Outer_Syntax.check_syntax ();
  Future.shutdown ();
  Event_Timer.shutdown ();
  Future.shutdown ();
  session_finished := true);



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
