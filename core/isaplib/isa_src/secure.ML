(*  Title:      Pure/General/secure.ML
    ID:         $Id: secure.ML,v 1.23 2008/05/14 09:05:11 wenzelm Exp $
    Author:     Makarius

Secure critical operations.
*)

signature SECURE =
sig
  val set_secure: unit -> unit
  val is_secure: unit -> bool
  val deny_secure: string -> unit
  val use_text: int * string -> (string -> unit) * (string -> 'a) -> bool -> string -> unit
  val use_file: (string -> unit) * (string -> 'a) -> bool -> string -> unit
  val use: string -> unit
  val commit: unit -> unit
  val system_out: string -> string * int
  val system: string -> int
end;

structure Secure: SECURE =
struct

(** secure flag **)

val secure = ref false;

fun set_secure () = secure := true;
fun is_secure () = ! secure;

fun deny_secure msg = if is_secure () then error msg else ();



(** critical operations **)

(* ML evaluation *)

fun secure_mltext () = deny_secure "Cannot evaluate ML source in secure mode";

fun raw_use_text x = use_text ML_Parse.fix_ints (Position.str_of oo Position.line_file) x;
fun raw_use_file x = use_file ML_Parse.fix_ints (Position.str_of oo Position.line_file) x;

fun use_text pos pr verbose txt =
  (secure_mltext (); raw_use_text pos pr verbose txt);

fun use_file pr verbose name =
  (secure_mltext (); raw_use_file pr verbose name);

fun use name = use_file Output.ml_output true name;

(*commit is dynamically bound!*)
fun commit () = raw_use_text (0, "") Output.ml_output false "commit();";


(* shell commands *)

fun secure_shell () = deny_secure "Cannot execute shell commands in secure mode";

val orig_system_out = system_out;

fun system_out s = (secure_shell (); orig_system_out s);

fun system s =
  (case system_out s of
    ("", rc) => rc
  | (out, rc) => (writeln (perhaps (try (unsuffix "\n")) out); rc));

end;

(*override previous toplevel bindings!*)
val use_text = Secure.use_text;
val use_file = Secure.use_file;
fun use s = Secure.use s handle ERROR msg => (writeln msg; raise Fail "ML error");
val system_out = Secure.system_out;
val system = Secure.system;
