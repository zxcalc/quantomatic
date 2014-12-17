(*  Title:      Pure/General/secure.ML
    Author:     Makarius

Secure critical operations.
*)

signature SECURE =
sig
  val set_secure: unit -> unit
  val is_secure: unit -> bool
  val deny_secure: string -> unit
  val secure_mltext: unit -> unit
  val use_text: use_context -> int * string -> bool -> string -> unit
  val use_file: use_context -> bool -> string -> unit
  val toplevel_pp: string list -> string -> unit
  val PG_setup: unit -> unit
  val commit: unit -> unit
end;

structure Secure: SECURE =
struct

(** secure flag **)

val secure = Unsynchronized.ref false;

fun set_secure () = secure := true;
fun is_secure () = ! secure;

fun deny_secure msg = if is_secure () then error msg else ();



(** critical operations **)

(* ML evaluation *)

fun secure_mltext () = deny_secure "Cannot evaluate ML source in secure mode";

val raw_use_text = use_text;
val raw_use_file = use_file;
val raw_toplevel_pp = toplevel_pp;

fun use_text context pos verbose txt = (secure_mltext (); raw_use_text context pos verbose txt);
fun use_file context verbose name = (secure_mltext (); raw_use_file context verbose name);

fun toplevel_pp path pp = (secure_mltext (); raw_toplevel_pp ML_Parse.global_context path pp);


(* global evaluation *)

val use_global = raw_use_text ML_Parse.global_context (0, "") false;

fun commit () = use_global "commit();";   (*commit is dynamically bound!*)

fun PG_setup () =
  use_global "val change = Unsynchronized.change; structure ThyLoad = ProofGeneral.ThyLoad;";

end;

