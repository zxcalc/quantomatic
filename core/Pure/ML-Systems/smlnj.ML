(*  Title:      Pure/ML-Systems/smlnj.ML

Compatibility file for Standard ML of New Jersey.
*)

val io_buffer_size = 4096;
use "ML-Systems/proper_int.ML";

exception Interrupt;
fun reraise exn = raise exn;

fun exit rc = Posix.Process.exit (Word8.fromInt rc);
fun quit () = exit 0;

use "ML-Systems/overloading_smlnj.ML";
use "General/exn.ML";
use "ML-Systems/single_assignment.ML";
use "ML-Systems/universal.ML";
use "ML-Systems/thread_dummy.ML";
use "ML-Systems/multithreading.ML";
use "ML-Systems/maximum_ml_stack_dummy.ML";
use "ML-Systems/ml_name_space.ML";
use "ML-Systems/ml_pretty.ML";
structure PolyML = struct end;
use "ML-Systems/pp_dummy.ML";
use "ML-Systems/use_context.ML";
use "ML-Systems/ml_positions.ML";


val seconds = Time.fromReal;

(*low-level pointer equality*)
CM.autoload "$smlnj/init/init.cmi";
val pointer_eq = InlineT.ptreql;


(* restore old-style character / string functions *)

val ord = mk_int o SML90.ord;
val chr = SML90.chr o dest_int;
val raw_explode = SML90.explode;
val implode = SML90.implode;


(* New Jersey ML parameters *)

val _ =
 (Control.Print.printLength := 1000;
  Control.Print.printDepth := 350;
  Control.Print.stringDepth := 250;
  Control.Print.signatures := 2;
  Control.MC.matchRedundantError := false);


(* Poly/ML emulation *)

(*limit the printing depth -- divided by 2 for comparibility with Poly/ML*)
local
  val depth = ref (10: int);
in
  fun get_default_print_depth () = ! depth;
  fun default_print_depth n =
   (depth := n;
    Control.Print.printDepth := dest_int n div 2;
    Control.Print.printLength := dest_int n);
  val _ = default_print_depth 10;
end;

fun ml_make_string (_: string) = "(fn _ => \"?\")";


(*prompts*)
fun ml_prompts p1 p2 =
  (Control.primaryPrompt := p1; Control.secondaryPrompt := p2);

(*dummy implementation*)
fun profile (n: int) f x = f x;

(*dummy implementation*)
fun print_exception_trace (_: exn -> string) (_: string -> unit) f = f ();


(* ML command execution *)

fun use_text ({tune_source, print, error, ...}: use_context) (line, name) verbose txt =
  let
    val ref out_orig = Control.Print.out;

    val out_buffer = ref ([]: string list);
    val out = {say = (fn s => out_buffer := s :: ! out_buffer), flush = (fn () => ())};
    fun output () =
      let val str = implode (rev (! out_buffer))
      in String.substring (str, 0, Int.max (0, size str - 1)) end;
  in
    Control.Print.out := out;
    Backend.Interact.useStream (TextIO.openString (tune_source (ml_positions line name txt)))
      handle exn =>
        (Control.Print.out := out_orig;
          error ((if name = "" then "" else "Error in " ^ name ^ "\n") ^ output ()); raise exn);
    Control.Print.out := out_orig;
    if verbose then print (output ()) else ()
  end;

fun use_file context verbose name =
  let
    val instream = TextIO.openIn name;
    val txt = Exn.release (Exn.capture TextIO.inputAll instream before TextIO.closeIn instream);
  in use_text context (1, name) verbose txt end;


(* toplevel pretty printing *)

fun ml_pprint pps =
  let
    fun str "" = ()
      | str s = PrettyPrint.string pps s;
    fun pprint (ML_Pretty.Block ((bg, en), prts, ind)) =
          (str bg; PrettyPrint.openHOVBox pps (PrettyPrint.Rel (dest_int ind));
            List.app pprint prts; PrettyPrint.closeBox pps; str en)
      | pprint (ML_Pretty.String (s, _)) = str s
      | pprint (ML_Pretty.Break (false, wd)) = PrettyPrint.break pps {nsp = dest_int wd, offset = 0}
      | pprint (ML_Pretty.Break (true, _)) = PrettyPrint.newline pps;
  in pprint end;

fun toplevel_pp context path pp =
  use_text context (1, "pp") false
    ("CompilerPPTable.install_pp [" ^ String.concatWith "," (map (fn s => "\"" ^ s ^ "\"")  path) ^
      "] (fn pps => ml_pprint pps o Pretty.to_ML o (" ^ pp ^ "))");



(** interrupts **)

local

fun change_signal new_handler f x =
  let
    val old_handler = Signals.setHandler (Signals.sigINT, new_handler);
    val result = Exn.capture (f old_handler) x;
    val _ = Signals.setHandler (Signals.sigINT, old_handler);
  in Exn.release result end;

in

fun interruptible (f: 'a -> 'b) x =
  let
    val result = ref (Exn.interrupt_exn: 'b Exn.result);
    val old_handler = Signals.inqHandler Signals.sigINT;
  in
    SMLofNJ.Cont.callcc (fn cont =>
      (Signals.setHandler (Signals.sigINT, Signals.HANDLER (fn _ => cont));
        result := Exn.capture f x));
    Signals.setHandler (Signals.sigINT, old_handler);
    Exn.release (! result)
  end;

fun uninterruptible f =
  change_signal Signals.IGNORE
    (fn old_handler => f (fn g => change_signal old_handler (fn _ => g)));

end;


use "ML-Systems/unsynchronized.ML";


(* ML system operations *)

use "ML-Systems/ml_system.ML";

structure ML_System =
struct

open ML_System;

fun save_state name =
  if SMLofNJ.exportML name then ()
  else OS.FileSys.rename {old = name ^ "." ^ platform, new = name};

end;

