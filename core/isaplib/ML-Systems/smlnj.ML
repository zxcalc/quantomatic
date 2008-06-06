(*  Title:      Pure/ML-Systems/smlnj.ML
    ID:         $Id: smlnj.ML,v 1.56 2008/05/14 09:09:07 wenzelm Exp $

Compatibility file for Standard ML of New Jersey 110 or later.
*)

use "ML-Systems/proper_int.ML";
use "ML-Systems/overloading_smlnj.ML";
use "ML-Systems/exn.ML";
use "ML-Systems/universal.ML";
use "ML-Systems/multithreading.ML";
use "ML-Systems/system_shell.ML";


(*low-level pointer equality*)

CM.autoload "$smlnj/init/init.cmi";
val pointer_eq = InlineT.ptreql;


(* restore old-style character / string functions *)

val ord     = mk_int o SML90.ord;
val chr     = SML90.chr o dest_int;
val explode = SML90.explode;
val implode = SML90.implode;


(* New Jersey ML parameters *)

val _ =
 (Control.Print.printLength := 1000;
  Control.Print.printDepth := 350;
  Control.Print.stringDepth := 250;
  Control.Print.signatures := 2;
  Control.MC.matchRedundantError := false);


(* Poly/ML emulation *)

val exit = exit o dest_int;
fun quit () = exit 0;

(*limit the printing depth -- divided by 2 for comparibility with Poly/ML*)
local
  val depth = ref (10: int);
in
  fun get_print_depth () = ! depth;
  fun print_depth n =
   (depth := n;
    Control.Print.printDepth := dest_int n div 2;
    Control.Print.printLength := dest_int n);
end;


(* compiler-independent timing functions *)

fun start_timing () =
  let val CPUtimer = Timer.startCPUTimer();
      val time = Timer.checkCPUTimer(CPUtimer)
  in  (CPUtimer,time)  end;

fun end_timing (CPUtimer, {sys,usr}) =
  let open Time  (*...for Time.toString, Time.+ and Time.- *)
      val {sys=sys2,usr=usr2} = Timer.checkCPUTimer(CPUtimer)
  in  "User " ^ toString (usr2-usr) ^
      "  All "^ toString (sys2-sys + usr2-usr) ^
      " secs"
      handle Time => ""
  end;

fun check_timer timer =
  let
    val {sys, usr} = Timer.checkCPUTimer timer;
    val gc = Timer.checkGCTime timer;    (* FIXME already included in usr? *)
  in (sys, usr, gc) end;


(*prompts*)
fun ml_prompts p1 p2 =
  (Control.primaryPrompt := p1; Control.secondaryPrompt := p2);

(*dummy implementation*)
fun profile (n: int) f x = f x;

(*dummy implementation*)
fun exception_trace f = f ();

(*dummy implementation*)
fun print x = x;

(*dummy implementation*)
fun makestring x = "dummy string for SML New Jersey";


(* toplevel pretty printing (see also Pure/pure_setup.ML) *)

fun make_pp path pprint =
  let
    open PrettyPrint;

    fun pp pps obj =
      pprint obj
        (string pps, openHOVBox pps o Rel o dest_int,
          fn wd => break pps {nsp=dest_int wd, offset=0}, fn () => newline pps,
          fn () => closeBox pps);
  in (path, pp) end;

fun install_pp (path, pp) = CompilerPPTable.install_pp path pp;


(* ML command execution *)

fun use_text (tune: string -> string) _ (line: int, name) (print, err) verbose txt =
  let
    val ref out_orig = Control.Print.out;

    val out_buffer = ref ([]: string list);
    val out = {say = (fn s => out_buffer := s :: ! out_buffer), flush = (fn () => ())};
    fun output () =
      let val str = implode (rev (! out_buffer))
      in String.substring (str, 0, Int.max (0, size str - 1)) end;
  in
    Control.Print.out := out;
    Backend.Interact.useStream (TextIO.openString (tune txt)) handle exn =>
      (Control.Print.out := out_orig;
        err ((if name = "" then "" else "Error in " ^ name ^ "\n") ^ output ()); raise exn);
    Control.Print.out := out_orig;
    if verbose then print (output ()) else ()
  end;

fun use_file tune str_of_pos output verbose name =
  let
    val instream = TextIO.openIn name;
    val txt = Exn.release (Exn.capture TextIO.inputAll instream before TextIO.closeIn instream);
  in use_text tune str_of_pos (1, name) output verbose txt end;

fun forget_structure name =
  use_text (fn x => x) (fn _ => "") (1, "ML") (TextIO.print, fn s => raise Fail s) false
    ("structure " ^ name ^ " = struct end");



(** interrupts **)

exception Interrupt;

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
    val result = ref (Exn.Exn Interrupt: 'b Exn.result);
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


(* basis library fixes *)

structure TextIO =
struct
  open TextIO;
  fun inputLine is = TextIO.inputLine is
    handle IO.Io _ => raise Interrupt;
end;


(** Signal handling: emulation of the Poly/ML Signal structure. Note that types
    Posix.Signal.signal and Signals.signal differ **)

structure IsaSignal =
struct

datatype sig_handle = SIG_DFL | SIG_IGN | SIG_HANDLE of Signals.signal -> unit;

(*From the SML/NJ documentation: "HANDLER(f) installs a handler for a
  signal. When signal is delivered to the process, the execution state
  of the current thread will be bundled up as a continuation k, then
  f(signal,n,k) will be called. The number n is the number of times
  signal has been signalled since the last time f was invoked for it."*)

fun toAction SIG_DFL = Signals.DEFAULT
  | toAction SIG_IGN = Signals.IGNORE
  | toAction (SIG_HANDLE iu) =
      Signals.HANDLER (fn (signo,_,cont) => (iu signo; cont));

(*The types are correct, but I'm not sure about the semantics!*)
fun fromAction Signals.DEFAULT = SIG_DFL
  | fromAction Signals.IGNORE = SIG_IGN
  | fromAction (Signals.HANDLER f) =
      SIG_HANDLE (fn signo => SMLofNJ.Cont.callcc (fn k => (f (signo,0,k); ())));

(*Poly/ML version has type  int * sig_handle -> sig_handle*)
fun signal (signo, sh) = fromAction (Signals.setHandler (signo, toAction sh));

val usr1 = UnixSignals.sigUSR1
val usr2 = UnixSignals.sigUSR2
val alrm = UnixSignals.sigALRM
val chld = UnixSignals.sigCHLD
val cont = UnixSignals.sigCONT
val int = UnixSignals.sigINT
val quit = UnixSignals.sigQUIT
val term = UnixSignals.sigTERM

end;


(** OS related **)

(* current directory *)

val cd = OS.FileSys.chDir;
val pwd = OS.FileSys.getDir;


(* system command execution *)

val system_out = (fn (output, rc) => (output, mk_int rc)) o system_out;


(*Convert a process ID to a decimal string (chiefly for tracing)*)
fun string_of_pid pid =
  Word.fmt StringCvt.DEC (Word.fromLargeWord (Posix.Process.pidToWord pid));


(* getenv *)

fun getenv var =
  (case OS.Process.getEnv var of
    NONE => ""
  | SOME txt => txt);
