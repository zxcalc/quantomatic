(*  Title:      Pure/ML-Systems/alice.ML
    ID:         $Id: alice.ML,v 1.24 2008/05/28 20:13:31 wenzelm Exp $

Compatibility file for Alice 1.4.

NOTE: there is no wrapper script; may run it interactively as follows:

$ cd Isabelle/src/Pure
$ env ALICE_JIT_MODE=0 ISABELLE_HOME=$(cd ../..; pwd) alice
- val ml_system = "alice";
- use "ML-Systems/exn.ML";
- use "ML-Systems/universal.ML";
- use "ML-Systems/multithreading.ML";
- use "ML-Systems/time_limit.ML";
- use "ML-Systems/alice.ML";
- use "ROOT.ML";
- Session.finish ();
*)

val ml_system_fix_ints = false;

fun forget_structure _ = ();

fun exit 0 = (OS.Process.exit OS.Process.success): unit
  | exit _ = OS.Process.exit OS.Process.failure;


(** ML system related **)

(*low-level pointer equality*)
fun pointer_eq (_: 'a, _: 'a) = false;


(* integer compatibility -- downgraded IntInf *)

structure Time =
struct
  open Time;
  val fromMilliseconds = Time.fromMilliseconds o IntInf.fromInt;
  val fromSeconds = Time.fromSeconds o IntInf.fromInt;
end;

structure IntInf =
struct
  fun divMod (x, y) = (x div y, x mod y);
  open Int;
end;


(* restore old-style character / string functions *)

exception Ord;
fun ord "" = raise Ord
  | ord s = Char.ord (String.sub (s, 0));

val chr = String.str o chr;
val explode = map String.str o String.explode;
val implode = String.concat;


(* Poly/ML emulation *)

fun quit () = exit 0;

fun get_print_depth () = ! Print.depth;
fun print_depth n = Print.depth := n;


(* compiler-independent timing functions *)

structure Timer =
struct
  open Timer;
  type cpu_timer = unit;
  fun startCPUTimer () = ();
  fun checkCPUTimer () = {sys = Time.zeroTime, usr = Time.zeroTime};
  fun checkGCTime () = Time.zeroTime;
end;

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
fun ml_prompts p1 p2 = ();

(*dummy implementation*)
fun profile (n: int) f x = f x;

(*dummy implementation*)
fun exception_trace f = f ();

(*dummy implementation*)
fun print x = x;


(* toplevel pretty printing (see also Pure/pure_setup.ML) *)

fun make_pp path pprint = (path, pprint);
fun install_pp (path, pp) = ();


(* ML command execution *)

fun use_text _ _ _ _ _ txt = (Compiler.eval txt; ());
fun use_file _ _ _ _ name = use name;



(** interrupts **)

exception Interrupt;

fun interruptible f x = f x;
fun uninterruptible f x = f (fn (g: 'c -> 'd) => g) x;


(* basis library fixes *)

structure TextIO =
struct
  open TextIO;
  fun inputLine is = TextIO.inputLine is
    handle IO.Io _ => raise Interrupt;
end;



(** OS related **)

structure OS =
struct
  open OS;
  structure FileSys =
  struct
    open FileSys;
    fun tmpName () =
      let val name = FileSys.tmpName () in
        if String.isSuffix "\000" name
        then String.substring (name, 0, size name - 1)
        else name
      end;
  end;
end;

val cd = OS.FileSys.chDir;
val pwd = OS.FileSys.getDir;

local

fun read_file name =
  let val is = TextIO.openIn name
  in Exn.release (Exn.capture TextIO.inputAll is before TextIO.closeIn is) end;

fun write_file name txt =
  let val os = TextIO.openOut name
  in Exn.release (Exn.capture TextIO.output (os, txt) before TextIO.closeOut os) end;

in

fun system_out script =
  let
    val script_name = OS.FileSys.tmpName ();
    val _ = write_file script_name script;

    val output_name = OS.FileSys.tmpName ();

    val status =
      OS.Process.system ("perl -w \"$ISABELLE_HOME/lib/scripts/system.pl\" nogroup " ^
        script_name ^ " /dev/null " ^ output_name);
    val rc = if OS.Process.isSuccess status then 0 else 1;

    val output = read_file output_name handle IO.Io _ => "";
    val _ = OS.FileSys.remove script_name handle OS.SysErr _ => ();
    val _ = OS.FileSys.remove output_name handle OS.SysErr _ => ();
  in (output, rc) end;

end;

structure OS =
struct
  open OS;
  structure FileSys =
  struct
    fun fileId name =
      (case system_out ("perl -e '@_ = stat(q:" ^ name ^ ":); print $_[1]'") of
        ("", _) => raise Fail "OS.FileSys.fileId"   (* FIXME IO.Io!? *)
      | (s, _) => (case Int.fromString s of NONE => raise Fail "OS.FileSys.fileId" | SOME i => i));
    val compare = Int.compare;
    fun fullPath name =
      (case system_out ("FILE='" ^ name ^
        "' && cd \"$(dirname \"$FILE\")\" && echo -n \"$(pwd -P)/$(basename \"$FILE\")\"") of
        ("", _) => raise SysErr ("Bad file", NONE)
      | (s, _) => s);
    open FileSys;
  end;
end;

structure Posix =
struct
  structure ProcEnv =
  struct
    fun getpid () = raise Fail "Posix.ProcEnv.getpoid undefined";
  end;
end;

fun string_of_pid _ = raise Fail "string_of_pid undefined";

fun getenv var =
  (case OS.Process.getEnv var of
    NONE => ""
  | SOME txt => txt);
