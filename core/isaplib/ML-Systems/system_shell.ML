(*  Title:      Pure/ML-Systems/system_shell.ML
    ID:         $Id: system_shell.ML,v 1.2 2008/03/31 21:08:56 wenzelm Exp $
    Author:     Makarius

Generic system shell processes (no provisions to propagate interrupts;
might still work via the controlling tty).
*)

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
    val rc =
      (case Posix.Process.fromStatus status of
        Posix.Process.W_EXITED => 0
      | Posix.Process.W_EXITSTATUS w => Word8.toInt w
      | Posix.Process.W_SIGNALED s => 256 + LargeWord.toInt (Posix.Signal.toWord s)
      | Posix.Process.W_STOPPED s => 512 + LargeWord.toInt (Posix.Signal.toWord s));

    val output = read_file output_name handle IO.Io _ => "";
    val _ = OS.FileSys.remove script_name handle OS.SysErr _ => ();
    val _ = OS.FileSys.remove output_name handle OS.SysErr _ => ();
  in (output, rc) end;

end;

