(*  Title:      Pure/System/bash.ML
    Author:     Makarius

GNU bash processes, with propagation of interrupts -- POSIX version.
*)

signature BASH =
sig
  val string: string -> string
  val strings: string list -> string
  val process: string -> {out: string, err: string, rc: int, terminate: unit -> unit}
end;

if ML_System.platform_is_windows then ML
\<open>
structure Bash: BASH =
struct

val string = Bash_Syntax.string;
val strings = Bash_Syntax.strings;

val process = Thread_Attributes.uninterruptible (fn restore_attributes => fn script =>
  let
    datatype result = Wait | Signal | Result of int;
    val result = Synchronized.var "bash_result" Wait;

    val id = serial_string ();
    val script_path = File.tmp_path (Path.basic ("bash_script" ^ id));
    val out_path = File.tmp_path (Path.basic ("bash_out" ^ id));
    val err_path = File.tmp_path (Path.basic ("bash_err" ^ id));
    val pid_path = File.tmp_path (Path.basic ("bash_pid" ^ id));

    fun cleanup_files () =
     (try File.rm script_path;
      try File.rm out_path;
      try File.rm err_path;
      try File.rm pid_path);
    val _ = cleanup_files ();

    val system_thread =
      Standard_Thread.fork {name = "bash", stack_limit = NONE, interrupts = false} (fn () =>
        Thread_Attributes.with_attributes Thread_Attributes.private_interrupts (fn _ =>
          let
            val _ = File.write script_path script;
            val bash_script =
              "bash " ^ File.bash_path script_path ^
                " > " ^ File.bash_path out_path ^
                " 2> " ^ File.bash_path err_path;
            val bash_process = getenv_strict "ISABELLE_BASH_PROCESS";
            val rc =
              Windows.simpleExecute ("",
                quote (ML_System.platform_path bash_process) ^ " " ^
                quote (File.platform_path pid_path) ^  " \"\" bash -c " ^ quote bash_script)
              |> Windows.fromStatus |> SysWord.toInt;
            val res = if rc = 130 orelse rc = 512 then Signal else Result rc;
          in Synchronized.change result (K res) end
          handle exn =>
            (Synchronized.change result (fn Wait => Signal | res => res); Exn.reraise exn)));

    fun read_pid 0 = NONE
      | read_pid count =
          (case (Int.fromString (File.read pid_path) handle IO.Io _ => NONE) of
            NONE => (OS.Process.sleep (seconds 0.1); read_pid (count - 1))
          | some => some);

    fun terminate NONE = ()
      | terminate (SOME pid) =
          let
            fun kill s =
              let
                val cmd = getenv_strict "CYGWIN_ROOT" ^ "\\bin\\bash.exe";
                val arg = "kill -" ^ s ^ " -" ^ string_of_int pid;
              in
                OS.Process.isSuccess (Windows.simpleExecute ("", quote cmd ^ " -c " ^ quote arg))
                  handle OS.SysErr _ => false
              end;

            fun multi_kill count s =
              count = 0 orelse
                (kill s; kill "0") andalso
                (OS.Process.sleep (seconds 0.1); multi_kill (count - 1) s);
            val _ =
              multi_kill 10 "INT" andalso
              multi_kill 10 "TERM" andalso
              multi_kill 10 "KILL";
          in () end;

    fun cleanup () =
     (Standard_Thread.interrupt_unsynchronized system_thread;
      cleanup_files ());
  in
    let
      val _ =
        restore_attributes (fn () =>
          Synchronized.guarded_access result (fn Wait => NONE | x => SOME ((), x))) ();

      val out = the_default "" (try File.read out_path);
      val err = the_default "" (try File.read err_path);
      val rc = (case Synchronized.value result of Signal => Exn.interrupt () | Result rc => rc);
      val pid = read_pid 1;
      val _ = cleanup ();
    in {out = out, err = err, rc = rc, terminate = fn () => terminate pid} end
    handle exn => (terminate (read_pid 10); cleanup (); Exn.reraise exn)
  end);

end;
\<close>
else ML
\<open>
structure Bash: BASH =
struct

val string = Bash_Syntax.string;
val strings = Bash_Syntax.strings;

val process = Thread_Attributes.uninterruptible (fn restore_attributes => fn script =>
  let
    datatype result = Wait | Signal | Result of int;
    val result = Synchronized.var "bash_result" Wait;

    val id = serial_string ();
    val script_path = File.tmp_path (Path.basic ("bash_script" ^ id));
    val out_path = File.tmp_path (Path.basic ("bash_out" ^ id));
    val err_path = File.tmp_path (Path.basic ("bash_err" ^ id));
    val pid_path = File.tmp_path (Path.basic ("bash_pid" ^ id));

    fun cleanup_files () =
     (try File.rm script_path;
      try File.rm out_path;
      try File.rm err_path;
      try File.rm pid_path);
    val _ = cleanup_files ();

    val system_thread =
      Standard_Thread.fork {name = "bash", stack_limit = NONE, interrupts = false} (fn () =>
        Thread_Attributes.with_attributes Thread_Attributes.private_interrupts (fn _ =>
          let
            val _ = File.write script_path script;
            val _ = getenv_strict "ISABELLE_BASH_PROCESS";
            val status =
              OS.Process.system
                ("exec \"$ISABELLE_BASH_PROCESS\" " ^ File.bash_path pid_path ^ " \"\"" ^
                  " bash " ^ File.bash_path script_path ^
                  " > " ^ File.bash_path out_path ^
                  " 2> " ^ File.bash_path err_path);
            val res =
              (case Posix.Process.fromStatus status of
                Posix.Process.W_EXITED => Result 0
              | Posix.Process.W_EXITSTATUS 0wx82 => Signal
              | Posix.Process.W_EXITSTATUS w => Result (Word8.toInt w)
              | Posix.Process.W_SIGNALED s =>
                  if s = Posix.Signal.int then Signal
                  else Result (256 + LargeWord.toInt (Posix.Signal.toWord s))
              | Posix.Process.W_STOPPED s =>
                  Result (512 + LargeWord.toInt (Posix.Signal.toWord s)));
          in Synchronized.change result (K res) end
          handle exn =>
            (Synchronized.change result (fn Wait => Signal | res => res); Exn.reraise exn)));

    fun read_pid 0 = NONE
      | read_pid count =
          (case (Int.fromString (File.read pid_path) handle IO.Io _ => NONE) of
            NONE => (OS.Process.sleep (seconds 0.1); read_pid (count - 1))
          | some => some);

    fun terminate NONE = ()
      | terminate (SOME pid) =
          let
            fun kill s =
              (Posix.Process.kill
                (Posix.Process.K_GROUP (Posix.Process.wordToPid (LargeWord.fromInt pid)), s); true)
              handle OS.SysErr _ => false;

            fun multi_kill count s =
              count = 0 orelse
                (kill s; kill (Posix.Signal.fromWord 0w0)) andalso
                (OS.Process.sleep (seconds 0.1); multi_kill (count - 1) s);
            val _ =
              multi_kill 10 Posix.Signal.int andalso
              multi_kill 10 Posix.Signal.term andalso
              multi_kill 10 Posix.Signal.kill;
          in () end;

    fun cleanup () =
     (Standard_Thread.interrupt_unsynchronized system_thread;
      cleanup_files ());
  in
    let
      val _ =
        restore_attributes (fn () =>
          Synchronized.guarded_access result (fn Wait => NONE | x => SOME ((), x))) ();

      val out = the_default "" (try File.read out_path);
      val err = the_default "" (try File.read err_path);
      val rc = (case Synchronized.value result of Signal => Exn.interrupt () | Result rc => rc);
      val pid = read_pid 1;
      val _ = cleanup ();
    in {out = out, err = err, rc = rc, terminate = fn () => terminate pid} end
    handle exn => (terminate (read_pid 10); cleanup (); Exn.reraise exn)
  end);

end;
\<close>