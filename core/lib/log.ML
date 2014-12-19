signature LOG = 
sig
  (* current level of logging *)
  val level_ref : int Unsynchronized.ref;
  val stream_ref : TextIO.outstream Unsynchronized.ref;

  val set_log_to_stdout : unit -> unit;
  val set_log_to_stderr : unit -> unit;

  val writeln_to_stream : TextIO.outstream -> string -> unit;
  val writeln_to_stdout : string -> unit;
  val writeln_to_stderr : string -> unit;

  (* deferred logging; the function will never be called
   * if the log level is not high enough *)
  val logf : int -> ('a -> string) -> 'a -> unit;

  (* log if log-level is above the int. *)
  val log : int -> string -> unit;
  (* forced to stdout *)
  val log_stdout : int -> string -> unit;
  (* forced to stderr *)
  val log_stderr : int -> string -> unit;

  val print : 'a -> unit;
end;

(* handy little library for writing logs specific to a bit of code. *)
functor Log(val init_level : int)
: LOG
= struct
  val level_ref = Unsynchronized.ref init_level;
  val stream_ref = Unsynchronized.ref TextIO.stdErr;

  fun writeln_to_stream stream s =
      (TextIO.output (stream, s ^ "\n"); TextIO.flushOut stream);
  fun writeln_to_stdout s = writeln_to_stream TextIO.stdOut s;
  fun writeln_to_stderr s = writeln_to_stream TextIO.stdErr s;

  fun set_log_to_stdout () = (stream_ref := TextIO.stdOut);
  fun set_log_to_stderr () = (stream_ref := TextIO.stdErr);

  fun logf level f x =
      if level <= (!level_ref) then writeln_to_stream (!stream_ref) (f x)
      else ();

  fun log level s =
      if level <= (!level_ref) then writeln_to_stream (!stream_ref) s
      else ();
  fun log_stdout level s =
      if level <= (!level_ref) then writeln_to_stdout s else ();
  fun log_stderr level s =
      if level <= (!level_ref) then writeln_to_stderr s else ();

  fun print x = (PolyML.print x; ());
end;

