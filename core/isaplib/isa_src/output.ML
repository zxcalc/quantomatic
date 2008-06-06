(*  Title:      Pure/General/output.ML
    ID:         $Id: output.ML,v 1.48 2008/03/15 21:07:32 wenzelm Exp $
    Author:     Makarius, Hagia Maria Sion Abbey (Jerusalem)

Output channels and timing messages.
*)

signature BASIC_OUTPUT =
sig
  type output = string
  val writeln: string -> unit
  val priority: string -> unit
  val tracing: string -> unit
  val warning: string -> unit
  val tolerate_legacy_features: bool ref
  val legacy_feature: string -> unit
  val cond_timeit: bool -> string -> (unit -> 'a) -> 'a
  val timeit: (unit -> 'a) -> 'a
  val timeap: ('a -> 'b) -> 'a -> 'b
  val timeap_msg: string -> ('a -> 'b) -> 'a -> 'b
  val timing: bool ref
  val time_accumulator: string -> ('a -> 'b) -> 'a -> 'b
end;

signature OUTPUT =
sig
  include BASIC_OUTPUT
  val default_output: string -> output * int
  val default_escape: output -> string
  val add_mode: string -> (string -> output * int) -> (output -> string) -> unit
  val output_width: string -> output * int
  val output: string -> output
  val escape: output -> string
  val std_output: output -> unit
  val std_error: output -> unit
  val immediate_output: string -> unit
  val writeln_default: output -> unit
  val writeln_fn: (output -> unit) ref
  val priority_fn: (output -> unit) ref
  val tracing_fn: (output -> unit) ref
  val warning_fn: (output -> unit) ref
  val error_fn: (output -> unit) ref
  val debug_fn: (output -> unit) ref
  val prompt_fn: (output -> unit) ref
  val error_msg: string -> unit
  val prompt: string -> unit
  val debugging: bool ref
  val no_warnings: ('a -> 'b) -> 'a -> 'b
  val debug: (unit -> string) -> unit
  val ml_output: (string -> unit) * (string -> 'a)
  val accumulated_time: unit -> unit
end;

structure Output: OUTPUT =
struct

(** print modes **)

type output = string;  (*raw system output*)

fun default_output s = (s, size s);
fun default_escape (s: output) = s;

local
  val default = {output = default_output, escape = default_escape};
  val modes = ref (Symtab.make [("", default)]);
in
  fun add_mode name output escape = CRITICAL (fn () =>
    change modes (Symtab.update_new (name, {output = output, escape = escape})));
  fun get_mode () =
    the_default default (Library.get_first (Symtab.lookup (! modes)) (print_mode_value ()));
end;

fun output_width x = #output (get_mode ()) x;
val output = #1 o output_width;

fun escape x = #escape (get_mode ()) x;



(** output channels **)

(* output primitives -- normally NOT used directly!*)

fun std_output s = NAMED_CRITICAL "IO" (fn () =>
  (TextIO.output (TextIO.stdOut, s); TextIO.flushOut TextIO.stdOut));

fun std_error s = NAMED_CRITICAL "IO" (fn () =>
  (TextIO.output (TextIO.stdErr, s); TextIO.flushOut TextIO.stdErr));

val immediate_output = std_output o output;
val writeln_default = std_output o suffix "\n";


(* Isabelle output channels *)

val writeln_fn = ref writeln_default;
val priority_fn = ref (fn s => ! writeln_fn s);
val tracing_fn = ref (fn s => ! writeln_fn s);
val warning_fn = ref (std_output o suffix "\n" o prefix_lines "### ");
val error_fn = ref (std_output o suffix "\n" o prefix_lines "*** ");
val debug_fn = ref (std_output o suffix "\n" o prefix_lines "::: ");
val prompt_fn = ref std_output;

fun writeln s = ! writeln_fn (output s);
fun priority s = ! priority_fn (output s);
fun tracing s = ! tracing_fn (output s);
fun warning s = ! warning_fn (output s);
fun error_msg s = ! error_fn (output s);
fun prompt s = ! prompt_fn (output s);

val tolerate_legacy_features = ref true;
fun legacy_feature s =
  (if ! tolerate_legacy_features then warning else error) ("Legacy feature! " ^ s);

fun no_warnings f = setmp warning_fn (K ()) f;

val debugging = ref false;
fun debug s = if ! debugging then ! debug_fn (output (s ())) else ()

val ml_output = (writeln, error);



(** timing **)

(*conditional timing with message*)
fun cond_timeit flag msg e =
  if flag then
    let
      val start = start_timing ();
      val result = Exn.capture e ();
      val end_msg = end_timing start;
      val _ = warning (if msg = "" then end_msg else msg ^ "\n" ^ end_msg);
    in Exn.release result end
  else e ();

(*unconditional timing*)
fun timeit e = cond_timeit true "" e;

(*timed application function*)
fun timeap f x = timeit (fn () => f x);
fun timeap_msg msg f x = cond_timeit true msg (fn () => f x);


(*global timing mode*)
val timing = ref false;


(* accumulated timing *)

local

datatype time_info = TI of
  {name: string,
   timer: Timer.cpu_timer,
   sys: Time.time,
   usr: Time.time,
   gc: Time.time,
   count: int};

fun time_init name = ref (TI
 {name = name,
  timer = Timer.startCPUTimer (),
  sys = Time.zeroTime,
  usr = Time.zeroTime,
  gc = Time.zeroTime,
  count = 0});

fun time_reset (r as ref (TI {name, ...})) = r := ! (time_init name);

fun time_check (ref (TI r)) = r;

fun time_add ti f x =
  let
    fun add_diff time time1 time2 =
      Time.+ (time, Time.- (time2, time1) handle Time.Time => Time.zeroTime);
    val {name, timer, sys, usr, gc, count} = time_check ti;
    val (sys1, usr1, gc1) = check_timer timer;
    val result = Exn.capture f x;
    val (sys2, usr2, gc2) = check_timer timer;
  in
    ti := TI
     {name = name,
      timer = timer,
      sys = add_diff sys sys1 sys2,
      usr = add_diff usr usr1 usr2,
      gc = add_diff gc gc1 gc2,
      count = count + 1};
    Exn.release result
  end;

fun time_finish ti =
  let
    fun secs prfx time = prfx ^ Time.toString time;
    val {name, timer, sys, usr, gc, count} = time_check ti;
  in
    warning ("Total of " ^ quote name ^ ": " ^
      secs "User " usr ^ secs "  GC " gc ^ secs "  All " (Time.+ (sys, Time.+ (usr, gc))) ^
      " secs in " ^ string_of_int count ^ " calls");
    time_reset ti
  end;

val time_finish_hooks = ref ([]: (unit -> unit) list);

in

fun time_accumulator name =
  let val ti = time_init name in
    CRITICAL (fn () => change time_finish_hooks (cons (fn () => time_finish ti)));
    time_add ti
  end;

fun accumulated_time () = List.app (fn f => f ()) (! time_finish_hooks);

end;

end;

structure BasicOutput: BASIC_OUTPUT = Output;
open BasicOutput;
