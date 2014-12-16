(*  Title:      Pure/General/timing.ML
    Author:     Makarius

Basic support for time measurement.
*)

signature BASIC_TIMING =
sig
  val cond_timeit: bool -> string -> (unit -> 'a) -> 'a
  val timeit: (unit -> 'a) -> 'a
  val timeap: ('a -> 'b) -> 'a -> 'b
  val timeap_msg: string -> ('a -> 'b) -> 'a -> 'b
end

signature TIMING =
sig
  include BASIC_TIMING
  type timing = {elapsed: Time.time, cpu: Time.time, gc: Time.time}
  type start
  val start: unit -> start
  val result: start -> timing
  val timing: ('a -> 'b) -> 'a -> timing * 'b
  val is_relevant: timing -> bool
  val message: timing -> string
end

structure Timing: TIMING =
struct

(* timer control *)

type timing = {elapsed: Time.time, cpu: Time.time, gc: Time.time};

abstype start = Start of
  Timer.real_timer * Time.time * Timer.cpu_timer *
    {gc: {sys: Time.time, usr: Time.time}, nongc: {sys: Time.time, usr: Time.time}}
with

fun start () =
  let
    val real_timer = Timer.startRealTimer ();
    val real_time = Timer.checkRealTimer real_timer;
    val cpu_timer = Timer.startCPUTimer ();
    val cpu_times = Timer.checkCPUTimes cpu_timer;
  in Start (real_timer, real_time, cpu_timer, cpu_times) end;

fun result (Start (real_timer, real_time, cpu_timer, cpu_times)) =
  let
    val real_time2 = Timer.checkRealTimer real_timer;
    val {nongc = {sys, usr}, gc = {sys = gc_sys, usr = gc_usr}} = cpu_times;
    val {nongc = {sys = sys2, usr = usr2}, gc = {sys = gc_sys2, usr = gc_usr2}} =
      Timer.checkCPUTimes cpu_timer;

    open Time;
    val elapsed = real_time2 - real_time;
    val gc = gc_usr2 - gc_usr + gc_sys2 - gc_sys;
    val cpu = usr2 - usr + sys2 - sys + gc;
  in {elapsed = elapsed, cpu = cpu, gc = gc} end;

end;

fun timing f x =
  let
    val start = start ();
    val y = f x;
  in (result start, y) end;


(* timing messages *)

val min_time = Time.fromMilliseconds 1;

fun is_relevant {elapsed, cpu, gc} =
  Time.>= (elapsed, min_time) orelse
  Time.>= (cpu, min_time) orelse
  Time.>= (gc, min_time);

fun message {elapsed, cpu, gc} =
  Time.toString elapsed ^ "s elapsed time, " ^
  Time.toString cpu ^ "s cpu time, " ^
  Time.toString gc ^ "s GC time" handle Time.Time => "";

fun cond_timeit enabled msg e =
  if enabled then
    let
      val (timing, result) = timing (Exn.interruptible_capture e) ();
      val _ =
        if is_relevant timing then
          let val end_msg = message timing
          in warning (if msg = "" then end_msg else msg ^ "\n" ^ end_msg) end
        else ();
    in Exn.release result end
  else e ();

fun timeit e = cond_timeit true "" e;
fun timeap f x = timeit (fn () => f x);
fun timeap_msg msg f x = cond_timeit true msg (fn () => f x);

end;

structure Basic_Timing: BASIC_TIMING = Timing;
open Basic_Timing;

