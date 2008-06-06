(*  Title:      Pure/ML-Systems/poplogml.ML
    ID:         $Id: poplogml.ML,v 1.17 2008/05/14 09:05:46 wenzelm Exp $
    Author:     Makarius

Compatibility file for Poplog/PML (version 15.6/2.1).
*)

(* Compiler and runtime options *)

val ml_system_fix_ints = false;

val _ = Compile.filetype := ".ML";
val _ = Memory.hilim := let fun MB n = n div 4 * 1024 * 1024 in MB 120 end;
val _ = Memory.stacklim := 10 * ! Memory.stacklim;

fun pointer_eq (_: 'a, _: 'a) = false;


(* ML toplevel *)

fun ml_prompts p1 p2 = ();

fun make_pp path pprint = ();
fun install_pp _ = ();

fun get_print_depth () = 10;
fun print_depth _ = ();

fun exception_trace f = f ();
fun profile (n: int) f x = f x;



(** Basis structures **)

structure General =
struct
  exception Subscript = Array.Subscript;
  exception Size;
  exception Fail of string;
  fun exnMessage (exn: exn) = "exception " ^ makestring exn ^ " raised";
  datatype order = LESS | EQUAL | GREATER;
end;
open General;

structure Bool =
struct
  val toString: bool -> string = makestring;
end;

structure Option =
struct
  open Option;
  exception Option;

  fun valOf (SOME x) = x
    | valOf NONE = raise Option;

  fun getOpt (SOME x, _) = x
    | getOpt (NONE, x) = x;

  fun isSome (SOME _) = true
    | isSome NONE = false;
end;
open Option;

structure Option =
struct
  open Option;
  fun map f (SOME x) = SOME (f x)
    | map _ NONE = NONE;
end;

structure Int =
struct
  open Int;
  type int = int;
  val toString: int -> string = makestring;
  fun fromInt (i: int) = i;
  val fromString = String.stringint;
  val op + = op + : int * int -> int;
  val op - = op - : int * int -> int;
  val op * = op * : int * int -> int;
  val op div = op div : int * int -> int;
  val op mod = op mod : int * int -> int;
  fun pow (x, y) = power x y : int;
  val op < = op < : int * int -> bool;
  val op > = op > : int * int -> bool;
  val op <= = op <= : int * int -> bool;
  val op >= = op >= : int * int -> bool;
  val abs = abs: int -> int;
  val sign = sign: int -> int;
  fun max (x, y) : int = if x < y then y else x;
  fun min (x, y) : int = if x < y then x else y;
  fun compare (x: int, y) = if x = y then EQUAL else if x < y then LESS else GREATER;
end;

structure IntInf = Int;

structure Real =
struct
  open Real;
  val toString: real -> string = makestring;
  fun max (x, y) : real = if x < y then y else x;
  fun min (x, y) : real = if x < y then x else y;
  val real = real;
  val floor = floor;
  val realFloor = real o floor;
  fun ceil x = ~1 - floor (~ (x + 1.0));
  fun round x = floor (x + 0.5);  (*does not do round-to-nearest*)
  fun trunc x = if x < 0.0 then ceil x else floor x;
end;

structure String =
struct
  open String;
  type char = string
  fun str (c: char) = c: string;
  val translate = mapstring;
  val isPrefix = isprefix;
  val isSuffix = issuffix;
  val size = size;
  fun compare (x: string, y) = if x = y then EQUAL else if x < y then LESS else GREATER;
  fun substring (s, i, n) = String.substring i (i + n) s
    handle String.Substring => raise Subscript;
end;

structure List =
struct
  open List;

  exception Empty;
  fun null [] = true | null _ = false;
  fun hd (x :: _) = x | hd _ = raise Empty;
  fun tl (_ :: xs) = xs | tl _ = raise Empty;
  val map = map;

  fun foldl f b [] = b
    | foldl f b (x :: xs) = foldl f (f (x, b)) xs;

  fun foldr f b [] = b
    | foldr f b (x :: xs) = f (x, foldr f b xs);

  fun last []      = raise Empty
    | last [x]     = x
    | last (x::xs) = last xs;

  fun nth (xs, n) =
      let fun h []      _ = raise Subscript
            | h (x::xs) n = if n=0 then x else h xs (n-1)
      in if n<0 then raise Subscript else h xs n end;

  fun drop (xs, n) =
      let fun h xs      0 = xs
            | h []      n = raise Subscript
            | h (x::xs) n = h xs (n-1)
      in if n<0 then raise Subscript else h xs n end;

  fun take (xs, n) =
      let fun h xs      0 = []
            | h []      n = raise Subscript
            | h (x::xs) n = x :: h xs (n-1)
      in if n<0 then raise Subscript else h xs n end;

  fun concat []      = []
    | concat (l::ls) = l @ concat ls;

  fun mapPartial f []      = []
    | mapPartial f (x::xs) =
         (case f x of NONE   => mapPartial f xs
                    | SOME y => y :: mapPartial f xs);

  fun find _ []        = NONE
    | find p (x :: xs) = if p x then SOME x else find p xs;


  (*copy the list preserving elements that satisfy the predicate*)
  fun filter p [] = []
    | filter p (x :: xs) = if p x then x :: filter p xs else filter p xs;

  (*Partition list into elements that satisfy predicate and those that don't.
    Preserves order of elements in both lists.*)
  fun partition (p: 'a->bool) (ys: 'a list) : ('a list * 'a list) =
      let fun part ([], answer) = answer
            | part (x::xs, (ys, ns)) = if p(x)
              then  part (xs, (x::ys, ns))
              else  part (xs, (ys, x::ns))
      in  part (rev ys, ([], []))  end;

end;
exception Empty = List.Empty;
val null = List.null;
val hd = List.hd;
val tl = List.tl;
val map = List.map;
val foldl = List.foldl;
val foldr = List.foldr;
val app = List.app;
val length = List.length;

structure ListPair =
struct
  fun zip ([], [])      = []
    | zip (x::xs,y::ys) = (x,y) :: zip(xs,ys);

  fun unzip [] = ([],[])
    | unzip((x,y)::pairs) =
          let val (xs,ys) = unzip pairs
          in  (x::xs, y::ys)  end;

  fun app f (x::xs, y::ys) = (f (x, y); app f (xs, ys))
    | app f _ = ();

  fun map f ([], [])      = []
    | map f (x::xs,y::ys) = f(x,y) :: map f (xs,ys);

  fun exists p =
    let fun boolf ([], [])      = false
          | boolf (x::xs,y::ys) = p(x,y) orelse boolf (xs,ys)
    in boolf end;

  fun all p =
    let fun boolf ([], [])      = true
          | boolf (x::xs,y::ys) = p(x,y) andalso boolf (xs,ys)
    in boolf end;
end;

structure TextIO =
struct
  type instream = instream;
  type outstream = outstream;
  exception Io = Io;
  val stdIn = std_in;
  val stdOut = std_out;
  val stdErr = std_err;
  val openIn = open_in;
  val openAppend = open_append;
  val openOut = open_out;
  val closeIn = close_in;
  val closeOut = close_out;
  val inputN = input;
  val inputAll = fn is => inputN (is, 1000000000);
  val inputLine = input_line;
  val endOfStream = end_of_stream;
  val output = output;
  val flushOut = flush_out;
end;



(** Compiler-independent timing functions **)

fun start_timing() = "FIXME";
fun end_timing (s: string) = s;
fun check_timer _ = (0, 0, 0);

structure Timer =
struct
  type cpu_timer = int;
  fun startCPUTimer () = 0;
end;

structure Time =
struct
  exception Time;
  type time = int;
  val toString: time -> string = makestring;
  val zeroTime = 0;
  fun now () = 0;
  fun (x: int) + y = x + y;
  fun (x: int) - y = x - y;
end;



(** interrupts **)      (*Note: may get into race conditions*)

fun interruptible f x = f x;
fun uninterruptible f x = f (fn (g: 'c -> 'd) => g) x;



(** OS related **)

val tmp_name =
  let val count = ref 0 in
    fn () => (count := ! count + 1;
      "/tmp/pml" ^ Int.toString (OS.pid ()) ^ "." ^ Int.toString (! count))
  end;

local

fun shell cmdline = OS.execve "/bin/sh" ["sh", "-c", cmdline] (OS.envlist());

fun execute_rc cmdline =
  let
    fun wait pid =
      (case OS.wait () of
        NONE => wait pid
      | SOME (pid', rc) =>
          if pid = pid' then rc div 256 else raise OS.Error ("wait", "wrong pid", ""));
  in
    (case OS.fork () of
      NONE => shell cmdline
    | SOME pid => wait pid)
  end;

fun squote s = "'" ^ s ^ "'";
fun remove name = (execute_rc ("/bin/rm -- " ^ squote name); ());
fun is_dir name = execute_rc ("perl -e \"exit (-d " ^ squote name ^ " ? 0 : 1)\"") = 0;

fun execute_result cmdline =
  let
    val tmp = tmp_name ();
    val rc = execute_rc (cmdline  ^ " > " ^ tmp);
    val instream = TextIO.openIn tmp;
    val result = TextIO.inputAll instream;
    val _ = TextIO.closeIn instream;
    val _ = remove tmp;
  in (rc, result) end;

in

fun exit rc = shell ("exit " ^ Int.toString rc);
fun quit () = exit 0;

fun execute cmdline = #2 (execute_result cmdline);

fun system cmdline =
  let val (rc, result) = execute_result cmdline
  in TextIO.output (TextIO.stdOut, result); TextIO.flushOut TextIO.stdOut; rc end;

val string_of_pid: int -> string = makestring;

fun getenv var = (case OS.translate var of NONE => "" | SOME s => s);

structure OS =
struct
  structure FileSys =
  struct
    val tmpName = tmp_name;
    val chDir = OS.cd;
    val getDir = OS.pwd;
    val remove = remove;
    val isDir = is_dir;
    val compare = Int.compare;

    fun modTime name =
      (case execute ("perl -e '@_ = stat(q:" ^ name ^ ":); print $_[9]'") of
        "" => raise TextIO.Io "OS.FileSys.modTime"
      | s => Int.fromString s);
    fun fileId name =
      (case execute ("perl -e '@_ = stat(q:" ^ name ^ ":); print $_[1]'") of
        "" => raise TextIO.Io "OS.FileSys.fileId"
      | s => Int.fromString s);
  end;
end;

end;


(* use *)

local val pml_use = use in

fun use name =
  if name = "ROOT.ML" then   (*workaround error about looping compilations*)
    let
      val instream = TextIO.openIn name;
      val txt = TextIO.inputAll instream;
      val _ = TextIO.closeIn;
    in use_string txt end
  else pml_use name;

end;

fun use_text _ _ _ _ txt = use_string txt;
fun use_file _ _ _ name = use name;
