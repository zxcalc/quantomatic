
(* Call out to the computer algebra system Maxima *)

signature MAXIMA_PROCESS =
sig
type T
exception Maxima of string
val flush : T -> T
val new : unit -> T
val kill : T -> unit
val eval : T -> string -> string
end

structure MaximaProcess : MAXIMA_PROCESS =
struct

exception Maxima of string
type T = (TextIO.instream, TextIO.outstream) Unix.proc
fun flush pr = let
    val (istr,ostr) = Unix.streamsOf pr
    val random_str = (Int.toString o round o Library.random) ()
    val rec chomp =
	(fn () =>
	    if String.isSubstring
		   ("FLUSH"^random_str)
		   (the (TextIO.inputLine istr)) then 
		TextIO.inputLine istr
	    else chomp())
in
    (TextIO.output (ostr, "print (\"FLUSH"^random_str^"\");\n");chomp();pr)
end

fun new () = flush (Unix.execute ("/usr/local/bin/maxima",[]))
fun kill pr = Unix.kill(pr,9)

fun read_grind [] str = raise Maxima str
  | read_grind (c::cs) str = case c
			  of #"$" => []
			   | _ => c::(read_grind cs str)
fun eval pr str = let
    val (istr,ostr) = Unix.streamsOf pr
    val _ = TextIO.output (ostr, "v:("^str^");\n");
    val _ = flush pr
    val _ = TextIO.output (ostr, "grind(v);\n")
    val _ = TextIO.inputLine istr
    val ln = the (TextIO.inputLine istr)
in String.implode (read_grind (String.explode ln) ln)
end



end


(* leave this here to debug, so we don't get a million procs *)
val _ = MaximaProcess.kill MaximaExpression.root_proc 



(*
structure MaximaExpression
================================================================================
Evaluate maxima expressions with a single underlying process in a
thread-safe manner.

Note MaximaExpression respects the signatures RING and FIELD
*)

structure MaximaExpression =
struct

type T = string

val root_proc = MaximaProcess.new ()
val root_proc_m = Mutex.mutex ();
fun eval str = (Mutex.lock root_proc_m;
		(MaximaProcess.eval root_proc str)
		before Mutex.unlock root_proc_m)
handle MaximaProcess.Maxima str =>
       (Mutex.unlock root_proc_m; "MAXIMA_ERROR:"^str)


val _ = eval ("match(pat,tgt) := (vs:listofvars(pat),"^
	      "  if vs=[] then (if pat=tgt then [] else NO_MATCH) "^
	      "  else"^
	      "    first(solve(map(lambda([v],first(solve(pat=tgt,v))),vs),vs))"^
	      ")")

fun match pat tgt = case eval ("match("^pat^","^tgt^")")
		     of "NO_MATCH" => NONE
		      | str => SOME str
fun do_subs subs expr = eval("ev("^expr^","^subs^")")

fun fromString str = eval str
val fromInt = fromString o Int.toString
fun (op+) (s1,s2) = eval ("("^s1^")+("^s2^")")
fun (op*) (s1,s2) = eval ("("^s1^")*("^s2^")")
fun (op/) (s1,s2) = eval ("("^s1^")/("^s2^")")
fun (op~) s = eval ("-("^s^")")
fun invert s = "1" / s
fun conj s = eval ("conjugate("^s^")")

val compare = String.compare
val zero = "0"
val one = "1"
fun is_zero exp = ((compare (exp, zero)) = EQUAL)
fun is_one exp = ((compare (exp, one)) = EQUAL)
val toString : T -> string = fn s => s
val pretty = Pretty.str o toString
val print = Pretty.writeln o pretty
end

structure ME = MaximaExpression

(*
use "alebra.ML" for this stuff
==================================================
*)
structure ME = MaximaExpression;
structure Matr = MatrixFun(structure UField = ME);

val angle = Matr.map (ME.fromString) [["1"],
				      ["%e^(%i*A)"]]
val deltaZ = Matr.map (ME.fromString)
	     [["1","0"],
	      ["0","0"],
	      ["0","0"],
	      ["0","1"]]

val ident = Matr.map (ME.fromString)
	    [["1","0"],
	     ["0","1"]]

val phase = Matr.mult
		(Matr.transpose deltaZ)
		(Matr.kronecker_product ident angle)

