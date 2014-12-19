(*
 * Emulate the Isabelle toplevel environment
 *)
OS.FileSys.chDir "isabelle";

fun exit st =
  OS.Process.exit
  (if st = 0 then OS.Process.success else OS.Process.failure);

use "ML-Systems/polyml.ML";

(* from Isabelle/src/Pure/Generic *)
use "General/basics.ML";
(* from Isabelle/src/Pure *)
use "basics/library.ML";

(* Global version variable *)
val version = "Isabelle/IsaPlanner Library";

(* from Isabelle/src/Pure/Generic *)
use "General/print_mode.ML";
use "General/alist.ML";
use "General/table.ML";
use "General/properties.ML";

(* from Isabelle/src/Pure/Concurrent *)
(*use "Concurrent/simple_thread.ML";
use "Concurrent/synchronized.ML";*)
use "Concurrent/simple_thread.ML";

use "Concurrent/synchronized.ML";
if Multithreading.available then ()
else use "Concurrent/synchronized_sequential.ML";
use "Concurrent/counter.ML";

(* from Isabelle/src/Pure/Generic *)
use "General/output.ML";
use "PIDE/markup.ML";

fun legacy_feature s = warning (Markup.markup Markup.legacy ("Legacy feature! " ^ s));

use "General/timing.ML";
use "General/scan.ML";
use "General/source.ML";
use "General/symbol.ML";
use "General/seq.ML";
use "General/position.ML";
use "General/symbol_pos.ML";
use "General/integer.ML";
use "General/stack.ML";
use "General/queue.ML";
use "General/heap.ML";
use "General/ord_list.ML";
use "General/balanced_tree.ML";
use "General/long_name.ML";
use "General/buffer.ML";
use "General/pretty.ML";
use "PIDE/xml.ML";
use "General/graph.ML";
use "General/binding.ML";
use "General/path.ML";
use "General/url.ML";
use "General/file.ML";
use "General/sha1.ML";
use "PIDE/yxml.ML";
use "System/options.ML";

(* hard-coded options *)
val options = Options.empty |>
  Options.declare {pos=Position.none, name="completion_limit",
                   typ=Options.intT, value="200"};
Options.set_default options;

use "General/completion.ML";
(*use "General/json.ML";*)
(*use "General/text_socket.ML";*)


(* Isar lexer, for use_thy *)

(* all proof-related types are stubs  *)
type typ = unit;
type term = unit;
type attribute = unit;
type morphism = unit;
type thm = unit;
type indexname = string * int;


(* a very minimal lex structure for inner syntax *)
structure Lexicon =
struct
open Basic_Symbol_Pos;

val err_prefix = "Inner lexical error: ";

fun !!! msg = Symbol_Pos.!!! (fn () => err_prefix ^ msg);

val scan_id = Symbol_Pos.scan_ident;
val scan_longid = scan_id @@@ (Scan.repeat1 ($$$ "." @@@ scan_id) >> flat);
val scan_tid = $$$ "'" @@@ scan_id;

val scan_nat = Scan.many1 (Symbol.is_digit o Symbol_Pos.symbol);
val scan_int = $$$ "-" @@@ scan_nat || scan_nat;
val scan_natdot = scan_nat @@@ $$$ "." @@@ scan_nat;
val scan_float = $$$ "-" @@@ scan_natdot || scan_natdot;
val scan_hex = $$$ "0" @@@ $$$ "x" @@@ Scan.many1 (Symbol.is_ascii_hex o Symbol_Pos.symbol);
val scan_bin = $$$ "0" @@@ $$$ "b" @@@ Scan.many1 (fn (s, _) => s = "0" orelse s = "1");

val scan_id_nat = scan_id @@@ Scan.optional ($$$ "." @@@ scan_nat) [];
val scan_var = $$$ "?" @@@ scan_id_nat;
val scan_tvar = $$$ "?" @@@ $$$ "'" @@@ scan_id_nat;
end;

use "Isar/token.ML";


(* from Isabelle/src/Pure/Concurrent; this is the main stuff loaded in
   Pure/ROOT.ML *)
use "ML/exn_properties_polyml.ML";
use "ML/ml_statistics_polyml-5.5.0.ML";
(*use "Concurrent/ROOT.ML";*)
val rootDir = OS.FileSys.getDir();

(* Include isaplib *)
(*OS.FileSys.chDir "Concurrent";
use "ROOT.ML";
OS.FileSys.chDir "..";*)

(*MJB: simplification of the future calls.
It needs to be made here to use Synchronized and Future at the same time*)
(*use "Concurrent/task_queue.ML";
use "Concurrent/future.ML";*)

use "Concurrent/single_assignment.ML";
if Multithreading.available then ()
else use "Concurrent/single_assignment_sequential.ML";

if Multithreading.available
then use "Concurrent/bash.ML"
else use "Concurrent/bash_sequential.ML";

use "Concurrent/par_exn.ML";
use "Concurrent/task_queue.ML";
use "Concurrent/future.ML";
use "Concurrent/event_timer.ML";

if ML_System.is_polyml then use "Concurrent/time_limit.ML" else ();

use "Concurrent/lazy.ML";
if Multithreading.available then ()
else use "Concurrent/lazy_sequential.ML";

use "Concurrent/par_list.ML";
if Multithreading.available then ()
else use "Concurrent/par_list_sequential.ML";

use "Concurrent/mailbox.ML";
use "Concurrent/cache.ML";


(*use "General/stopwatch.ML";
use "Concurrent/future_wrapper.ML";
use "Concurrent/par_seq.ML";
use "Concurrent/compactor.ML";
use "Concurrent/par_tree.ML";*)

(* Other IsapLib Special stuff: names and graphs *)
(*PolyML.Project.use_root "names/ROOT.ML";
PolyML.Project.use_root "unif/ROOT.ML";
PolyML.Project.use_root "graph/ROOT.ML";
PolyML.Project.use_root "maps/ROOT.ML";
*)

(* Other basic top level things *)
(*use "basics/collection.ML";*)
(*use "basics/polym_table.ML";*)


(*use "basics/toplevel.ML";*)

use "use_thy.ML";

OS.FileSys.chDir "..";
