(*
 * Emulate the Isabelle toplevel environment
 *)
OS.FileSys.chDir "Pure";

fun exit st =
  OS.Process.exit
  (if st = 0 then OS.Process.success else OS.Process.failure);

(*use "General/exn.ML";
use "ML-Systems/thread_dummy.ML";
use "ML-Systems/multithreading.ML";
use "ML-Systems/polyml.ML";*)

use "ML-Systems/polyml.ML";
exception Interrupt;
use "General/exn.ML";
datatype ref = datatype Unsynchronized.ref;
use "ML-Systems/multithreading.ML";
use "ML-Systems/multithreading_polyml.ML";
fun tracing _ = ();

(* from Isabelle/src/Pure/Generic *)
use "General/basics.ML";
(* from Isabelle/src/Pure *)
use "library.ML";

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

(*fun legacy_feature s = warning (Markup.markup Markup.legacy ("Legacy feature! " ^ s));*)

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

use "name.ML";
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



OS.FileSys.chDir "..";


