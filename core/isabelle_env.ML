(* Global version variable *)
val version = "Isabelle/IsaPlanner Library";

(*
 * Emulate the Isabelle toplevel environment
 *)
OS.FileSys.chDir "Pure";

fun exit st =
  OS.Process.exit
  (if st = 0 then OS.Process.success else OS.Process.failure);

use "ML-Systems/polyml-5.5.2.ML";
datatype ref = datatype Unsynchronized.ref;

use "General/basics.ML";
use "library.ML";
use "General/print_mode.ML";
use "General/alist.ML";
use "General/table.ML";

use "Concurrent/synchronized.ML";
if Multithreading.available then ()
else use "Concurrent/synchronized_sequential.ML";
use "Concurrent/counter.ML";

use "General/properties.ML";
use "General/output.ML";
use "PIDE/markup.ML";
use "General/scan.ML";
use "General/source.ML";
use "General/symbol.ML";
use "General/position.ML";
use "General/symbol_pos.ML";

use "General/integer.ML";
use "General/stack.ML";
use "General/queue.ML";
use "General/heap.ML";
use "General/ord_list.ML";
use "General/balanced_tree.ML";
use "General/buffer.ML";
use "General/pretty.ML";
use "PIDE/xml.ML";
use "General/path.ML";
use "General/url.ML";
use "General/file.ML";
use "General/long_name.ML";
use "General/binding.ML";
use "General/seq.ML";
use "General/timing.ML";

use "General/sha1.ML";

use "PIDE/yxml.ML";

use "General/graph.ML";

use "System/options.ML";

(* hard-coded options *)
val options =
  Options.empty
  |> Options.declare
      {pos = Position.none, name = "completion_limit", typ = Options.intT, value = "200"}
  |> Options.declare
      {pos = Position.none, name = "threads_stack_limit", typ = Options.realT, value = "0.25"};

Options.set_default options;


use "ML/exn_properties_polyml.ML";
use "ML/ml_statistics_polyml-5.5.0.ML";

use "Concurrent/simple_thread.ML";

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


use "name.ML";
use "General/completion.ML";

val rootDir = OS.FileSys.getDir ();

OS.FileSys.chDir "..";
