(*
 * Emulate a (nearly) stock Isabelle toplevel environment
 *)
OS.FileSys.chDir "isabelle";

val exit = OS.Process.exit;

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
use "Concurrent/simple_thread.ML";
use "Concurrent/synchronized.ML";

(* from Isabelle/src/Pure/Generic *)
use "General/output.ML";
use "General/timing.ML";
use "General/markup.ML";
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
use "General/xml.ML";
use "General/graph.ML";
use "General/binding.ML";
use "General/path.ML";
use "General/url.ML";
use "General/file.ML";
use "General/yxml.ML";
(*use "General/json.ML";*)
(*use "General/text_socket.ML";*)


(* from Isabelle/src/Pure/Concurrent; this is the main stuff loaded in
   Pure/ROOT.ML *)
use "ML/exn_properties_polyml.ML";
use "ML/ml_statistics_polyml-5.5.0.ML";
(*use "Concurrent/ROOT.ML";*)
val rootDir = OS.FileSys.getDir();

(* Include isaplib *)
OS.FileSys.chDir "Concurrent";
use "ROOT.ML";
OS.FileSys.chDir "..";

(*MJB: simplification of the future calls.
It needs to be made here to use Synchronized and Future at the same time*)
use "General/stopwatch.ML";
use "Concurrent/future_wrapper.ML";
use "Concurrent/par_seq.ML";
use "Concurrent/compactor.ML";
use "Concurrent/par_tree.ML";

(* Other IsapLib Special stuff: names and graphs *)
(*PolyML.Project.use_root "names/ROOT.ML";
PolyML.Project.use_root "unif/ROOT.ML";
PolyML.Project.use_root "graph/ROOT.ML";
PolyML.Project.use_root "maps/ROOT.ML";
*)

(* Other basic top level things *)
use "basics/collection.ML";
use "basics/polym_table.ML";


use "basics/toplevel.ML";

(* adds support for "use"-ing Isabelle theories *)
use "use_thy.ML";

OS.FileSys.chDir "..";
