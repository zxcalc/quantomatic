(*  Title:      Pure/ML-Systems/polyml.ML
    Author:     Makarius

Compatibility wrapper for Poly/ML.
*)

(* ML name space *)

structure ML_Name_Space =
struct
  open PolyML.NameSpace;
  type T = PolyML.NameSpace.nameSpace;
  val global = PolyML.globalNameSpace;
  val initial_val =
    List.filter (fn (a, _) => a <> "use" andalso a <> "exit" andalso a <> "commit")
      (#allVal global ());
  val initial_type = #allType global ();
  val initial_fixity = #allFix global ();
  val initial_structure = #allStruct global ();
  val initial_signature = #allSig global ();
  val initial_functor = #allFunct global ();
end;


(* ML system operations *)

use "ML-Systems/ml_system.ML";

if ML_System.name = "polyml-5.3.0"
then use "ML-Systems/share_common_data_polyml-5.3.0.ML"
else ();

structure ML_System =
struct

open ML_System;

fun share_common_data () = PolyML.shareCommonData PolyML.rootFunction;
val save_state = PolyML.SaveState.saveState;

end;


(* exceptions *)

fun reraise exn =
  (case PolyML.exceptionLocation exn of
    NONE => raise exn
  | SOME location => PolyML.raiseWithLocation (exn, location));

exception Interrupt = SML90.Interrupt;

use "General/exn.ML";


(* multithreading *)

val seconds = Time.fromReal;

if List.exists (fn s => s = "SingleAssignment") (PolyML.Compiler.structureNames ())
then ()
else use "ML-Systems/single_assignment_polyml.ML";

open Thread;
use "ML-Systems/multithreading.ML";
use "ML-Systems/multithreading_polyml.ML";

use "ML-Systems/unsynchronized.ML";
val _ = PolyML.Compiler.forgetValue "ref";
val _ = PolyML.Compiler.forgetType "ref";


(* pervasive environment *)

val _ = PolyML.Compiler.forgetValue "isSome";
val _ = PolyML.Compiler.forgetValue "getOpt";
val _ = PolyML.Compiler.forgetValue "valOf";
val _ = PolyML.Compiler.forgetValue "foldl";
val _ = PolyML.Compiler.forgetValue "foldr";
val _ = PolyML.Compiler.forgetValue "print";
val _ = PolyML.Compiler.forgetValue "explode";
val _ = PolyML.Compiler.forgetValue "concat";

val ord = SML90.ord;
val chr = SML90.chr;
val raw_explode = SML90.explode;
val implode = SML90.implode;

val io_buffer_size = 4096;

fun quit () = exit 0;


(* ML runtime system *)

fun print_exception_trace (_: exn -> string) = PolyML.exception_trace;
val timing = PolyML.timing;
val profiling = PolyML.profiling;

fun profile 0 f x = f x
  | profile n f x =
      let
        val _ = RunCall.run_call1 RuntimeCalls.POLY_SYS_profiler n;
        val res = Exn.capture f x;
        val _ = RunCall.run_call1 RuntimeCalls.POLY_SYS_profiler 0;
      in Exn.release res end;

val pointer_eq = PolyML.pointerEq;


(* ML compiler *)

use "ML-Systems/use_context.ML";
use "ML-Systems/ml_positions.ML";
use "ML-Systems/compiler_polyml.ML";

PolyML.Compiler.reportUnreferencedIds := true;
PolyML.Compiler.printInAlphabeticalOrder := false;
PolyML.Compiler.maxInlineSize := 80;

fun ml_prompts p1 p2 = (PolyML.Compiler.prompt1 := p1; PolyML.Compiler.prompt2 := p2);


(* ML toplevel pretty printing *)

use "ML-Systems/ml_pretty.ML";

local
  val depth = Unsynchronized.ref 10;
in
  fun get_default_print_depth () = ! depth;
  fun default_print_depth n = (depth := n; PolyML.print_depth n);
  val _ = default_print_depth 10;
end;

val error_depth = PolyML.error_depth;

val pretty_ml =
  let
    fun convert _ (PolyML.PrettyBreak (wd, _)) = ML_Pretty.Break (false, wd)
      | convert _ (PolyML.PrettyBlock (_, _,
            [PolyML.ContextProperty ("fbrk", _)], [PolyML.PrettyString " "])) =
          ML_Pretty.Break (true, 1)
      | convert len (PolyML.PrettyBlock (ind, _, context, prts)) =
          let
            fun property name default =
              (case List.find (fn PolyML.ContextProperty (a, _) => name = a | _ => false) context of
                SOME (PolyML.ContextProperty (_, b)) => b
              | _ => default);
            val bg = property "begin" "";
            val en = property "end" "";
            val len' = property "length" len;
          in ML_Pretty.Block ((bg, en), map (convert len') prts, ind) end
      | convert len (PolyML.PrettyString s) =
          ML_Pretty.String (s, case Int.fromString len of SOME i => i | NONE => size s)
  in convert "" end;

fun ml_pretty (ML_Pretty.Break (false, wd)) = PolyML.PrettyBreak (wd, 0)
  | ml_pretty (ML_Pretty.Break (true, _)) =
      PolyML.PrettyBlock (0, false, [PolyML.ContextProperty ("fbrk", "")],
        [PolyML.PrettyString " "])
  | ml_pretty (ML_Pretty.Block ((bg, en), prts, ind)) =
      let val context =
        (if bg = "" then [] else [PolyML.ContextProperty ("begin", bg)]) @
        (if en = "" then [] else [PolyML.ContextProperty ("end", en)])
      in PolyML.PrettyBlock (ind, false, context, map ml_pretty prts) end
  | ml_pretty (ML_Pretty.String (s, len)) =
      if len = size s then PolyML.PrettyString s
      else PolyML.PrettyBlock
        (0, false, [PolyML.ContextProperty ("length", Int.toString len)], [PolyML.PrettyString s]);

fun toplevel_pp context (_: string list) pp =
  use_text context (1, "pp") false
    ("PolyML.addPrettyPrinter (fn _ => fn _ => ml_pretty o Pretty.to_ML o (" ^ pp ^ "))");

val ml_make_string =
  "(fn x => Pretty.string_of (Pretty.from_ML (pretty_ml (PolyML.prettyRepresentation (x, Isabelle.ML_print_depth ())))))";

