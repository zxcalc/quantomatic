(*  Title:      Pure/ML/ml_name_space.ML
    Author:     Makarius

ML name space, with initial entries for strict Standard ML.
*)

signature ML_NAME_SPACE =
sig
  type T
  val global: T
  val global_values: (string * string) list -> T
  val forget_val: string -> unit
  val forget_type: string -> unit
  val forget_structure: string -> unit
  val bootstrap_values: string list
  val hidden_structures: string list
  val bootstrap_structures: string list
  val bootstrap_signatures: string list
  val sml_val: (string * PolyML.NameSpace.Values.value) list
  val sml_type: (string * PolyML.NameSpace.TypeConstrs.typeConstr) list
  val sml_fixity: (string * PolyML.NameSpace.Infixes.fixity) list
  val sml_structure: (string * PolyML.NameSpace.Structures.structureVal) list
  val sml_signature: (string * PolyML.NameSpace.Signatures.signatureVal) list
  val sml_functor: (string * PolyML.NameSpace.Functors.functorVal) list
end;

structure ML_Name_Space: ML_NAME_SPACE =
struct

type T = PolyML.NameSpace.nameSpace;


(* global *)

val global = PolyML.globalNameSpace;
fun global_values values : T =
 {lookupVal = #lookupVal global,
  lookupType = #lookupType global,
  lookupStruct = #lookupStruct global,
  lookupFix = #lookupFix global,
  lookupSig = #lookupSig global,
  lookupFunct = #lookupFunct global,
  enterVal =
    fn (x, value) =>
      (case List.find (fn (y, _) => x = y) values of
        SOME (_, x') => #enterVal global (x', value)
      | NONE => ()),
  enterType = fn _ => (),
  enterFix = fn _ => (),
  enterStruct = fn _ => (),
  enterSig = fn _ => (),
  enterFunct = fn _ => (),
  allVal = #allVal global,
  allType = #allType global,
  allFix = #allFix global,
  allStruct = #allStruct global,
  allSig = #allSig global,
  allFunct = #allFunct global};


(* forget entries *)

val forget_val = PolyML.Compiler.forgetValue;
val forget_type = PolyML.Compiler.forgetType;
val forget_structure = PolyML.Compiler.forgetStructure;


(* bootstrap environment *)

val bootstrap_values =
  ["use", "exit", "ML_file", "ML_system_pretty", "ML_system_pp", "ML_system_overload",
    "chapter", "section", "subsection", "subsubsection", "paragraph", "subparagraph"];
val hidden_structures = ["CInterface", "Foreign", "RunCall", "RuntimeCalls", "Signal"];
val bootstrap_structures =
  ["Exn", "Output_Primitives", "Basic_Exn", "Thread_Data", "Thread_Position", "ML_Recursive",
    "Private_Output", "PolyML"] @ hidden_structures;
val bootstrap_signatures =
  ["EXN", "OUTPUT_PRIMITIVES", "BASIC_EXN", "THREAD_DATA", "THREAD_POSITION", "ML_RECURSIVE",
    "PRIVATE_OUTPUT", "ML_NAME_SPACE"];


(* Standard ML environment *)

val sml_val =
  List.filter (fn (a, _) => List.all (fn b => a <> b) bootstrap_values) (#allVal global ());
val sml_type = #allType global ();
val sml_fixity = #allFix global ();
val sml_structure =
  List.filter (fn (a, _) => List.all (fn b => a <> b) bootstrap_structures) (#allStruct global ());
val sml_signature =
  List.filter (fn (a, _) => List.all (fn b => a <> b) bootstrap_signatures) (#allSig global ());
val sml_functor = #allFunct global ();

end;
