(*  Title:      Pure/Tools/class_deps.ML
    Author:     Florian Haftmann, TU Muenchen

Visualization of class dependencies.
*)

signature CLASS_DEPS =
sig
  val class_deps: Proof.context -> sort list option * sort list option -> Graph_Display.entry list
  val class_deps_cmd: Proof.context -> string list option * string list option -> unit
end;

structure Class_Deps: CLASS_DEPS =
struct

fun gen_class_deps prep_sort ctxt bounds =
  let
    val (upper, lower) = apply2 ((Option.map o map) (prep_sort ctxt)) bounds;
    val {classes = (space, algebra), ...} = Type.rep_tsig (Proof_Context.tsig_of ctxt);
    val rel = Sorts.sort_le algebra;
    val pred =
      (case upper of
        SOME bs => (fn c => exists (fn b => rel ([c], b)) bs)
      | NONE => K true) andf
      (case lower of
        SOME bs => (fn c => exists (fn b => rel (b, [c])) bs)
      | NONE => K true);
    fun node c =
      Graph_Display.content_node (Name_Space.extern ctxt space c)
        (Class.pretty_specification (Proof_Context.theory_of ctxt) c);
  in
    Sorts.subalgebra (Context.Proof ctxt) pred (K NONE) algebra
    |> #2 |> Sorts.classes_of |> Graph.dest
    |> map (fn ((c, _), ds) => ((c, node c), ds))
  end;

val class_deps = gen_class_deps (Type.cert_sort o Proof_Context.tsig_of);
val class_deps_cmd = Graph_Display.display_graph oo gen_class_deps Syntax.read_sort;

end;
