(*  Title:      Pure/Tools/thy_deps.ML
    Author:     Makarius

Visualization of theory dependencies.
*)

signature THY_DEPS =
sig
  val thy_deps: Proof.context -> theory list option * theory list option -> Graph_Display.entry list
  val thy_deps_cmd: Proof.context ->
    (string * Position.T) list option * (string * Position.T) list option -> unit
end;

structure Thy_Deps: THY_DEPS =
struct

fun gen_thy_deps _ ctxt (NONE, NONE) =
      let
        val parent_session = Session.get_name ();
        val parent_loaded = is_some o Thy_Info.lookup_theory;
      in Present.session_graph parent_session parent_loaded (Proof_Context.theory_of ctxt) end
  | gen_thy_deps prep_thy ctxt bounds =
      let
        val (upper, lower) = apply2 ((Option.map o map) (prep_thy ctxt)) bounds;
        val rel = Context.subthy o swap;
        val pred =
          (case upper of
            SOME Bs => (fn thy => exists (fn B => rel (thy, B)) Bs)
          | NONE => K true) andf
          (case lower of
            SOME Bs => (fn thy => exists (fn B => rel (B, thy)) Bs)
          | NONE => K true);
        fun node thy =
          ((Context.theory_name thy, Graph_Display.content_node (Context.theory_name thy) []),
            map Context.theory_name (filter pred (Theory.parents_of thy)));
      in map node (filter pred (Theory.nodes_of (Proof_Context.theory_of ctxt))) end;

val thy_deps =
  gen_thy_deps (fn ctxt => fn thy =>
    let val thy0 = Proof_Context.theory_of ctxt
    in if Context.subthy (thy, thy0) then thy else raise THEORY ("Bad theory", [thy, thy0]) end);

val thy_deps_cmd = Graph_Display.display_graph oo gen_thy_deps Theory.check;

end;
