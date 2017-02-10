(*  Title:      Pure/Tools/thm_deps.ML
    Author:     Stefan Berghofer, TU Muenchen

Visualize dependencies of theorems.
*)

signature THM_DEPS =
sig
  val thm_deps: theory -> thm list -> unit
  val unused_thms: theory list * theory list -> (string * thm) list
end;

structure Thm_Deps: THM_DEPS =
struct

(* thm_deps *)

fun thm_deps thy thms =
  let
    fun make_node name directory =
      Graph_Display.session_node
       {name = Long_Name.base_name name, directory = directory, unfold = false, path = ""};
    fun add_dep ("", _, _) = I
      | add_dep (name, _, PBody {thms = thms', ...}) =
          let
            val prefix = #1 (split_last (Long_Name.explode name));
            val session =
              (case prefix of
                a :: _ =>
                  (case try (Context.get_theory thy) a of
                    SOME thy =>
                      (case Present.session_name thy of
                        "" => []
                      | session => [session])
                  | NONE => [])
              | _ => ["global"]);
            val node = make_node name (space_implode "/" (session @ prefix));
            val deps = filter_out (fn s => s = "") (map (#1 o #2) thms');
          in Symtab.update (name, (node, deps)) end;
    val entries0 = Proofterm.fold_body_thms add_dep (Thm.proof_bodies_of thms) Symtab.empty;
    val entries1 =
      Symtab.fold (fn (_, (_, deps)) => deps |> fold (fn d => fn tab =>
        if Symtab.defined tab d then tab
        else Symtab.update (d, (make_node d "", [])) tab)) entries0 entries0;
  in
    Symtab.fold (fn (name, (node, deps)) => cons ((name, node), deps)) entries1 []
    |> Graph_Display.display_graph_old
  end;


(* unused_thms *)

fun unused_thms (base_thys, thys) =
  let
    fun add_fact transfer space (name, ths) =
      if exists (fn thy => Global_Theory.defined_fact thy name) base_thys then I
      else
        let val {concealed, group, ...} = Name_Space.the_entry space name in
          fold_rev (fn th =>
            (case Thm.derivation_name th of
              "" => I
            | a => cons (a, (transfer th, concealed, group)))) ths
        end;
    fun add_facts thy =
      let
        val transfer = Global_Theory.transfer_theories thy;
        val facts = Global_Theory.facts_of thy;
      in Facts.fold_static (add_fact transfer (Facts.space_of facts)) facts end;

    val new_thms =
      fold add_facts thys []
      |> sort_distinct (string_ord o apply2 #1);

    val used =
      Proofterm.fold_body_thms
        (fn (a, _, _) => a <> "" ? Symtab.update (a, ()))
        (map Proofterm.strip_thm (Thm.proof_bodies_of (map (#1 o #2) new_thms)))
        Symtab.empty;

    fun is_unused a = not (Symtab.defined used a);

    (*groups containing at least one used theorem*)
    val used_groups = fold (fn (a, (_, _, group)) =>
      if is_unused a then I
      else
        (case group of
          NONE => I
        | SOME grp => Inttab.update (grp, ()))) new_thms Inttab.empty;

    val (thms', _) = fold (fn (a, (th, concealed, group)) => fn q as (thms, seen_groups) =>
      if not concealed andalso
        (* FIXME replace by robust treatment of thm groups *)
        Thm.legacy_get_kind th = Thm.theoremK andalso is_unused a
      then
        (case group of
           NONE => ((a, th) :: thms, seen_groups)
         | SOME grp =>
             if Inttab.defined used_groups grp orelse
               Inttab.defined seen_groups grp then q
             else ((a, th) :: thms, Inttab.update (grp, ()) seen_groups))
      else q) new_thms ([], Inttab.empty);
  in rev thms' end;

end;
