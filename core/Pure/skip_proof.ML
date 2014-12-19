(*  Title:      Pure/skip_proof.ML
    Author:     Makarius

Skip proof via oracle invocation.
*)

val quick_and_dirty_raw = Config.declare_option ("quick_and_dirty", @{here});
val quick_and_dirty = Config.bool quick_and_dirty_raw;

signature SKIP_PROOF =
sig
  val report: Proof.context -> unit
  val make_thm_cterm: cterm -> thm
  val make_thm: theory -> term -> thm
  val cheat_tac: int -> tactic
end;

structure Skip_Proof: SKIP_PROOF =
struct

(* report *)

fun report ctxt =
  if Context_Position.is_visible ctxt then
    Output.report [Markup.markup Markup.bad "Skipped proof"]
  else ();


(* oracle setup *)

val (_, make_thm_cterm) =
  Context.>>>
    (Context.map_theory_result (Thm.add_oracle (Binding.make ("skip_proof", @{here}), I)));

fun make_thm thy prop = make_thm_cterm (Thm.cterm_of thy prop);


(* cheat_tac *)

fun cheat_tac i st =
  rtac (make_thm (Thm.theory_of_thm st) (Var (("A", 0), propT))) i st;

end;
