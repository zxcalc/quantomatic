(*  Title:      Pure/assumption.ML
    Author:     Makarius

Context assumptions, parameterized by export rules.
*)

signature ASSUMPTION =
sig
  type export = bool -> cterm list -> (thm -> thm) * (term -> term)
  val assume_export: export
  val presume_export: export
  val assume: Proof.context -> cterm -> thm
  val assume_hyps: cterm -> Proof.context -> thm * Proof.context
  val all_assms_of: Proof.context -> cterm list
  val all_prems_of: Proof.context -> thm list
  val local_assms_of: Proof.context -> Proof.context -> cterm list
  val local_prems_of: Proof.context -> Proof.context -> thm list
  val add_assms: export -> cterm list -> Proof.context -> thm list * Proof.context
  val add_assumes: cterm list -> Proof.context -> thm list * Proof.context
  val export: bool -> Proof.context -> Proof.context -> thm -> thm
  val export_term: Proof.context -> Proof.context -> term -> term
  val export_morphism: Proof.context -> Proof.context -> morphism
end;

structure Assumption: ASSUMPTION =
struct

(** basic rules **)

type export = bool -> cterm list -> (thm -> thm) * (term -> term);

(*
    [A]
     :
     B
  --------
  #A ==> B
*)
fun assume_export is_goal asms =
  (if is_goal then Drule.implies_intr_protected asms else Drule.implies_intr_list asms, fn t => t);

(*
    [A]
     :
     B
  -------
  A ==> B
*)
fun presume_export _ = assume_export false;


fun assume ctxt = Raw_Simplifier.norm_hhf ctxt o Thm.assume;

fun assume_hyps ct ctxt =
  let val (th, ctxt') = Thm.assume_hyps ct ctxt
  in (Raw_Simplifier.norm_hhf ctxt' th, ctxt') end;



(** local context data **)

datatype data = Data of
 {assms: (export * cterm list) list,    (*assumes: A ==> _*)
  prems: thm list};                     (*prems: A |- norm_hhf A*)

fun make_data (assms, prems) = Data {assms = assms, prems = prems};
val empty_data = make_data ([], []);

structure Data = Proof_Data
(
  type T = data;
  fun init _ = empty_data;
);

fun map_data f = Data.map (fn Data {assms, prems} => make_data (f (assms, prems)));
fun rep_data ctxt = Data.get ctxt |> (fn Data rep => rep);


(* all assumptions *)

val all_assumptions_of = #assms o rep_data;
val all_assms_of = maps #2 o all_assumptions_of;
val all_prems_of = #prems o rep_data;


(* local assumptions *)

fun local_assumptions_of inner outer =
  drop (length (all_assumptions_of outer)) (all_assumptions_of inner);

val local_assms_of = maps #2 oo local_assumptions_of;

fun local_prems_of inner outer =
  drop (length (all_prems_of outer)) (all_prems_of inner);


(* add assumptions *)

fun add_assms export new_asms ctxt =
  let val (new_prems, ctxt') = fold_map assume_hyps new_asms ctxt in
    ctxt'
    |> map_data (fn (asms, prems) => (asms @ [(export, new_asms)], prems @ new_prems))
    |> pair new_prems
  end;

val add_assumes = add_assms assume_export;


(* export *)

fun export is_goal inner outer =
  Raw_Simplifier.norm_hhf_protect inner #>
  fold_rev (fn (e, As) => #1 (e is_goal As)) (local_assumptions_of inner outer) #>
  Raw_Simplifier.norm_hhf_protect outer;

fun export_term inner outer =
  fold_rev (fn (e, As) => #2 (e false As)) (local_assumptions_of inner outer);

fun export_morphism inner outer =
  let
    val thm = export false inner outer;
    val term = export_term inner outer;
    val typ = Logic.type_map term;
  in
    Morphism.morphism "Assumption.export"
      {binding = [], typ = [typ], term = [term], fact = [map thm]}
  end;

end;
