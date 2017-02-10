(*  Title:      Pure/Isar/spec_rules.ML
    Author:     Makarius

Rules that characterize specifications, with rough classification.
NB: In the face of arbitrary morphisms, the original shape of
specifications may get lost.
*)

signature SPEC_RULES =
sig
  datatype rough_classification = Unknown | Equational | Inductive | Co_Inductive
  type spec = rough_classification * (term list * thm list)
  val get: Proof.context -> spec list
  val get_global: theory -> spec list
  val retrieve: Proof.context -> term -> spec list
  val retrieve_global: theory -> term -> spec list
  val add: rough_classification -> term list * thm list -> local_theory -> local_theory
  val add_global: rough_classification -> term list * thm list -> theory -> theory
end;

structure Spec_Rules: SPEC_RULES =
struct

(* rules *)

datatype rough_classification = Unknown | Equational | Inductive | Co_Inductive;
type spec = rough_classification * (term list * thm list);

structure Rules = Generic_Data
(
  type T = spec Item_Net.T;
  val empty : T =
    Item_Net.init
      (fn ((class1, (ts1, ths1)), (class2, (ts2, ths2))) =>
        class1 = class2 andalso
        eq_list (op aconv) (ts1, ts2) andalso
        eq_list Thm.eq_thm_prop (ths1, ths2))
      (#1 o #2);
  val extend = I;
  val merge = Item_Net.merge;
);


(* get *)

fun get_generic context =
  let
    val thy = Context.theory_of context;
    val transfer = Global_Theory.transfer_theories thy;
  in Item_Net.content (Rules.get context) |> (map o apsnd o apsnd o map) transfer end;

val get = get_generic o Context.Proof;
val get_global = get_generic o Context.Theory;


(* retrieve *)

fun retrieve_generic context =
  Item_Net.retrieve (Rules.get context)
  #> (map o apsnd o apsnd o map) (Thm.transfer (Context.theory_of context));

val retrieve = retrieve_generic o Context.Proof;
val retrieve_global = retrieve_generic o Context.Theory;


(* add *)

fun add class (ts, ths) lthy =
  let
    val cts = map (Thm.cterm_of lthy) ts;
  in
    lthy |> Local_Theory.declaration {syntax = false, pervasive = true}
      (fn phi =>
        let
          val (ts', ths') =
            Morphism.fact phi (map Drule.mk_term cts @ ths)
            |> chop (length cts)
            |>> map (Thm.term_of o Drule.dest_term)
            ||> map Thm.trim_context;
        in Rules.map (Item_Net.update (class, (ts', ths'))) end)
  end;

fun add_global class spec =
  Context.theory_map (Rules.map (Item_Net.update (class, (apsnd o map) Thm.trim_context spec)));

end;
