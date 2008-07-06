(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Theory with Fixed Logical Kernel style *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
signature THEORY 
= sig

structure Rule : RULE
structure Rn : NAMES where type Rn.name = string

type T

val empty : T

(* adds an assumption/axiom *)
val add_assm : Rn.name -> Rule.T -> T -> T

exception no_such_result_exp of Rn.name * T;

val get_assms : T -> Rule.T Rn.NTab.T
val get_assm_list : T -> (Rn.name * Rule.T) list
val get_assm : T -> Rn.name -> Rule.T

val trivial : Rn.name -> Rule.RGGraph.T -> T -> (Rn.name * T)
val symmetric : Rn.name -> T -> (Rn.name * T)
val subst : Rn.name -> Rn.name -> T -> (Rn.name * T) Seq.seq

end;


structure DB_Theory
= struct


structure Rule : RULE = Rule
structure Rn :> NAMES where type name = string = StrName;

datatype proof = 
         Proof of (Rn.name (* applied the rule named this *)
                   * Rn.name option (* resulting in this subgoal *)
                   * bool (* true = applied left to right, false = right to left *)
                   * Match.T (* as instantiated here *)
                  )
       | Refl;

datatype result = Result of Rule.T (* final resulting rule/conjecture *)
                            * proof option; (* proof *)

datatype T = Theory of { assms: Rn.NSet.T, (* names of axioms of the theory *)
                         topns: Rn.NSet.T, (* names of interesting results *)
                         rs : result Rn.NTab.T,  (* results in the theory *)
                         deps : Rn.NSet.T Rn.NTab.T (* cached dependencies: 
                                                       rule -> rule used in proof of *)
                       };

(* basic proof functions *)

(* basic result functions *)


(* basic theory functions *)
fun get_assms (Theory rep) = #assms rep;
fun update_assms (Theory rep) = Theory {assms = f (#assms rep)};
val set_assms = update_assms o K;

fun get_topns (Theory rep) = #topns rep;
fun update_topns (Theory rep) = Theory {topns = f (#topns rep)};
val set_topns = update_topns o K;

fun get_rs (Theory rep) = #rs rep;
fun update_rs (Theory rep) = Theory {rs = f (#rs rep)};
val set_rs = update_rs o K;

fun get_deps (Theory rep) = #deps rep;
fun update_deps (Theory rep) = Theory {deps = f (#deps rep)};
val set_deps = update_deps o K;

(* implicit: thry n *)
val get_r_deps = Rn.NTab.get o get_deps;

(* implicit: n f *)
val update_result = update_rs oo Rn.NTab.update;

(* getting all assumptions *)
val get_assm = RnTab.get o get_assms;
val get_assms_list = RnTab.dest o get_assms;

(* new empty theory *)
val empty = Theory { assms = Rn.NSet.empty,
                     topns = Rn.NSet.empty,
                     rs = Rn.NTab.empty,
                     deps = Rn.NTab.empty
                   };

fun add_named_r nr thry = 
    let val (n2,rs2) = Rn.NTab.add nr (get_rs thry)
    in (n2, set_rs rs2 thry) end;

(* assume a new result *)
(* implicit theory *)
fun add_assm n r = 
    update_assms (Rn.NSet.ins_fresh n)
    o update_rs (Rn.NTab.ins (n,r));

(* rename a result within a theory *)
fun rename_in_proof (from_n,to_n) (Proof (used_n,optto_n,lr,m)) =
    Proof (if Rn.name_eq (from_n,used_n) 
           then to_n else used_n, 
           case optto_n 
            of NONE => NONE
             | SOME n => if Rn.name_eq (n,optto_n) 
                         then to_n else optto_n,
           lr, m)
  | rename_in_proof _ Refl = Refl;

fun rename_in_result rn (Result (r,p)) = Result (r, rename_in_proof rn p);

fun rename (rn as (n1, n2)) thry = 
    NSet.fold 
      (update_result (rename_in_result rn))
      (get_r_deps n1 thry)
      thry;

(* implicit: n thry *)
val note_result = update_topns o Rn.NSet.add;
val unnote_result  = update_topns o Rn.NSet.del;

(* Make a new trivial result from a graph. *)
fun trivial n g thry = add_named_r (n,(Result(Rule.mk (g,g), SOME Refl))) thry;
(* conjecture a new result, is unproved *)
fun conjecture n r thry = add_named_r (r,Result(r,NONE)) thry;

(* implicit n thry *)
(* ? maybe raise exception if result is currently closed *)
val openify_result = update_result (fn Result (r,p) => Result (r,NONE));

(* delete result and all applications of result *)
fun delete n thry = 
    thry |> update_assms (NSet.del n)
         |> update_topns (NSet.del n)
         |> update_rs (NTab.delete n)
         (* deps of n (depn) uses n, if n is subgoal of depn,
            depn become open subgoal again. if n is used as a rule to rewrite
            depn, then also depn becomes open. So, just openify depn. *) 
         |> NSet.fold openify_result (get_r_deps thry n)
         |> update_deps (NTab.del n)

(* converts between "A = B" and "B = A" *)
fun symmetric n thry = 
    let val Result (r,p) = get_result thry n
    in case p of 
         NONE => let val n2 = conjecture n (Rule.symmetric r);
                 in Result (r, SOME (n2,NONE,true,))end
                 
    end;

exception no_such_result_exp of Rn.name * T;

(* backward reasoning *)
val subst_rhs_lr : Rn.name -> Rn.name -> T -> (Rn.name * T) Seq.seq
(* forward reasoning *)
val subst_rhs_rl : Rn.name -> Rn.name -> T -> (Rn.name * T) Seq.seq


end;

