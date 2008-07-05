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

datatype proof = Proof of (Rn.name (* applied the rule named this *)
                           * Match.T (* as instantiated here *)
                           * Rn.name option (* resulting in this subgoal *)
                          );

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

(* getting all assumptions *)
val get_assm = RnTab.get o get_assms;
val get_assms_list = RnTab.dest o get_assms;

(* new empty theory *)
val empty = Theory { assms = Rn.NSet.empty,
                     topns = Rn.NSet.empty,
                     rs = Rn.NTab.empty,
                     deps = Rn.NTab.empty
                   };

fun add_trynamed_r nr thry = 
    let val (n2,rs2) = Rn.NTab.add nr (get_rs thry)
    in (n2, set_rs rs2 thry) end;

(* assume a new result *)
(* implicit theory *)
fun add_assm n r = 
    update_assms (Rn.NSet.ins_fresh n)
    o update_rs (Rn.NTab.ins (n,r));

fun rename_in_proof (from_n,to_n) (Proof (used_n,m,optto_n)) =
    Proof (if Rn.name_eq (from_n,used_n) 
           then to_n else used_n, m, 
           case optto_n 
            of NONE => NONE
             | SOME n => if Rn.name_eq (n,optto_n) 
                         then to_n else optto_n);

fun rename_in_result (Result (r,p)) = Result (r, rename_in_proof p);

fun rename n1 n2 thry = 
    get_deps_for n1 thry 

(* Make a new trivial result from a graph. *)
(* : Rn.name -> Rule.RGGraph.T -> T -> (Rn.name * T) *)
fun trivial n g thry = add_trynamed_r (r,Rule.mk (g,g)) thry;

val save_as : Rn.name -> T -> T
val delete : Rn.name -> T -> T
val symmetric : Rn.name -> T -> T


exception no_such_result_exp of Rn.name * T;
val subst : Rn.name -> Rn.name -> T -> (Rn.name * T) Seq.seq


end;

