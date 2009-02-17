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

(* rules *)
structure Rule : RULE = Rule

(* names of rules *)
structure Rn = StrName;

(* dependencies: Dom = result name used in Cod result *) 
structure Dep = BinRelFun(structure Dom = Rn and Cod = Rn); 

(* proofs for results/derived rules *)
datatype proof = 
         Proof of (Rn.name (* applied the rule named this *)
                   * Rn.name option (* resulting in this subgoal *)
                   * bool (* true = applied left to right, false = right to left *)
                   * Match.T (* as instantiated here *)
                  )
       | Refl;

(* a derived rusult *)
datatype result = Result of Rule.T (* final resulting rule/conjecture *)
                            * proof option; (* proof *)

(* a theory *)
datatype T = Theory of { assms: Rn.NSet.T, (* names of axioms of the theory *)
                         topns: Rn.NSet.T, (* names of interesting results *)
                         rs : result Rn.NTab.T,  (* all results in the theory *)
                         deps : Dep.T (* cached dependencies: 
                                         dom = result name; cod = names of things that use it *)
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
val get_r_deps = Dep.domf o get_deps;
(* implicit: n nset thry *)
val add_r_deps = update_deps oo Dep.add_to_dom;
(* implicit: n n2 thry, where n2 is uses n1 *)
val add_r_dep = update_deps oo Dep.add1_to_dom;

(* implicit: n f *)
val update_result = update_rs oo Rn.NTab.map_entry;
val set_result = update_result o K;

(* getting all assumptions *)
val get_assm = RnTab.get o get_assms;
val get_assms_list = RnTab.list_of o get_assms;

(* new empty theory *)
val empty = Theory { assms = Rn.NSet.empty,
                     topns = Rn.NSet.empty,
                     rs = Rn.NTab.empty,
                     deps = Rn.NTab.empty
                   };

(* does not note the names result *)
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
    thry |> (NSet.fold 
               (update_result (rename_in_result rn))
               (get_r_deps n1 thry))
         |> update_deps (Dep.rename_dom n1 n2 o Dep.rename_cod n1 n2)
         |> update_topns (Rn.NSet.rename n1 n2)
         |> update_assms (Rn.NSet.rename n1 n2);

(* for tagging top-level results you might want to refer to later *)
(* implicit: n thry *)
val note_result = update_topns o Rn.NSet.add;
val unnote_result  = update_topns o Rn.NSet.del;

(* Make a new trivial result from a graph. *)
fun trivial n g thry = add_named_r (n,(Result(Rule.mk (g,g), SOME Refl))) thry;
(* conjecture a new result, is unproved *)
fun conjecture n r thry = add_named_r (r,Result(r,NONE)) thry;

(* implicit n thry *)
(* ? maybe we sould raise an exception if result is currently closed *)
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
         |> update_deps (Dep.del_dom n);


(* converts between "A = B" and "B = A" *)
fun symmetric n thry = 
    let val Result (r,p) = get_result thry n
        val rhs = Rule.get_rhs r
        val m = Match.mk_id (get_boundary rhs) 
                            (RGGraph.get_vnames rhs)
                            (RGGraph.get_enames rhs);
    in case p of 
         NONE => let val (n2,thry2) = conjecture n (Rule.symmetric r) thry;
                     val res = Result (r, SOME (n2,NONE,true,m))
                 in (n2, thry2 |> add_r_dep n n2 
                               |> set_result n res)
                 end;
       | SOME prf =>
         let val (n2,thry2) = 
                 add_named_r n (Result (Rule.symmetric r, SOME (n,NONE,true,m))) thry
         in thry |> add_r_dep n2 n end
    end;


exception no_such_result_exp of Rn.name * T;

(* Subst using first name, from left to right, in rhs of second name *)
(* val subst : Rn.name -> Rn.name -> T -> (Rn.name * T) Seq.seq *)
fun subst rulen n thry = 
    let 
      val Result (rule,_) = get_result thry rulen
      val Result (r,prf) = get_result thry n
      val rhs = Rule.get_rhs 
    in 
      case Rule.rule_matches_within
           
    end;


end;

