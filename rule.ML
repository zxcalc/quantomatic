(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* rules for rewriting graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
signature RULE 
= sig

structure PatGraph : PATTERN_GRAPH

type T (* rules *)

val mk : string * (PatGraph.T * PatGraph.T) -> T;

val get_lhs : T -> PatGraph.T
val get_rhs : T -> PatGraph.T

val get_name : T -> string;
val set_name : string -> T -> T;

val get_rhs_newnodes : T -> NName.NTab.N 
val get_lhs_delnodes : T -> NName.NTab.N

val get_rhs_newedges : T -> EName.NTab.N
val get_lhs_deledges : T -> EName.NTab.N

(* swaps lhs and rhs *)
val sym : string (* new name *)
          -> T -> T


val empty_match : PatGraph.match

(* Note: implicitly dependent types: match must be a mapping between
names in lhs(T) and those in the given (graph or rhs(T)) *)
val match1 : T  (* use this rule *)
             -> PatGraph.T  (* to match something in this graph *)
             -> PatGraph.match (* starting with this initial matching *)
             -> PatGraph.match (* this is the first found resulting match *)

(* find all matches, evaluated lazily *)
val matches : T -> PatGraph.T -> PatGraph.match -> PatGraph.match Seq.seq

val do_rewrite : (* rewriting pattern graph *)
    T -> PatGraph.match -> PatGraph.T -> PatGraph.T
val do_rewrite_rrhs : (* rewriting a rules rhs *)
    T (* use this rule *)
    -> PatGraph.match (* with this matching *)
    -> T (* to rewrite this *)
    -> T (* to this *)

val rewritings : T -> (PatGraph.match * PatGraph.T) 
								 -> (PatGraph.match * PatGraph.T) Seq.seq
val rewritings_rrhs : T -> (PatGraph.match * T) -> (PatGraph.match * T) Seq.seq

end;


functor Rule(PatGraph : PATTERN_GRAPH)
= struct

structure PatGraph = PatGraph;

datatype T = Rule of { name : string,
                       lhs : PatGraph.T,
                       rhs : PatGraph.T };

fun mk (n,(lhs,rhs)) = 
    Rule { name = n, lhs = lhs, rhs = rhs };

(* match anything = no set matching *)
val any_match =
    

fun get_lhs (Rule rep) = #lhs rep;
fun get_rhs (Rule rep) = #rhs rep;
fun get_name (Rule rep) = #name rep;

fun update_lhs f (Rule rep) = 
    Rule { name = #name rep, lhs = f (#lhs rep), rhs = #rhs rep };
fun update_rhs f (Rule rep) = 
    Rule { name = #name rep, lhs = #lhs rep, rhs = f (#rhs rep) };
fun update_name f (Rule rep) = 
    Rule { name = f (#name rep), lhs = #lhs rep, rhs = #rhs rep };

val set_lhs = update_lhs o K;
val set_rhs = update_rhs o K;
val set_name = update_name o K;

fun get_nnames r = 
    ((get_lhs r) 
      |> PatGraph.get_nodes 
      |> NName.NTab.get_names,
     (get_rhs r) 
       |> PatGraph.get_nodes 
       |> NName.NTab.get_names);

fun get_enames r =
   ((get_lhs r) 
      |> PatGraph.get_edges 
      |> EName.NTab.get_names,
    (get_rhs r) 
      |> PatGraph.get_edges 
      |> eName.NTab.get_names);

(* THINK: cache this info? *)
fun get_rhs_newnodes r = 
    let val (lhsns,rhsns) = get_nnames r
    in NName.NTab.N.subtract rhsns lhsns end;
fun get_lhs_delnodes r =
    let val (lhsns,rhsns) = get_nnames r
    in NName.NTab.N.subtract lhsns rhsns  end;
fun get_rhs_newedges r =
    let val (lhsns,rhsns) = get_enames r
    in EName.NTab.N.subtract rhsns lhsns end;
fun get_lhs_deledges r =
    let val (lhsns,rhsns) = get_enames r
    in EName.NTab.N.subtract lhsns rhsns  end;


fun sym n r = mk (n,(get_rhs r, get_lhs r));


val do_rewrite : (* rewriting pattern graph *)
    T -> match -> PatGraph.T -> PatGraph.T
val do_rewrite_rrhs : (* rewriting a rules rhs *)
    T (* use this rule *)
    -> match (* with this matching *)
    -> T (* to rewrite this *)
    -> T (* to this *)

val rewritings : T -> (match * PatGraph.T) -> (match * PatGraph.T) Seq.seq
val rewritings_rrhs : T -> (match * T) -> (match * T) Seq.seq



(* find all matches, evaluated lazily *)
val matches : T -> PatGraph.T -> match -> match Seq.seq


val match1 : T  (* use this rule *)
             -> PatGraph.T  (* to match something in this graph *)
             -> match (* starting with this initial matching *)
             -> match (* this is the first found resulting match *)


end;
