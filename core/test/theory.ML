(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Theory with Fixed Logical Kernel style *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
signature THEORY 
= sig

structure Rule : RULE
structure Result : NAMES

type T

val empty : T

val assume : Result.name -> Rule.T -> T -> T

exception no_such_result_exp of Result.name * T;

val get_assumptions : T -> Rule.T Symtab.T
val get_assumption : T -> Result.name -> Rule.T

val trivial : Result.name -> Rule.RGGraph.T -> T -> (Result.name * T)
val symmetric : Result.name -> T -> (Result.name * T)
val subst : Result.name -> Result.name -> T -> (Result.name * T) Seq.seq



end;


