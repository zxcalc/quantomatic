(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* Name  *)
signature NAME
= sig
type T
structure NTab : FNAME_TAB where type name = T;
structure Col : COLLECTION where type obj = T;
val pretty : T -> Pretty.T
val print : T -> unit
val eq : T -> T -> bool
val ord : T * T -> General.order
end;

signature STR_NAME
= sig
include NAME
val mk : string -> T
val dest : T -> string
end;


functor StrNameFun(N : INTERP_NAMER where type src = string) : STR_NAME
= struct
type T = N.name
structure NTab :> FNAME_TAB where type name = T 
  = FNameTabFun(FNamerFun(N));
structure Col :> COLLECTION where type obj = T 
  = CollectionFUN(type key = T val ord = N.ord);
val mk = N.mk;
val dest = N.dest;
val pretty = N.pretty;
val print = Pretty.writeln o pretty;
val ord = N.ord;
fun eq a b = case ord (a, b) of EQUAL => true | _ => false;
end;


structure StrName :> STR_NAME
= struct 
type T = string
structure NTab :> FNAME_TAB where type name = T = StrNameTab;
structure Col :> COLLECTION where type obj = T = StrCollection;
val mk = I;
val dest = I;
val pretty = Pretty.str;
val print = Pretty.writeln o pretty;
val ord = String.compare;
fun eq a b = case (ord (a, b)) of EQUAL => true | _ => false;
end;
