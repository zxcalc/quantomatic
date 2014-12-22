(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      namer.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                Initially written Oct 2005, Last Updated 3 Jan 2009    *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

    Namer: basic concept of a name: ordered object with the ability to 
    construct new names that are the Suc of old ones. 

    properties: 
     suc and pred inverse
     ord (pred x, x) == less (optional for bottom element)
     ord (suc x, x) == more

     pred can have a lower bound, such that (pred x = x) but suc
     cannot have an upper bound.


   note: so far I've only used NAMER, not (SNAMER and IDEAL_NAMER)
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature NAMER
= sig
  type name 
  val suc : name -> name
  val pred : name -> name option
  val ord : name * name -> order
  val eq : name * name -> bool
  val pretty : name -> Pretty.T
end;

signature PRETTY_ORD
= sig
  type name
  val ord : name * name -> order
  val pretty : name -> Pretty.T 
end;

(* a functor to make a an opaque namer from a src namer type, this is
   really just for name hidding. *)
functor AddDataNamerFun(
structure N : NAMER 
type data
val ord_data : data * data -> order
val pretty : data * N.name -> Pretty.T
) : NAMER
= struct
type name = data * N.name;
fun suc (d,n) = (d, N.suc n);
fun pred (d,n) = 
    (case N.pred n of NONE => NONE | SOME n' => SOME (d, n'));
(* order must be this way around! Else break Suc operation on names, 
   which breaks name tables. *)
val ord = Library.prod_ord ord_data N.ord;
fun eq n1n2 = (case ord n1n2 of EQUAL => true | _ => false);
val pretty = pretty;
end;


(* a (safe) namer that has names made from a src type assumes: (mk (dest n) = n) *)
signature SNAMER
= sig
type src 
include NAMER
val mk : src -> name
val dest : name -> src
end;

(* a functor to make a an opaque namer from a src namer type, this is
   really just for name hidding. *)
functor SNamerFun(N : NAMER) : SNAMER
= struct
type src = N.name;
datatype name = Name of src;
fun dest (Name n) = n;
val mk = Name;
fun suc (Name n) = Name (N.suc n);
fun pred (Name n) = (case N.pred n of NONE => NONE | SOME n2 => SOME (Name n2));
fun ord (Name n1, Name n2) = N.ord (n1,n2);
fun eq (Name n1, Name n2) = N.eq (n1,n2);
fun pretty (Name n) = N.pretty n;
end;

(* names that have ideals - what they would ideally like to be, for nicer renaming *)
signature IDEAL_NAMER
= sig
structure Src : NAMER
include NAMER
val mk : Src.name (* ideal *)
				 * Src.name (* actual *)
				 -> name
val get_ideal : name -> Src.name (* each name has an ideal name it wants to be *)
val get_actual : name -> Src.name (* each name has an ideal name it wants to be *)
end;

(* a functor to make an ideal namer from a src namer type, this is
   really just for name hidding. It allows us to try to make every
   name closer to the idea, for example, when used name is removed, we
   might be able to rename later names. *)
functor IdealNamerFun(N : NAMER) : IDEAL_NAMER
= struct
structure Src = N;
datatype name = Name of N.name (* ideal *)
												* N.name; (* actual *)


val mk = Name;
fun suc (Name (i,n)) = Name (i,N.suc n);
fun pred (Name (i,n)) = (case N.pred n of NONE => NONE 
																				| SOME n2 => SOME (Name (i,n2)));
(* FIXME: is prod order right??? *)
fun ord (Name ns1, Name ns2) = Library.prod_ord N.ord N.ord (ns1, ns2);
fun eq n1n2 = (case ord n1n2 of EQUAL => true | _ => false);
fun pretty (Name (_,n)) = N.pretty n;
fun get_ideal (Name (i,_)) = i;
fun get_actual (Name (_,n)) = n;
end;

