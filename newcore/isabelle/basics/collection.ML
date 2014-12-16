(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      collection.ML
    Author:     Lucas Dixon, University of Edinburgh
                lucas.dixon@ed.ac.uk
    Updated:    21 Aug 2007
    Date:       12 May 2006
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Collections of objects (with an ordering)                          *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* IMPROVE: direct implementation rather than using Symtab *)

signature COLLECTION =
sig

type T
type obj 

exception DUP of obj
exception UNDEF of obj

val empty : T
val is_empty : T -> bool

val addnew : obj -> T -> T (* raises DUP exception *)
val union1 : obj -> T -> T

val delete : obj -> T -> T (* raises UNDEF exp *)
val remove : obj -> T -> T 

val of_list : obj list -> T
val list_of : T -> obj list

val minus_set : T -> T -> T
val union_set : T -> T -> T
val intersect_set : T -> T -> T

val map : (obj -> obj) -> T -> T
val fold : (obj -> 'a -> 'a) -> T -> 'a -> 'a
val filter : (obj -> bool) -> T -> T

val contains : T -> obj -> bool

(* internal order *)
val ord : obj * obj -> order

(* *)
val unionfst_f : ('a -> (obj * 'b)) -> (T * 'a) -> (T * 'b)

(* pretty *)
val pretty : (obj -> Pretty.T) -> T -> Pretty.T;
(* val direct_print : T -> unit; *)

end;

(* IMPROVE: allow to take table as input - avoid creating many tablefun's *)
functor CollectionFUN(Key : KEY) : COLLECTION = 
struct 

type obj = Key.key;

val ord = Key.ord;
structure Tab = Table(type key = Key.key val ord = Key.ord);

exception DUP = Tab.DUP;
exception UNDEF = Tab.UNDEF;

type T = unit Tab.table;

val empty = Tab.empty;
val is_empty = Tab.is_empty;

fun addnew x = Tab.update_new (x,());
fun union1 x = Tab.update (x,());

val delete = Tab.delete; (* raises UNDEF exp *)
val remove = Tab.delete_safe;

fun contains tb x = 
    case Tab.lookup tb x of NONE => false | SOME _ => true;

fun filter f tb = 
    Tab.fold (fn (x,()) => if f x then I else delete x) tb tb;

fun of_list l = fold union1 l empty;
val list_of = Tab.keys;

fun minus_set tb mtb =
    Tab.fold (Tab.delete_safe o fst) mtb tb;

val union_set : T -> T -> T = Tab.fold (union1 o fst);

fun unionfst_f f (tb, x) = 
    let val (a,x2) = (f x) in (union1 a tb, x2) end;

fun intersect_set tb1 tb2 = 
    Tab.fold (fn (x,()) => fn tb => 
              if contains tb2 x then addnew x tb
              else tb)
             tb1 empty;

fun pretty pf c = Pretty.list "[" "]" (map pf (list_of c));
(* val direct_print = Pretty.writeln o pretty (Pretty.str o PolyML.makestring); *)

fun fold f = Tab.fold (f o fst);
fun map f t = fold (union1 o f) t empty;

end;


structure StrCollection :> COLLECTION where type obj = string = 
CollectionFUN(type key = string val ord = String.compare)
