structure Permutation =
struct
type T = int list
(* recover the permutation from two lists of ordered elements *)
fun get_perm ord lst1 lst2 =
    map (fn a => find_index 
		     (fn b => (ord (a, b)) = EQUAL) lst2)
	lst1

(* invert the given permutation *)
fun invert_perm perm = get_perm Int.compare (0 upto ((length perm)-1)) perm

(* apply a permutation to a list defined by
 *  [0,1,2,3,...] |-> perm  *)
fun apply_perm perm lst = map (nth lst) (invert_perm perm)

fun id size = (0 upto (size-1))
fun is_id perm = perm = (id (length perm))

(* The above functions have the property:
 *   apply_perm (get_perm ord lst1 lst2) lst1 = lst2 *)


end


signature RING =
sig
    type T
    val zero : T
    val one : T
    val is_zero : T -> bool
    val is_one : T -> bool
    val + : (T * T) -> T
    val * : (T * T) -> T
    val ~ : T -> T
    val ord : T * T -> General.order
    val toString : T -> string
    val pretty : T -> Pretty.T
    val print : T -> unit
end

signature MATRIX =
sig
    structure URing : RING
    type 'a vector = 'a list
    type 'a matrix = 'a list list
    type T = URing.T matrix
    
    (* functions that work for any matrix *)
    val map : ('a -> 'b) -> 'a matrix -> 'b matrix
    val fold :
        ('a -> 'b -> 'b) -> 'b ->
        ('b -> 'c -> 'c) -> 'c ->
	'a matrix -> 'c
    val transpose : 'a matrix -> 'a matrix
    val flatten : 'a matrix matrix -> 'a matrix
    val singleton : 'a -> 'a matrix

    (* functions for matrices of ring elements *)
    val dot_product : URing.T vector -> URing.T vector -> URing.T
    val kronecker_product : T -> T -> T
    val mult : T -> T -> T
    val pretty : T -> Pretty.T
    val print : T -> unit
    val scalar_mult : URing.T -> T -> T
    val sum_of_squares : T -> URing.T
    val tensor : T list -> T
    val permute_tensor : int list -> int -> T

    val ident : int -> T
end

structure IntRing : RING =
struct
open Int

type T = int
val zero = 0
val one = 1
fun is_zero 0 = true | is_zero _ = false
fun is_one 1 = true | is_one _ = false
fun pretty i = Pretty.str (toString i)
val print = Pretty.writeln o pretty
val ord = compare
end

functor PolyRingFun (
  structure ExpRing : RING
  structure CoeffRing : RING) =
struct


type T = (CoeffRing.T * ExpRing.T StrName.NTab.T) list

val keys = StrName.NTab.get_nameset

fun const k = [(k, StrName.NTab.empty)]
fun monomial (v,p) =
    [(p, StrName.NTab.empty |> StrName.NTab.update (v, ExpRing.one))]

fun var v = monomial (v, CoeffRing.one)

(* compare two tables of exponents *)
fun var_ord ((_,e1),(_,e2)) = list_ord (prod_ord StrName.name_ord ExpRing.ord)
				       (StrName.NTab.list_of e1,
					StrName.NTab.list_of e2)

fun collapse_t (c,e) = let
    fun fld (k,v) = if ExpRing.is_zero v
		    then StrName.NTab.delete k
		    else I
in (c, StrName.NTab.fold fld e e)
end

(* merge variable tables using f *)
fun merge_vars t1 t2 = let
    fun merge_add NONE (SOME v) = v
      | merge_add (SOME v) NONE = v
      | merge_add (SOME v1) (SOME v2) = ExpRing.+(v1,v2)
      | merge_add _ _ = raise Match
    val allkeys = StrName.NSet.union_merge (keys t1) (keys t2)
    fun fld k = StrName.NTab.update
		    (k, merge_add (StrName.NTab.lookup t1 k)
				  (StrName.NTab.lookup t2 k))
in StrName.NSet.fold fld allkeys StrName.NTab.empty
end


val simplify = let
    fun merge (t1::t2::ts) = if var_ord (t1,t2) = EQUAL
			     then (CoeffRing.+(fst t1, fst t2), snd t1)::merge ts
			     else t1::t2::merge ts
      | merge rem = rem
in filter_out (CoeffRing.is_zero o fst) o merge o sort var_ord
end

fun op + (p1, p2) = simplify (p1 @ p2)

fun op * (p1, p2) =
     let fun mult_terms (c1,e1) (c2,e2) = (CoeffRing.*(c1,c2), merge_vars e1 e2)
     in map_product mult_terms p1 p2
     end

fun pretty_term (coeff,exp) =
    Pretty.block (CoeffRing.pretty coeff ::
		  (StrName.NTab.fold
		       (fn (k,v) =>
			   append [Pretty.str "*",
				   Pretty.str k,
				   Pretty.str "^",
				   ExpRing.pretty v])
		       exp []))
fun pretty poly = Pretty.block (Pretty.separate " +" (map pretty_term poly))
val print = Pretty.writeln o pretty

end

structure IntPoly =
struct
structure PR = PolyRingFun(
  structure CoeffRing = IntRing
  structure ExpRing = IntRing)

open PR
open SimpleLexer


fun factor x = 
    x |> (
	  (ident --| sym "^" -- num >> (fn (ID id, INT p) => monomial(id,p)
                                   | _ => raise ERROR "alg.ML:factor:1"))
		  || (ident >> (fn (ID id) => var id
                     | _ => raise ERROR "alg.ML:factor:2"))
		  || (num >> (fn (INT n) => const n
                   | _ => raise ERROR "alg.ML:factor:3")))
(*
fun term x = x|>((factor --| sym "*" -- term >> op* ) || factor)
fun expr x = x|>(
	     (term --| sym "+" -- expr >> op+)
		 || term)

*)

end




functor MatrixFun (structure URing : RING) : MATRIX  =
struct

structure URing = URing
type 'a vector = 'a list
type 'a matrix = 'a list list
type T = URing.T matrix

fun dot_product v1 v2 = fold2 (fn x => fn y => fn sum => (URing.+(URing.*(x,y),sum))) v1 v2 URing.zero
    
val singleton = fn x => [[x]]

local
    fun split_col [] h t = (rev h,rev t)
      | split_col ([]::rows) _ _ = ([],[])
      | split_col ((e::cols)::rows) h t = split_col rows (e::h) (cols::t)
					  
    fun mult' [] _ = []
      | mult' (r::rs) cs = let
	    val mrow = fold (fn c => fn row => (dot_product r c)::row) cs [] 
	in (mrow)::(mult' rs cs)
	end
in
fun transpose m = case (split_col m [] [])
		   of ([],[]) => []
		    | (col,rest) => col::(transpose rest)
				    
fun mult m1 m2 = mult' m1 (transpose m2)
end (* local *)
fun map f m = List.map (fn row => List.map f row) m
fun scalar_mult k m = map (fn e => URing.*(k,e)) m
fun fold cmb_x base_x cmb_y base_y m =
    Basics.fold cmb_y (List.map (fn row => Basics.fold cmb_x row base_x) m) base_y

fun flatten m = let
    fun merge_rows m1 [] = m1
      | merge_rows m1 m2 = map2 (fn r1 => fn r2 => r2 @ r1 (* for a left fold *))
				m1 m2
    fun merge_cols m1 m2 = transpose (merge_rows (transpose m1) (transpose m2))
in fold merge_rows [] merge_cols [] m
end

fun sum_of_squares m = fold (fn x => fn s => URing.+(URing.*(x,x),s)) URing.zero (curry URing.+) URing.zero m
fun kronecker_product m1 m2 = flatten (map (fn a => scalar_mult a m1) m2)
fun tensor lst = Basics.fold kronecker_product lst [[URing.one]]
fun ident dim =
    let fun row n = List.map (fn a => if a=n then URing.one else URing.zero)
			     (0 upto (dim-1))
    in List.map row (0 upto (dim-1))
    end

(* cartesion product of lists, pairing elems with f *)
fun cart f lst1 lst2 = 
    Library.flat (List.map (fn a => List.map (fn b => f a b) lst2) lst1)

fun tensor_basis 0 dim = [[]]
  | tensor_basis size dim = let
	val smaller = tensor_basis (size-1) dim
    in cart (curry op::) (0 upto (dim-1)) smaller
    end

(* apply the given permutation of tensors in a computation basis of 
 * dimension dim. *)
fun permute_tensor perm dim =
    let
	    val basis = ident dim
	    val tbasis = tensor_basis (length perm) dim
	    val permuted_tbasis =
	        map ((fn x=>[x]) o nth basis)
		          (List.map (Permutation.apply_perm perm) tbasis)
    in
	    transpose (List.map ((fn [x] => x
                             | _ => raise ERROR "alg.ML:permute_tensor:1") 
                           o tensor) permuted_tbasis)
    end;

fun pretty m = Pretty.chunks (List.map (Pretty.list "[" "]") (map (URing.pretty) m));
val print = Pretty.writeln o pretty

end (* structure MatrixFun *)

structure IntMatrix = MatrixFun(structure URing = IntRing)
