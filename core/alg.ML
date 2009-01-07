structure Permutation =
struct
type T = int list
(* recover the permutation from two lists of ordered elements *)
fun get_perm ord lst1 lst2 =
    map (fn a => find_index 
		     (fn b => (ord (a, b)) = EQUAL) lst1)
	lst2

(* invert the given permutation *)
fun invert_perm perm = get_perm Int.compare perm (0 upto ((length perm)-1)) 

(* apply a permutation to a list *)
fun apply_perm perm lst = map (nth lst) perm

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
    val compare : T * T -> General.order
    val toString : T -> string
    val pretty : T -> Pretty.T
    val print : T -> unit
end

signature MATRIX =
sig
    structure URing : RING
    type T = URing.T list list
    val dot_product : URing.T list -> URing.T list -> URing.T
    val flatten : T list list -> T
    val fold :
        (URing.T -> 'a -> 'a) -> 'a ->
        ('a -> 'b -> 'b) -> 'b ->
	T -> 'b
    (*val ident : 'a -> int*)
    val kronecker_product : T -> T -> T
    val map : (URing.T -> 'a) -> T -> 'a list list
    val merge_x : 'a list list -> 'a list list -> 'a list list
    val merge_y : 'a list -> 'a list -> 'a list
    val mult : T -> T -> T
    val pretty : T -> Pretty.T
    val print : T -> unit
    val scalar_mult : URing.T -> T -> T
    val sum_of_squares : T -> URing.T
    val tensor : T list -> T
    val transpose : T -> T
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
end

functor MatrixFun (structure URing : RING) =
struct

structure URing = URing
type T = URing.T list list

fun dot_product v1 v2 = fold2 (fn x => fn y => fn sum => (URing.+(URing.*(x,y),sum))) v1 v2 URing.zero
    
local
    fun split_col [] h t = (rev h,rev t)
      | split_col ([]::rows) _ _ = ([],[])
      | split_col ((e::cols)::rows) h t = split_col rows (e::h) (cols::t)
					  
    fun mult' [] _ = []
      | mult' (r::rs) cs = let
	    val mrow = foldr (fn (c,row) => (dot_product r c)::row) [] cs
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
fun merge_y m1 m2 = m1 @ m2
fun merge_x m1 m2 = transpose (merge_y (transpose m1) (transpose m2))
fun flatten m = fold merge_x [] merge_y [] m
fun sum_of_squares m = fold (fn x => fn s => URing.+(URing.*(x,x),s)) URing.zero (curry URing.+) URing.zero m
fun kronecker_product m1 m2 = flatten (map (fn a => scalar_mult a m2) m1)
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

fun permute_tensor perm dim =
    let
	val basis = ident dim
	val tbasis = tensor_basis (length perm) dim
	val permuted_tbasis =
	    map ((fn x=>[x]) o nth basis)
		(List.map (Permutation.apply_perm perm) tbasis)
    in
	permuted_tbasis
    end

fun pretty m = Pretty.chunks (List.map (Pretty.list "[" "]") (map (URing.pretty) m));
val print = Pretty.writeln o pretty

end (* structure MatrixFun *)

structure IntModule = MatrixFun(structure URing = IntRing)
