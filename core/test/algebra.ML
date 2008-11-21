

fun list_of 0 x = []
  | list_of n x = x::(list_of (n-1) x)

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

signature FIELD =
sig
    include RING
    val invert : T -> T
    val / : (T*T) -> T

    (* if not a complex field, let conj = id *)
    val conj : T -> T
end

signature POLY_RING =
sig
    include RING;
    structure URing : RING;
    val mono : URing.T * int * URing.T -> T;
    val const : URing.T -> T;
    val reduce : T -> T;
end

signature INDET_NAMER =
sig
    val indet_name : int -> string
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

structure RealField : FIELD =
struct
open Real

type T = real
val zero = 0.0
val one = 1.0
fun invert r = 1.0 / r
fun conj r = r
fun is_zero r = (compare (r,zero))=EQUAL
fun is_one r = (compare (r,one))=EQUAL
fun pretty i = Pretty.str (toString i)
val print = Pretty.writeln o pretty
end


functor PolyRingFun (
	structure URing : RING
	      and IndetNamer : INDET_NAMER
	) : POLY_RING =
struct

structure URing = URing;
type T = (URing.T * URing.T list) list

fun indet_name i = "X[" ^ (Int.toString i) ^ "]"


fun add_powers [] [] = []
  | add_powers [] x = add_powers x []
  | add_powers (p::ps) [] = p::add_powers ps []
  | add_powers (p::ps) (q::qs) = (URing.+(p,q))::add_powers ps qs
fun compare_powers (ps,qs) = List.collate (URing.compare) (ps, qs)
fun compare_monos ((k,ps),(l,qs)) =
    case compare_powers (ps,qs)
     of EQUAL => URing.compare(k,l)
      | ord => ord
			    
fun reduce_powers' [] = []
  | reduce_powers' (p::ps) = if (URing.is_zero p) then reduce_powers' ps
			     else p::ps
fun reduce_powers l = rev(reduce_powers'(rev l))
		      
fun reduce' ((k,ps')::(l,qs')::poly) =
    if URing.is_zero k then reduce' ((l,qs')::poly)
    else
	let
	    val ps = reduce_powers ps'
	    val qs = reduce_powers qs'
	in
	    if compare_powers (ps, qs) = EQUAL then reduce' ((URing.+(k,l),ps)::poly)
	    else (k,ps)::reduce' ((l,qs)::poly)
	end
  | reduce' [(k,ps)] = if URing.is_zero k then [(URing.zero,[])] else [(k,ps)]
  | reduce' poly = poly
fun multiply_monos (k,ps) (l,qs) = (URing.*(k,l), add_powers ps qs)
				   
fun power_string [] _ = ""
  | power_string (p::ps) x =
    (if (URing.is_zero p) then ""
     else
	 (IndetNamer.indet_name x)^
	 (if URing.is_one p then "" else "^"^(URing.toString p))^
	 (case ps of [] => "" | _ => "*"))
    ^(power_string ps (x+1))

fun mono_toString (k,p) = if (URing.is_one k) then power_string p 0
			  else (URing.toString k)^"*"^(power_string p 0)
			  
val minus_one = [(URing.~(URing.one),[])]

fun mono (k,x,p) =
    [if URing.is_zero p then (k,[])
    else (k, (list_of x URing.zero) @ [p])]

fun sort_terms poly = sort compare_monos poly
val reduce = reduce' o sort_terms
fun const i = [(i,[]:URing.T list)]

(* RING functions *)
val zero = [(URing.zero,[])]
val one = [(URing.one,[])]
fun (op+) (p1,p2) = reduce (p1 @ p2)
fun (op*) (p1,p2) = reduce (flat (map (fn x => map (multiply_monos x) p2) p1))
fun (op~) poly = poly * minus_one
fun compare (p1, p2) = List.collate (compare_monos) (p1,p2)
fun is_zero poly = compare (poly, zero) = EQUAL
fun is_one poly = compare (poly, one) = EQUAL


fun toString' [] = ""
  | toString' [m] = mono_toString m
  | toString' (m::ms) = mono_toString m ^ " + " ^ (toString' ms)
fun toString p = "(" ^ (toString' p) ^ ")"
fun pretty poly = Pretty.str (toString poly)
val print = Pretty.writeln o pretty
end


functor ModuleFun (structure URing : RING) =
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
fun merge_x m1 m2 = transpose ((transpose m1) @ (transpose m2))
fun flatten m = fold merge_x [] merge_y [] m
fun sum_of_squares m = fold (fn x => fn s => URing.+(URing.*(x,x),s)) URing.zero (curry URing.+) URing.zero m
fun kronecker_product m1 m2 = flatten (map (fn a => scalar_mult a m2) m1)

fun pretty m = Pretty.chunks (List.map (Pretty.list "[" "]") (map (URing.pretty) m));
val print = Pretty.writeln o pretty

end (* structure Matrix *)


functor MatrixFun (structure UField : FIELD) =
struct

structure UField = UField
structure URing = UField
structure MF = ModuleFun(structure URing = URing)
open MF

fun adjoint m = map (UField.conj) (transpose m)

end
