datatype 'a tree = Nd of 'a * 'a tree list;

structure G = RGGraph;
structure NS = RGVertex.NSet;
val bv = Vertex.BVert ();

fun split set =
    let
	val (SOME fst) = set|>NS.get_first
	val rest = set|>NS.delete fst
    in (fst, rest)
    end

val g = G.empty

local
    val (v1, g) = g|>G.add_vertex bv
    val (v2, g) = g|>G.add_vertex bv
    val (v3, g) = g|>G.add_vertex bv
    val (v4, g) = g|>G.add_vertex bv
    val (v5, g) = g|>G.add_vertex bv
    val (v6, g) = g|>G.add_vertex bv
    val (v7, g) = g|>G.add_vertex bv
    val (v8, g) = g|>G.add_vertex bv
in

val (_,g) = g|>G.add_edge () v1 v2
val (_,g) = g|>G.add_edge () v1 v3
val (_,g) = g|>G.add_edge () v2 v4
val (_,g) = g|>G.add_edge () v3 v5
val (_,g) = g|>G.add_edge () v5 v1

(* a disconnected component *)
val (_,g) = g|>G.add_edge () v6 v7
val (_,g) = g|>G.add_edge () v6 v8
val (_,g) = g|>G.add_edge () v8 v7

end



fun wipe_edges g = UnitEdge.NSet.fold (G.delete_edge) (g|>G.get_enames) g

fun no_op a b = b
local
    fun dft' cmb1 cmb2 g branches prev tr =
	if (NS.is_empty branches) then (g, tr)
	else
	    let
		val (head,tail) = split branches
	    in
		if (G.has_vname g head) then
		    let
			val (g', tr') =
			    dft' cmb1 cmb2
				 (g|>G.delete_vertex head)
				 (G.adj_vnames g head)
				 head tr
		    in
			dft' cmb1 cmb2 g' tail prev (cmb1 (prev,head) tr')
		    end
		else (g, (cmb2 (prev,head) tr))
	    end
in

(* perform a depth-first traversal, folding on cmb1 for paths IN the
   spanning tree and cmb2 for cycle connections *)
fun dft2 cmb1 cmb2 g base =
    case NS.get_first (g|>G.get_vnames)
     of (SOME v) =>
	let
	    val (g',tr) = dft' cmb1 cmb2
			       (g|>G.delete_vertex v)
			       (G.adj_vnames g v)
			       v base
	in dft2 cmb1 cmb2 g' tr
	end
      | NONE => base

(* same, but only care about the spanning tree *)
fun dft cmb g base = dft2 cmb no_op g base

end


fun paths g = dft (curry op::) g []
fun traces g = dft2 no_op (curry op::) g []

fun spanning_tree g =
    dft
	(fn (s,t) => fn tr => #2(tr |> (G.add_edge () s t)))
	g (wipe_edges g)

fun spanning_tree_with_traces g =
    dft2
	(fn (s,t) => fn (tr,lst) => ((#2(tr |> (G.add_edge () s t))),lst))
	(fn (s,t) => fn (tr,lst) => (tr,(s,t)::lst))
	 g (wipe_edges g, [])

local
    fun bld_tree (p,h) [] = [Nd(p,[Nd(h,[])])]
      | bld_tree (p,h) (Nd(r,b)::ts) =
	let
	    val merge =
	     fn ts' =>
		case ts' of (Nd(r1,b1)::Nd(r2,b2)::ts'') =>
			    (if r1=r2 then Nd(r1,b1@b2)::ts''
			     else ts')
			  | _ => ts'
	in
	    merge (
	    if r=h then Nd(p,[Nd(r,b)])::ts
	    else Nd(p,[Nd(h,[])])::Nd(r,b)::ts
	    )
	end
in
fun span2 g = dft bld_tree g []
end

fun term [] = ""
  | term (s::t::ts) = "(" ^ (term [s]) ^ " X " ^ (term [t]) ^ ")"
  | term [Nd(r,[])] = r
  | term [Nd(r,ts)] = "(" ^ r ^ " o " ^ (term ts) ^ ")"




fun list_of 0 x = []
  | list_of n x = x::(list_of (n-1) x)


signature RING =
sig
    type ring
    val zero : ring
    val one : ring
    val is_zero : ring -> bool
    val is_one : ring -> bool
    val + : (ring * ring) -> ring
    val * : (ring * ring) -> ring
    val ~ : ring -> ring
    val compare : ring * ring -> General.order
    val toString : ring -> string
    val pretty : ring -> Pretty.T
    val print : ring -> unit
end

signature FIELD =
sig
    include RING
    type field = ring
    val invert : field -> field
    val / : (field*field) -> field
end

signature ACF =
sig
    include FIELD
    val conj : field -> field
end

signature POLY_RING =
sig
    include RING;
    structure URing : RING;
    val mono : URing.ring * int * URing.ring -> ring;
    val const : URing.ring -> ring;
    val reduce : ring -> ring;
end

signature INDET_NAMER =
sig
    val indet_name : int -> string
end

structure IntRing : RING =
struct
open Int

type ring = int
val zero = 0
val one = 1
fun is_zero 0 = true | is_zero _ = false
fun is_one 1 = true | is_one _ = false
fun pretty i = Pretty.str (toString i)
val print = Pretty.writeln o pretty
end


functor PolyRingFun (
	structure URing : RING
	      and IndetNamer : INDET_NAMER
	) : POLY_RING =
struct

structure URing = URing;
type ring = (URing.ring * URing.ring list) list

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
fun const i = [(i,[]:URing.ring list)]

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


functor MatrixFun (structure UField : ACF) =
struct

structure UField = UField
structure URing = UField
structure MF = ModuleFun(structure URing = URing)
open MF

end


structure PolyRingInt = PolyRingFun(structure URing = IntRing
      and IndetNamer = struct
      fun indet_name i = case i
			  of 0=>"I"| 1=>"(pi)"
			   | 2=>"A"| 3=>"B" | 4=>"C" | 5 => "D"
			   | _=>"X["^(Int.toString i)^"]"
      end)
		
structure ExpRingInt = PolyRingFun(structure URing = PolyRingInt
      and IndetNamer = struct
      fun indet_name i = case i
			  of 0=>"e"
			   | _=>"X["^(Int.toString i)^"]"
      end)


structure PRI = PolyRingInt;
structure ERI = ExpRingInt;

val p1 = PRI.+(PRI.mono(2,0,2),PRI.mono(3,0,3))
val p2 = PRI.+(PRI.mono(3,1,4),PRI.mono(5,1,2))

fun angle exp =
    ERI.mono(PRI.one, 0,
	     PRI.*([(IntRing.one,[1])],exp))


val e1 = ERI.mono(PRI.const 1,0,p1)
val e2 = ERI.mono(PRI.const 4,0,p2)

structure IMatrix = ModuleFun(structure URing = IntRing)
structure PMatrix = ModuleFun(structure URing = PRI)
val ident = [[1,0],[0,1]]
val matr = [[2,3],[4,5]]
val matr2 = [[6,7],[8,9]]

val pmat = [[p1,p2],[PRI.const 1,PRI.const 5]]




