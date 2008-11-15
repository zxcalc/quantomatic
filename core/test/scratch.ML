
datatype 'a tree = Nd of 'a * 'a tree list;

structure G = RGGraph;
structure NS = RGVertex.NSet;
val bv = Vertex.BVert ();

fun split set =
    let
	val (SOME fst) = set|>NS.get_first
	val rest = set|>NS.delete fst
    in (fst, rest)
    end;

val g = G.empty;

local
    val (v1, g) = g|>G.add_vertex bv;
    val (v2, g) = g|>G.add_vertex bv;
    val (v3, g) = g|>G.add_vertex bv;
    val (v4, g) = g|>G.add_vertex bv;
    val (v5, g) = g|>G.add_vertex bv;
    val (v6, g) = g|>G.add_vertex bv;
    val (v7, g) = g|>G.add_vertex bv;
    val (v8, g) = g|>G.add_vertex bv;
in
val (_,g) = g|>G.add_edge () v1 v2;
val (_,g) = g|>G.add_edge () v1 v3;
val (_,g) = g|>G.add_edge () v2 v4;
val (_,g) = g|>G.add_edge () v3 v5;
val (_,g) = g|>G.add_edge () v5 v1;
end;

(* a disconnected component *)
val (_,g) = g|>G.add_edge () v6 v7;
val (_,g) = g|>G.add_edge () v6 v8;
val (_,g) = g|>G.add_edge () v8 v7;


fun wipe_edges g = UnitEdge.NSet.fold (G.delete_edge) (g|>G.get_enames) g;

fun no_op a b = b;
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

end;


fun paths g = dft (curry op::) g [];
fun traces g = dft2 no_op (curry op::) g [];

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
end;

fun term [] = ""
  | term (s::t::ts) = "(" ^ (term [s]) ^ " X " ^ (term [t]) ^ ")"
  | term [Nd(r,[])] = r
  | term [Nd(r,ts)] = "(" ^ r ^ " o " ^ (term ts) ^ ")";


