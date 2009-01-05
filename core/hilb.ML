structure G = RGGraph;
structure V = RGVertex;
structure E = UnitEdge;
val bv = V.BVert ();
val rv = V.IVert (V.Xnd AngleExpr.zero)
val gv =  V.IVert (V.Znd AngleExpr.zero)


fun split set =
    let
	val fst = the (set|>V.NSet.get_first)
	val rest = set|>V.NSet.delete fst
    in (fst, rest)
    end

val g = G.empty

local
    val (v1, g) = g|>G.add_vertex rv
    val (v2, g) = g|>G.add_vertex (V.IVert (V.Znd (AngleExpr.parse "4*x")))
    val (v3, g) = g|>G.add_vertex gv
    val (v4, g) = g|>G.add_vertex bv
    val (v5, g) = g|>G.add_vertex gv
    val (v6, g) = g|>G.add_vertex rv
    val (v7, g) = g|>G.add_vertex gv
in

val (_,g) = g|>G.add_edge () v1 v2
val (_,g) = g|>G.add_edge () v1 v3
val (_,g) = g|>G.add_edge () v2 v5
val (_,g) = g|>G.add_edge () v2 v5
val (_,g) = g|>G.add_edge () v3 v4
val (_,g) = g|>G.add_edge () v3 v1

(* a disconnected component *)
val (_,g) = g|>G.add_edge () v5 v6
val (_,g) = g|>G.add_edge () v5 v7
val (_,g) = g|>G.add_edge () v7 v6

val g2 = G.empty

local
    val (v1, g2) = g2|>G.add_vertex (V.IVert (V.Xnd (AngleExpr.parse "1*z")))
    val (v2, g2) = g2|>G.add_vertex (V.IVert (V.Znd (AngleExpr.parse "2*y")))
in
val (_,g2) = g2|>G.add_edge () v1 v2
end

end

fun wipe_edges g = E.NSet.fold (G.delete_edge) (g|>G.get_enames) g


signature GRAPH_ADAPTER =
sig
    structure Graph : GRAPH
    datatype term =
	     V of (string * string list) |
	     BV of Graph.Vertex.name |
	     Comp of term list |
	     Tens of term list

    (* translate a vertex into its representation as a term *)
    val translate_vertex : Graph.T -> Graph.Vertex.name -> term

    (* trace out the given edge from a graph and add the new bvertex names to
     * the trace list *)
    val trace_out : Graph.Edge.name ->
		    (Graph.T * (Graph.Vertex.name * Graph.Vertex.name) list) ->
		    (Graph.T * (Graph.Vertex.name * Graph.Vertex.name) list)

    (* compose the list of terms, given in graph order, i.e. [f,g] means g o f *)
    val compose : term list -> term

    (* tensor the given term list, where []->1, [t]->t *)
    val tensor : term list -> term

    val id_vert : term
    val one_vert : term
    val bell_vert : term
    val permutation_op : int list -> term
end

(* Generate a Hilbert space term from an arbitrary directed graph *)
functor HilbertTermFun(structure GraphAdapter : GRAPH_ADAPTER) =
struct

structure GraphAdapter = GraphAdapter
open GraphAdapter

structure Vertex = Graph.Vertex
structure Edge = Graph.Edge

fun list_of_successors graph v = let
    val out_edges = Edge.NSet.list_of (#2(#2(Graph.get_vertex graph v)))
    val targ = fn e => #2(#2(Graph.get_edge graph e))
in map targ out_edges
end

fun locate_bv (Comp lst) = locate_bv (List.last lst)
  | locate_bv (Tens lst) = fold (curry op@) (map locate_bv lst) []
  | locate_bv (V _) = []
  | locate_bv (BV a) = [a]


fun get_output_perm trlist term =
    let
	val bvs = locate_bv term
	val trset = Graph.Vertex.NSet.of_list trlist
	val real_bvs = filter_out (Graph.Vertex.NSet.contains trset) bvs
	val bvs' = trlist @ (sort Graph.Vertex.name_ord real_bvs)
    in Permutation.get_perm Graph.Vertex.name_ord bvs bvs'
    end

(* forest MUST be acyclic, or this will not terminate *)
fun from_tree forest root =
    case list_of_successors forest root
     of [] => translate_vertex forest root
      | cs => compose [translate_vertex forest root,
		       tensor (map (from_tree forest) cs)]

fun from_graph graph = let
    val (bad_edges,roots) =
	Graph.dft_with_roots (Edge.NSet.delete) graph (Graph.get_enames graph)
    val (term_graph,traces) = Edge.NSet.fold trace_out bad_edges (graph, [])
    val term = tensor (map (from_tree term_graph) (Vertex.NSet.list_of roots))
    val trlist = foldr (fn ((a,b),lst) => a::b::lst) [] traces
    val perm = get_output_perm trlist term
    val nbell = length traces
    val nid = (length perm) - (2 * nbell)
    val bell_states = if nbell = 0 then []
		      else [Tens(
			    (replicate nbell bell_vert)@
			    (replicate nid id_vert))]
in compose (term::(permutation_op perm)::bell_states)
end


(* multiple output formats *)
fun pretty_mathematica (V (f, [])) = Pretty.str f
  | pretty_mathematica (V (f, args)) = Pretty.block[Pretty.str f,
					   Pretty.str_list "[" "]" args]
  | pretty_mathematica (BV n) = pretty_mathematica id_vert
  | pretty_mathematica (Comp lst) = Pretty.block
					([Pretty.str "("]@
					 (Pretty.separate
					      " ."
					      (map pretty_mathematica (rev lst)))@
					 [Pretty.str ")"])
  | pretty_mathematica (Tens lst) = Pretty.block[
				    Pretty.str "T",
				    Pretty.list
					"[" "]" (map pretty_mathematica lst)]
				    

fun pretty_text (V (f, [])) = Pretty.str f
  | pretty_text (V (f, args)) = Pretty.block[Pretty.str f,
					   Pretty.str_list "[" "]" args]
  | pretty_text (BV _) = pretty_text id_vert
  (* Pretty.enclose "<" ">" [Vertex.pretty_name n] *)
  | pretty_text (Comp lst) = Pretty.block
				 ([Pretty.str "("]@
				  (Pretty.separate
				       " o" (map pretty_text (rev lst)))@
				  [Pretty.str ")"])
  | pretty_text (Tens lst) = Pretty.block
				 ([Pretty.str "("]@
				  (Pretty.separate
				       " X" (map pretty_text lst))@
				  [Pretty.str ")"])

val print = Pretty.writeln o pretty_text
			     
end

structure RGHilbAdapter  : GRAPH_ADAPTER =
struct

structure Graph = RGGraph
datatype term =
	 V of (string * string list) |
	 BV of Graph.Vertex.name |
	 Comp of term list |
	 Tens of term list

val bv = RGVertex.BVert ();

val id_vert = V ("id",[])
val one_vert = V ("1",[])
val bell_vert = V ("bell",[])
fun permutation_op perm =
    case perm
     of [] => id_vert
      | _ => if perm = (0 upto ((length perm)-1)) then id_vert
	     else V ("sigma", (map Int.toString perm))


(* drop the tensor if we don't need it, empty tensor is the unit *)
fun tensor [] = one_vert
  | tensor [x] = x
  | tensor lst = Tens lst

(* compose and do some easy simplification *)
fun compose [] = id_vert
  | compose [x] = x
  | compose lst = let
	val lst' = filter_out (fn v => v=id_vert) lst
	fun flat_c [] = []
	  | flat_c ((Comp xs)::ys) = xs @ (flat_c ys)
	  | flat_c (y::ys) = y::(flat_c ys)
	val lst'' = flat_c lst'
    in if lst'' = lst then Comp lst else compose lst''
    end

exception vertex_tree_exp

(* If the given angle is non-zero, add it above the vertex, unless it has no
 * inputs, then add it below. *)
fun attach_angle b an trm inputs =
    if AngleExpr.is_zero an then trm
    else compose ((case inputs of 1 => I | _ => rev)
		      [V ("a"^b, [Pretty.string_of (AngleExpr.pretty an)]),trm])

(* TODO: does NSet have size? *)
val edge_count = fn s => UnitEdge.NSet.fold (fn _ => fn t => 1+t) s 0

(* expand spiders as a tree of deltas *)
fun vertex_tree b 0 n = compose [V ("e"^b^"_dag",[]), vertex_tree b 1 n]
  | vertex_tree b 1 0 = V ("e"^b,[])
  | vertex_tree b 1 1 = id_vert
  | vertex_tree b 1 2 = V ("d"^b,[])
  | vertex_tree b 1 n = compose [V ("d"^b,[]),
				 tensor [id_vert,
					 vertex_tree b 1 (n-1)]]
  | vertex_tree b _ _ = raise vertex_tree_exp (* shouldn't have two inputs *)

(* boundary vertices become identities, spiders become delta-trees, possibly
 * preceeded by an angle *)
fun translate_vertex graph vert =
    let val (v, (ie,oe)) = Graph.get_vertex graph vert
	val (ic,oc) = (edge_count ie, edge_count oe)
    in
	case v
	 of RGVertex.BVert () => BV vert
	  | RGVertex.IVert (RGVertex.Xnd e) =>
	    attach_angle "x" e (vertex_tree "x" ic oc) ic
	  | RGVertex.IVert (RGVertex.Znd e) =>
	    attach_angle "z" e (vertex_tree "z" ic oc) ic
	  | RGVertex.IVert (RGVertex.Hnd) => V ("H",[])
    end

fun trace_out e (g,tr) = let
	val (bv1, g') = g |> Graph.add_vertex bv
	val (bv2, g') = g' |> Graph.add_vertex bv
	val (_, (iv1,iv2)) = Graph.get_edge g' e
	val g' = g' |> Graph.delete_edge e
	val (_,g') = g' |> Graph.add_edge () iv1 bv1
	val (_,g') = g' |> Graph.add_edge () iv2 bv2
    in (g', (bv1, bv2)::tr)
    end

end

(* Hilbert space terms for red-green graphs *)
structure RGHilbTerm = HilbertTermFun(structure GraphAdapter = RGHilbAdapter)

