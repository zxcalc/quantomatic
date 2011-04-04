
structure Graff = Test_Graph;

val g = Graff.empty
local
    val bv = RG_VertexData.BVert
    val (v1,g) = g|> Graff.add_vertex bv
    val (v2,g) = g|> Graff.add_vertex bv
    val (v3,g) = g|> Graff.add_vertex bv
    val (v4,g) = g|> Graff.add_vertex bv
    val (v5,g) = g|> Graff.add_vertex bv
    val (v6,g) = g|> Graff.add_vertex bv
    val (v7,g) = g|> Graff.add_vertex bv
    val (v8,g) = g|> Graff.add_vertex bv
in
val (_,g) = g|> Graff.add_edge () v1 v2
val (_,g) = g|> Graff.add_edge () v1 v2
val (_,g) = g|> Graff.add_edge () v2 v3
val (_,g) = g|> Graff.add_edge () v3 v4
val (_,g) = g|> Graff.add_edge () v4 v2
val (_,g) = g|> Graff.add_edge () v4 v5

val (_,g) = g|> Graff.add_edge () v6 v7
val (_,g) = g|> Graff.add_edge () v7 v8
val (_,g) = g|> Graff.add_edge () v8 v6

val (_,g) = g|> Graff.add_edge () v1 v6
end

(*
structure Dag = 
struct
open Graff

(* will probably not terminate if dag isn't directed acyclic *)
fun get_dag_ranks dag = let
    val vnames = Vertex.NSet.list_of (get_vnames dag)
    fun test seen v = not (Vertex.NSet.contains seen v)
		      andalso Vertex.NSet.forall
				  (Vertex.NSet.contains seen)
				  (succ_vnames dag v)
    fun mk seen =
	case filter (test seen) vnames
	 of []=>[] | lst => lst :: mk (Vertex.NSet.add_list lst seen)
in mk Vertex.NSet.empty
end


(* find the "longest" directed acyclic graph that spans the given graph *)
fun dag_traverse back_edge fw_edge new_edge graph = let
    val out_elist = Edge.NSet.list_of o (out_enames graph)

    (* generate a dag from the given edge list *)
    fun dag depth (e::es) dg vtab =
	let
	    val (edata, (s, t)) = get_edge graph e
	    val (dg', vtab') =
		(case Vertex.NTab.lookup vtab t
		  (* if target hasn't been seen, add edge and recurse *)
		  of NONE => dag (depth + 1) (out_elist t)
				 (dg |> new_edge e edata s t)
				 (vtab |> Vertex.NTab.update (t, depth + 1))
		   (* if target has been seen, only add edge if its rank is lower *)
		   | SOME d => if (depth < d orelse d = ~1)
			       then (dg |> fw_edge e edata s t, vtab)
			       else (dg |> back_edge e edata s t, vtab))
	in dag depth es dg' vtab'
	end
      | dag _ [] dg vtab = (dg, vtab)
    
    (* pull a fresh vertex *)
    fun fresh_vert vtab = let
	val vnames = get_vnames graph
	fun fv NONE = NONE
	  | fv (SOME v) = (case Vertex.NTab.lookup vtab v
			    of NONE => SOME v
			     | _ => fv (Vertex.NSet.next_bigger vnames v))
    in fv (Vertex.NSet.get_min vnames)
    end
    
    (* iteratively run dag until we run out of vertices *)
    fun dag_until_done dg vtab =
	case fresh_vert vtab
	 of NONE => dg
	  | (SOME v) => let
		val vtab = (Vertex.NTab.map_all (K ~1) vtab)
			       |> Vertex.NTab.update (v,0)
		val (dg',vtab') = dag 0 (out_elist v) dg vtab
	    in dag_until_done dg' vtab'
	    end
    val init_dg = Edge.NSet.fold delete_edge (get_enames graph) graph
in dag_until_done init_dg Vertex.NTab.empty
end


local
    fun ignore_edge _ _ _ _ = I
    fun keep_edge e edata s t g = #2(g|>add_named_edge e edata s t)
    fun flip_edge e edata s t g = if Vertex.name_ord (s,t) = EQUAL then g
				    else #2(g|>add_named_edge e edata t s)
in
    (* remove self-loops and reverse back-edges *)
    val convert_to_dag = dag_traverse flip_edge keep_edge keep_edge
    (* remove self-loops and back-edges *)
    val spanning_dag = dag_traverse ignore_edge keep_edge keep_edge
    val spanning_tree = dag_traverse ignore_edge ignore_edge keep_edge
end

end
*)
