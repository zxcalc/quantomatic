signature GRAPH_ADAPTER =
sig
    structure Graph : OGRAPH
    val pretty_vertex : AlgFormat.format -> Graph.T ->
			V.name -> Pretty.T
end

(* construct tensor term from a DAG *)
functor TensorTermFun (
  structure Graph : OGRAPH
  val pretty_vertex : AlgFormat.format -> Graph.T ->
		      V.name -> Pretty.T) =
struct

datatype term =
	 Tens of term list |
	 Comp of term list |
	 V of V.name |
	 Sigma of Permutation.T |
	 Id of int

(* a component holds a term and a list of inputs in order *)
type component = E.name list * term * E.name list


(* tensor a list of components, gathering inputs *)
fun tensor [] = ([],Id 0,[])
  | tensor [x] = x
  | tensor clist =
    let val (input, terms, output)
          = fold_rev (
            fn (i1,t,o1) =>
			      fn (i1s, ts, o1s) =>
				       (i1@i1s, (t::ts), o1@o1s))
			      clist
            ([], [], [])
    in (input, Tens terms, output)
    end
val flat_compose = let
    fun fc [] = []
      | fc ((Comp l1)::l2) = l1 @ fc l2
      | fc (t::ts) = t :: fc ts
in Comp o fc
end

(* perform the composition t1 ; t2 *)
fun compose (i1, t1, o1) (_, Id _, _) = (i1, t1, o1)
  | compose (i1, t1, o1) (i2, t2, o2) = let
    val oset = E.NSet.of_list o1
    val new_ins =  filter_out (E.NSet.contains oset) i2
    val full_t1 = case length new_ins
		   of 0 => t1
		    | n => (case t1 of Tens ts => Tens (ts@[Id n])
				     | _ => Tens [t1, Id n])
    val perm = Permutation.get_perm E.name_ord (o1 @ new_ins) i2
    val sigma = if Permutation.is_id perm then [] else [Sigma perm]
in
    (i1 @ new_ins, flat_compose (full_t1 :: sigma @ [t2]), o2)
end

  
local
  (* find the "longest" directed acyclic graph that spans the given graph, applying
   * the three given functions to add edges of each kind.
   * back_edge : an edge that creates a directed cycle
   * fw_edge : an edge that creates a cycle but not a directed cycle
   * new_edge : an edge that creates no cycles *)
  fun dag_traverse back_edge fw_edge new_edge graph = let
      val out_elist = E.NSet.list_of o (Graph.get_out_edges graph)
  
      (* generate a dag from the given edge list *)
      fun dag depth (e::es) dg vtab =
    let
        val (edata, (s, t)) = Graph.get_edge_info graph e
        val (dg', vtab') =
      (case V.NTab.lookup vtab t
        (* if target hasn't been seen, add edge and recurse *)
        of NONE => dag (depth + 1) (out_elist t)
           (dg |> new_edge e edata s t)
           (vtab |> V.NTab.update (t, depth + 1))
         (* if target has been seen, only add edge if its rank is lower *)
         | SOME d => if (depth < d orelse d = ~1)
               then (dg |> fw_edge e edata s t, vtab)
               else (dg |> back_edge e edata s t, vtab))
    in dag depth es dg' vtab'
    end
        | dag _ [] dg vtab = (dg, vtab)
      
      (* pull a fresh vertex *)
      fun fresh_vert vtab = let
    val vnames = Graph.get_vertices graph
    fun fv NONE = NONE
      | fv (SOME v) = (case V.NTab.lookup vtab v
            of NONE => SOME v
             | _ => fv (V.NSet.next_bigger vnames v))
      in fv (V.NSet.get_min vnames)
      end
      
      (* iteratively run dag until we run out of vertices *)
      fun dag_until_done dg vtab =
    case fresh_vert vtab
     of NONE => dg
      | (SOME v) => let
      val vtab = (V.NTab.map_all (K (K ~1)) vtab)
               |> V.NTab.update (v,0)
      val (dg',vtab') = dag 0 (out_elist v) dg vtab
        in dag_until_done dg' vtab'
        end
      val init_dg = E.NSet.fold Graph.delete_edge (Graph.get_edges graph) graph
  in dag_until_done init_dg V.NTab.empty
  end
  fun ignore_edge _ _ _ _ = I
  fun keep_edge _ edata s t g = g|>Graph.add_edge_anon edata s t
  (* flip_edge also removes self-loops *)
  fun flip_edge _ edata s t g = if V.name_ord (s,t) = EQUAL then g
        else g|>Graph.add_edge_anon edata t s
in
  (* remove self-loops and reverse back-edges *)
  val convert_to_dag = dag_traverse flip_edge keep_edge keep_edge
  (* remove self-loops and back-edges *)
  val spanning_dag = dag_traverse ignore_edge keep_edge keep_edge
  val spanning_tree = dag_traverse ignore_edge ignore_edge keep_edge
end
  
  (* will probably not terminate if dag isn't directed acyclic *)
  fun get_dag_ranks dag = let
      val outputs = V.NSet.filter
           (fn v => E.NSet.is_empty (Graph.get_out_edges dag v))
           (Graph.get_vertices dag)
      val vnames = V.NSet.list_of
           (V.NSet.subtract (Graph.get_vertices dag) outputs)
      fun test seen v = not (V.NSet.contains seen v)
            andalso V.NSet.forall
            (V.NSet.contains seen)
            (Graph.get_predecessor_vertices dag v)
      fun mk seen =
    case filter (test seen) vnames
     of []=>[] | lst => lst :: mk (V.NSet.add_list lst seen)
  in mk V.NSet.empty @ [V.NSet.list_of outputs]
  end



fun of_graph graph = let
    fun wrap_vertex v =
      (E.NSet.list_of (Graph.get_in_edges graph v),
       V v,
       E.NSet.list_of (Graph.get_out_edges graph v))
    val dag:Graph.T = convert_to_dag graph
    val ranks:V.name list list = get_dag_ranks dag
    val tens_rank = tensor o (map wrap_vertex)
in (graph, #2 (fold_rev compose (map tens_rank ranks) ([], Id 0, [])))
end

open AlgFormat
fun pretty format (graph,term) = let
    val ((topen,tclose,tsep),csep) =
	case format
	 of MATHEMATICA => (("T[", "]", ","), " .")
	  | PLAIN => (("(", ")", " x"), " o")
	  | _ => raise not_supported_exp
    fun pr (V name) = pretty_vertex format graph name
      | pr (Id num) = Pretty.str ("id2[" ^ (Int.toString num) ^ "]")
      | pr (Sigma perm) = Pretty.block[Pretty.str "sig",
				       Pretty.str_list
					   "[" "]"
					   (map Int.toString perm)]
      | pr (Comp lst) = Pretty.block
			    ([Pretty.str "("]@
			     (Pretty.separate
				  csep (map pr (rev lst)))@
			     [Pretty.str ")"])
      | pr (Tens lst) = Pretty.block
			    ([Pretty.str topen]@
			     (Pretty.separate
				  tsep (map pr lst))@
			     [Pretty.str tclose])
in pr term
end

    

val print = Pretty.writeln o (pretty PLAIN)
fun printc (_,t,_) = print t
end

(*
structure RGGraphAdapter : GRAPH_ADAPTER =
struct
structure Graph = RGGraph
fun pretty_vertex _ _ = V.pretty_name

end
*)

