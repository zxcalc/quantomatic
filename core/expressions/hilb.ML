signature GRAPH_ADAPTER =
sig
    structure Graph : GRAPH
    val pretty_vertex : AlgFormat.format -> Graph.T ->
			Graph.Vertex.name -> Pretty.T
end

(* construct tensor term from a DAG *)
functor TensorTermFun (
  structure Graph : GRAPH
  val pretty_vertex : AlgFormat.format -> Graph.T ->
		      Graph.Vertex.name -> Pretty.T) =
struct

datatype term =
	 Tens of term list |
	 Comp of term list |
	 V of Graph.Vertex.name |
	 Sigma of Permutation.T |
	 Id of int

(* a component holds a term and a list of inputs in order *)
type component = Graph.Edge.name list * term * Graph.Edge.name list


fun wrap_vertex graph v = let
    val (ie,oe) = ((apfst Graph.Edge.NSet.list_of) o
		   (apsnd Graph.Edge.NSet.list_of) o
		   snd) (Graph.get_vertex graph v)
in (ie,V v,oe)
end

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
    val oset = Graph.Edge.NSet.of_list o1
    val new_ins =  filter_out (Graph.Edge.NSet.contains oset) i2
    val full_t1 = case length new_ins
		   of 0 => t1
		    | n => (case t1 of Tens ts => Tens (ts@[Id n])
				     | _ => Tens [t1, Id n])
    val perm = Permutation.get_perm Graph.Edge.name_ord (o1 @ new_ins) i2
    val sigma = if Permutation.is_id perm then [] else [Sigma perm]
in
    (i1 @ new_ins, flat_compose (full_t1 :: sigma @ [t2]), o2)
end


fun of_graph graph = let
    val dag = Graph.convert_to_dag graph
    val ranks = Graph.get_dag_ranks dag
    val tens_rank = tensor o (map (wrap_vertex dag))
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
fun pretty_vertex _ _ = Graph.Vertex.pretty_name

end
*)

structure RGHilbTerm = TensorTermFun(

structure Graph = RGGraph
fun pretty_vertex fmt graph vert = let
    val (v, (ie,oe)) = Graph.get_vertex graph vert
    val (ic,oc) = (Graph.Edge.NSet.cardinality ie,
		   Graph.Edge.NSet.cardinality oe)
    val nums = [Pretty.str (Int.toString ic),
		Pretty.str ", ",
		Pretty.str (Int.toString oc)]
in Pretty.block	
   (case v of RGVertex.IVert iv =>
	     (case iv
	       of RGVertex.Xnd e =>
		        [Pretty.str "xsp[", AngleExpr.pretty_math fmt e, Pretty.str ", "] @
		        nums @ [Pretty.str "]"]
		      | RGVertex.Znd e =>
		        [Pretty.str "zsp[", AngleExpr.pretty_math fmt e, Pretty.str ", "] @
		        nums @ [Pretty.str "]"]
          | RGVertex.Hnd => [Pretty.str "h"])
	  | _ => [Pretty.str "id2[1]"])
end

)

