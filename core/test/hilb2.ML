(* construct hilbert term from a DAG *)

signature GRAPH_ADAPTER =
sig
    structure Graph : GRAPH
    datatype term =
	     Tens of term list |
	     Comp of term list |
	     V of Graph.Vertex.name |
	     Sigma of Permutation.T |
	     Id of int
    (* a component holds a term and a list of inputs in order *)
    type component = Graph.Edge.name list * term * Graph.Edge.name list

    val translate_vertex : Graph.T -> Graph.Vertex.name -> component
end


functor TensorTermFun (structure GraphAdapter : GRAPH_ADAPTER) =
struct

structure GraphAdapter = GraphAdapter
open GraphAdapter

(* tensor a list of components, gathering inputs *)
fun tensor [] = ([],Id 0,[])
  | tensor [x] = x
  | tensor clist = fold_rev (fn (i1,t,o1) =>
			     fn (i1s, Tens ts, o1s) =>
				(i1@i1s, Tens (t::ts), o1@o1s))
			    clist ([], Tens [], [])
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
    val tens_rank = tensor o (map (translate_vertex dag))
in #2 (fold_rev compose (map tens_rank ranks) ([], Id 0, []))
end


fun pretty_text (V n) = Graph.Vertex.pretty_name n
  | pretty_text (Id num) = Pretty.str ("id(" ^ (Int.toString num) ^ ")")
  | pretty_text (Sigma perm) = Pretty.block[Pretty.str "sig",
					    Pretty.str_list
						"[" "]"
						(map Int.toString perm)]
  | pretty_text (Comp lst) = Pretty.block
				 ([Pretty.str "("]@
				  (Pretty.separate
				       " o" (map pretty_text (rev lst)))@
				  [Pretty.str ")"])
  | pretty_text (Tens lst) = Pretty.block
				 ([Pretty.str "("]@
				  (Pretty.separate
				       " x" (map pretty_text lst))@
				  [Pretty.str ")"])

val print = Pretty.writeln o pretty_text
fun printc (_,t,_) = print t
end


structure RGGraphAdapter : GRAPH_ADAPTER =
struct
structure Graph = RGGraph
datatype term =
	 Tens of term list |
	 Comp of term list |
	 V of Graph.Vertex.name |
	 Sigma of Permutation.T |
	 Id of int

(* a component holds a term and a list of inputs in order *)
type component = Graph.Edge.name list * term * Graph.Edge.name list
		 
fun translate_vertex graph v = let
    val (ie,oe) = ((apfst Graph.Edge.NSet.list_of) o
		   (apsnd Graph.Edge.NSet.list_of) o
		   snd) (Graph.get_vertex graph v)
in (ie,V v,oe)
end

end


structure RGHilbTerm = TensorTermFun(structure GraphAdapter = RGGraphAdapter)


