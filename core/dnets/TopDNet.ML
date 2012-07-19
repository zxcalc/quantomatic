signature TOP_DNET =
sig
  type T
  structure G : BANG_GRAPH
  structure CL : COUNTOUR_LIST
  
  val mk : G.T GraphName.NTab.T -> T
  val prune : G.T -> T -> T
  val fold : (G.T -> 'a -> 'a) -> 'a -> T -> T

  val empty : T
  val get_contour_list : G.T * V.name -> 
  val add_graph_to_tree : G.T -> T -> T
end


structure Top_DNet : TOP_DNET
	
	type T

	(* build a tree given a set of pattern graphs *)
	fun mk(tab) =

		(* create a new empty_tree *)
		let t = empty

		(* foreach graph g in tab, call ContourList.build_contour_list(g) *)
		in  
				
		(* foreach contour_list c, add it to the tree t *)

	(* prune a tree given the tree and the graph *)
	fun prune(g,dnet_tree) =

	(* compute the list of all the graphs not pruned *)
	fun fold(g,a,b) =

end


val dnet = mk pattern_graphs;

val dnet' = prune target_graph dnet;

val matches = fold (fn r => fn lst => (match r g)@lst) dnet' [];