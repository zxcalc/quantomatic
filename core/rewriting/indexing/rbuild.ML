
structure RandomGraph
= struct
  
  structure G = RGGraph;
  structure V = G.RGVertex
  structure Lib = Library;
  structure NSet = V.NSet;
  open MkG;
  
  
  
  fun all_ivert_list 0 = []
    | all_ivert_list n =
    	let
    	val angle = Lib.one_of [zero_angle, pi_angle]
    	val node  = Lib.one_of [mkZ angle, mkX angle, mkH]
    	in
    	node :: all_ivert_list (n-1)
    	end;
    	
  fun no_angle_ivert_list 0 = []
    | no_angle_ivert_list n =
    	let
    	val node  = Lib.one_of [mkZ zero_angle, mkX zero_angle, mkH]
    	in
    	node :: no_angle_ivert_list (n-1)
    	end;
    	
  (**)
  fun create_connected_component vtypes =
  	let
  	val graph = fold G.doadd_vertex vtypes G.empty
  	val vnames = NSet.list_of (G.get_vnames graph)
  	fun add_e_to_graph [] g acc 	    = g
  	  | add_e_to_graph (n::names) g []  = add_e_to_graph names g [n]
  	  | add_e_to_graph (n::names) g acc =
  		let
  		val iname = Lib.one_of acc
  		val g2 = G.doadd_edge edge n iname g
  		in
  		add_e_to_graph names g2 (n::acc)
  		end
  	in
  	add_e_to_graph vnames graph []
  	end;
  
  fun one_pair_from [name] = (name,name)
    | one_pair_from names =
  	let 
  	val first = Lib.one_of names
  	val second= Lib.one_of (filter (fn x => not (V.name_eq (first,x))) names)
  	in
  	(first,second)
  	end;
  	
  	
  fun  add_extra_edges n vnames graph =
  	let
  	fun add_edges 0 vs g = g
  	  | add_edges n (vs : V.name list) g = 
  	  	let
  	  	val (v1,v2) = one_pair_from vs
  	  	in
  	  	add_edges (n-1) vnames
  	  		(G.doadd_edge edge
  	  			v1 v2
  	  			g)
  	  	end
  	in
  	add_edges n vnames graph
  	end;
  	
  fun add_boundary n ivnames graph =
  	let 
  	val (bvnames,graph2) = 
  		fold (fn _ => fn (xs, g) => 
  			let 
  			val (name,g2) = G.add_vertex bvert g
  			in 
  			(name::xs,g2) 
  			end
  			) (1 upto n) ([],graph)
	in
	fold (fn name => fn g =>
		  G.doadd_edge edge 
		  	       name 
		  	       (Lib.one_of ivnames) 
		  	       g) bvnames graph2
	end;
  	
  (*generates a random connected graph in minimal form (all boundary verts have 
    only one incident edge) there will be at least sufficient edges to connect 
    Iverts, but more may be added with nume.Allows you to control which random 
    vertex type generator is used*)	
  fun pre_random_graph vgen numv numb nume =
  	let
  	val prim_graph = vgen numv
  			 |> create_connected_component
  	val vnames = (NSet.list_of o G.get_vnames) prim_graph
  	in
  	prim_graph
  	|> add_extra_edges nume vnames
  	|> add_boundary    numb vnames
  	end;
	
  (*all vertex types appear, including pi and zero X and Z vertices*)
  val std_rgraph = pre_random_graph all_ivert_list
  
  (*X and Z vertices all have zero angle*)
  val simple_rgraph = pre_random_graph no_angle_ivert_list
  	
  fun pattern_set' graph_generator nrules maxv maxb  =
  	let
  	val maxv' = if maxv < 3 then 3 else maxv
  	val maxb' = if maxb < 2 then 2 else maxb
  	in
  	map (fn _ => 
  		let
  		val vs = Lib.random_range 2 maxv'
  		val bs = Lib.random_range 1 maxb'
  		in
  		graph_generator vs bs 0
  		end) (1 upto nrules)
  	end;
  	
  	
  
  fun pattern_set gg nr = pattern_set' gg nr 4 3;	
  fun std_pattern_set nrules = pattern_set' (std_rgraph) nrules 4 3;
  fun simple_pattern_set nrules = pattern_set' (simple_rgraph) nrules 4 3;
  fun varied_pattern_set nrules = pattern_set' (std_rgraph) nrules 20 5;
  		
  
end;
