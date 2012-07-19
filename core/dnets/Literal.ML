signature LITERAL =
sig
	type T
	structure G : BANG_GRAPH

	val mk : G.T * V.name -> T
	val eq : T * T -> bool

	val get_adj : G.T * T -> T list

end


functor Literal ( G : BANG_GRAPH ) : LITERAL =
struct

	structure G = G;

	datatype multiplicity = None | Star | QM

	type T = { name : V.name,
			   kind : G.VData.data,
	           kind_mult : multiplicity,
	           input_arity : int,
	           input_mult : multiplicity, 
	           output_arity : int,
	           output_mult : multiplicity }

	(* Helper function to check if all the adjacent nodes are in a bang-box *)
	fun all_bang(g, []) = false
  	  | all_bang(g, v::[]) = G.is_bboxed g v
  	  | all_bang(g, v::vlist) =
      	if G.is_bboxed g v then all_bang(g, vlist)
      	else false

    (* Helper function to check if any of the adjacent nodes is in a bang-box *)
    fun exists_bang(g, []) = false
      | exists_bang (g, v::[]) = G.is_bboxed g v
      | exists_bang (g, v::vlist) =
        if G.is_bboxed g v then true
        else exists_bang(g,vlist)
		
	
	(* Helper functions to get multiplicity *)
	fun get_kind_mult(g,v) =
	   let
	      val adj_vert = G.adj_vnames g v
	      val v_list = V.NSet.list_of adj_vert
	   in
	     if G.is_bboxed g v then Star
	     else if all_bang(g, v_list) then QM
	         else None
	   end	

	fun get_input_mult(g,v) =
		let
	      val adj_vert = G.pred_vnames g v
	      val v_list = V.NSet.list_of adj_vert
	   in
	     if exists_bang(g,v_list) then Star
	     else None
	   end

	fun get_output_mult(g,v) =
		let
	      val adj_vert = G.succ_vnames g v
	      val v_list = V.NSet.list_of adj_vert
	   in
	     if exists_bang(g,v_list) then Star
	     else None
	   end

	(* Make a literal *)   
	fun mk(g,v) = 
		let
			val k = G.get_vertex_data g v
			
			val k_m = get_kind_mult(g,v)			
			
			val i_a = Arity.get_in (G.get_arity g v)
			
			val i_m = get_input_mult(g,v)
			
			val o_a = Arity.get_out (G.get_arity g v)

			val o_m = get_output_mult(g,v)
		in
			{name = v, kind = k, kind_mult = k_m, input_arity = i_a, input_mult = i_m, output_arity = o_a, output_mult = o_m} : T
		end


	(* Helper functions to compare literals *)
	fun compare_kind(l1,l2) = 
		G.VData.data_eq(#kind l1, #kind l2)

	fun compare_input(l1,l2) = 
		if (#input_arity l1 = #input_arity l2) then true
		else if (#input_arity l1 > #input_arity l2) andalso (#input_mult l2 = Star) then true
		     else if (#input_arity l1 < #input_arity l2) andalso (#input_mult l1 = Star) then true
		          else false

	fun compare_output(l1,l2) =
		if (#output_arity l1 = #output_arity l2) then true
		else if (#output_arity l1 > #output_arity l2) andalso (#output_mult l2 = Star) then true
		     else if (#output_arity l1 < #output_arity l2) andalso (#output_mult l1 = Star) then true
		          else false

	(* Compare literals *)
	fun eq(l1,l2) = compare_kind(l1,l2) andalso compare_input(l1,l2) andalso compare_output(l1,l2)

	(* Get adjacent nodes *)
	fun get_adj(g,l) =
		let
	      val adj_vert = G.adj_vnames g (#name l)
	      val v_list = V.NSet.list_of adj_vert

	      val g_list = replicate (length v_list) g

	      val gv_list = g_list ~~ v_list

	    in

		  map mk gv_list    
	      
	    end


end