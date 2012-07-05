(* functor OGraphPreLayout(Graph : OGRAPH) 
: 
= struct
*) 
local 
  structure Graph = Str_Theory.Graph;
in 
structure OGraphPreLayout = 
struct
  (* Given vertices seen_vs in graph g, this is true when next_v has 
     an edge to it from one of the seen_vs of g. *)
  fun is_on_next_level g seen_vs next_v = 
    V.NSet.sub_set (Graph.pred_vnames g next_v) seen_vs;

  (* Given vertex names seen_vs in graph g, add the vertices with edges from 
     v to the set of vertex names next_vs *)
  fun add_next_vs g seen_vs v next_vs = 
    let 
      (* look at a vertex, v', that is assumed to be adjacent to v, and 
         decide if it should be on the next level of the layout (true when 
         all it's ancestors have been seen, but it hasn't yet). *)
      fun consider_next_v (Graph.SelfLoop,_,_) vs = vs
        | consider_next_v (Graph.Incoming,_,_) vs = vs
        | consider_next_v (Graph.Outgoing,v',_) vs =
          if is_on_next_level g seen_vs v' then V.NSet.add v' vs 
          else vs;
    in Graph.fold_adj consider_next_v g v next_vs end;
    
  (* make list of vs that will be on the next level *)
  fun add_to_next_vlist g current_vlist (seen_vs,next_vs) = 
    let 
      val seen_vs' = fold V.NSet.add current_vlist seen_vs
      val next_vs' = fold (add_next_vs g seen_vs') current_vlist next_vs
    in (seen_vs', next_vs') end;

  (* *)
  fun add_to_layouts g start_vs (seen_vs, layout) = 
    snd (while_some 
          (fn (next_vs, (seen_vs, layout)) => 
              if V.NSet.is_empty next_vs then NONE
              else let 
                val (seen_vs', next_vs') = 
                    add_to_next_vlist g (V.NSet.list_of next_vs) 
                      (seen_vs, V.NSet.empty)
                in SOME (next_vs', (seen_vs', next_vs::layout)) end)
          (start_vs, (seen_vs, layout)));
    
  (* *)
  fun layout_graph g = 
    let
      val init_vs = Graph.get_inputs g;
      val init_layout = [];
          
      (* First make walk down from inputs to what they connect to, following 
         the direction of edges (for undirected, we use their internal
         direction). *)
      val (x as (seen_vs, layout1)) = 
        add_to_layouts g init_vs (V.NSet.empty, init_layout);
      
      (* there may be graph components which are disjoint from inputs, we now 
         make the layout for each of these. *)
      val (x as (seen_vs, layouts)) = 
          V.NSet.fold
            (fn v => fn (seen_vs, layouts) =>
              if V.NSet.contains seen_vs v then (seen_vs, layouts)
              else 
                let val (seen_vs', new_layout) = 
                  add_to_layouts g (V.NSet.single v) (seen_vs, init_layout)
                in (seen_vs', new_layout :: layouts) end)
            (Graph.get_vnames g) (seen_vs, [layout1]);
      (* Invariant: seen_vs = (Graph.get_vnames g) *)

      (* fix ordering of layouts *)
      val layouts_inputs_to_outputs = map rev layouts         
    in layouts_inputs_to_outputs end;

  fun pretty_layout (l : V.NSet.T list) = 
    Pretty.list "[" "]" 
      (map V.NSet.pretty l); 

  val print_layout = Pretty.writeln o pretty_layout;
  
  datatype box = Param of {
    (* space between vertices *)
    spacing : int,
    (* vertexs-size *)
    vertex_size : int,
    (* edge-width *)
    edge_width : int
  };
  
  datatype box = Box of {
    (* in terms of number of vertices *)
    width : int,
    height : int,
    vertex_layout : V.name list list
  };
  
  fun width_of_layout l = 
      fold (fn vset => fn n => 
            let val n' = V.NSet.cardinality vset in 
              if n < n' then n' else n end) l 0;
  
  fun height_of_layout l = length l;

  fun box_of_layout l = 
    Box {
      width = width_of_layout l,
      height = height_of_layout l,
      vertex_layout = map V.NSet.list_of l
    };

  (* define the x-coordinate for each vertex in the row, updating it's 
     entry in xposmap. *)
  fun add_row_to_xpos_map pix_width row xposmap = 
      let 
        val pix_sep = (Real.fromInt pix_width) / (Real.fromInt ((length row) + 1));
        val (endpos, xposmap) = 
          fold (fn v => fn (xpos,xposmap) => 
                (pix_sep + xpos, V.NTab.update (v,xpos) xposmap)
               ) row (pix_sep, xposmap)
      in xposmap end;
  
  fun add_layout_to_ypos_map vsep layout yposmap = 
      fold (fn row => fn (ypos, yposmap) =>
            (ypos + vsep, fold (fn v => V.NTab.update (v,ypos)) row yposmap)
           ) layout (vsep, yposmap);
  
  fun pos_maps_of_box hsep vsep (Box rep) =
    let
      (* pix_width is total width of the layout, this is used to centre-align
         the vertices by the widest part of the graph. *)
      val pix_width = (hsep + 1) * (#width rep)
      val xpos_map = 
          fold (add_row_to_xpos_map pix_width) 
               (#vertex_layout rep) V.NTab.empty; 
      val ypos_map = 
          add_layout_to_ypos_map vsep (#vertex_layout rep) V.NTab.empty;
    in 
      (xpos_map, ypos_map)
    end;

  fun svg_pretty_in_context p = 
    Pretty.block 
      ((map Pretty.str 
         ["<svg xmlns=\"http://www.w3.org/2000/svg\" ",
          "version=\"1.1\" ",
          (* "viewBox=\"0 0 1000 1000\" preserveAspectRatio=\"xMidYMid slice\" ",
          "style=\"width:", Int.toString x, "; height:", 
          Int.toString y,"; z-index:-2;", *) 
          "\">", "\n"])
        @ [p]
        @ (map Pretty.str
           ["</svg>", "\n"]));
  
  fun svg_pretty_vertex n x y = 
    Pretty.block (map Pretty.str 
      ["<circle id=\"", V.string_of_name n, "\" ", 
      "cx=\"", Int.toString x, "\" ", 
      "cy=\"", Int.toString y, "\" ",
      "r=\"10\" stroke=\"black\" stroke-width=\"2\" fill=\"red\" />\n"]);
    
end;
end; (* *)
