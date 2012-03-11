(* functor OGraphPreLayout(Graph : OGRAPH) 
: 
= struct
*) 
structure OGraphPreLayout = 
struct
  structure Theory = Str_Theory;
  open Theory;

  (* grid layout of graph *)
  datatype T = PreLayout of (V.NSet.T) (* horizontal seq defines a layer *) 
                            list (* vertical seq connected to previous layer *)
                            list; (* horizontal seq of disconnected parts *)

  fun is_on_next_level g seen_vs next_v = 
    V.NSet.sub_set (Graph.pred_vnames g next_v) seen_vs;

  fun add_next_vs g seen_vs v next_vs = 
    let 
      (* look at a vertex, v', that is assumed to be adjacent to v, and decide if it should be on the next level of the layout (true when all it's ancestors have been seen, but it hasn't yet). *)
      fun consider_next_v (ehn, (dir, data)) v' vs =
        if H.is_start ehn then vs (* going into v, so skip *)
        else (* going out of v, so maybe put on next level *)
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
      
      val (x as (seen_vs, layouts)) = 
          V.NSet.fold
            (fn v => fn (seen_vs, layouts) =>
              if V.NSet.contains seen_vs v then (seen_vs, layouts)
              else 
                let val (seen_vs', new_layout) = 
                  add_to_layouts g (V.NSet.single v) (seen_vs, init_layout)
                in (seen_vs', new_layout :: layouts) end)
            (Graph.get_vnames g) (seen_vs, [layout1]);
            
    (* seen_vs = (Graph.get_vnames g) *)
    in (* fix ordering of layouts *) 
    map rev layouts end;

  fun pretty_layout (l : V.NSet.T list) = 
    Pretty.list "[" "]" 
      (map V.NSet.pretty l); 
    
  val print_layout = Pretty.writeln o pretty_layout;
end;
