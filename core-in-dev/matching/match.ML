(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*   Matching between two graphs without bang boxes but with boundary      *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature MATCH_SHARING 
= sig
  type T
  structure G : BANG_GRAPH_SHARING
  structure EMap : NAME_INJ_SHARING
  structure BBMatch : BBOX_MATCH_SHARING

  sharing BBMatch.G = G
  sharing EMap.Cod = G.Edge.Ehn
  sharing EMap.Dom = G.Edge.Ehn
end;

signature MATCH 
= sig
  structure G : BANG_GRAPH
  structure EMap : NAME_INJENDO
  structure BBMatch : BBOX_MATCH

  type T (* the type of matches *)

  (* sharing via sharing structure *)
  structure Sharing : MATCH_SHARING
  sharing type Sharing.T = T
  sharing Sharing.BBMatch = BBMatch.Sharing
  sharing Sharing.EMap = EMap.Sharing
  sharing Sharing.G = G.SharingBangGraph

  (* init = empty, but with pat and tgt's exterior as interface *)
  val init : (G.Vertex.NSet.T * G.Edge.NSet.T 
               * G.BBox.NSet.T) (* other names to avoid when matching *)
             -> G.T -> G.T -> T 

  (* basic gets/sets/updates *)
  val get_emap : T -> EMap.T
  val get_viso : T -> G.Vertex.InjEndo.T
  val get_bvmap : T -> G.Vertex.Map.T
  val get_bbmatch : T -> BBMatch.T
  val get_names : T -> (G.Vertex.NSet.T * G.Edge.NSet.T * G.BBox.NSet.T)
  val get_subst : T -> G.Param.subst
  val set_names : (G.Vertex.NSet.T * G.Edge.NSet.T * G.BBox.NSet.T) -> T -> T 
  val update_names : (   G.Vertex.NSet.T * G.Edge.NSet.T * G.BBox.NSet.T
                      -> G.Vertex.NSet.T * G.Edge.NSet.T * G.BBox.NSet.T)
                     -> T -> T
  val update_bvmap : (G.Vertex.Map.T -> G.Vertex.Map.T) -> T -> T
  val update_emap : (EMap.T -> EMap.T) -> T -> T

  (* check is half edges are mapped this way already *)
  val emap_contains : T -> G.Edge.Ehn.name * G.Edge.Ehn.name -> bool

  (* exception raised when things do not match *)
  exception match_exp of unit;

  (* basic matching functions; raise match_exp on failure *)

  (* match edge data *)
  val match_edge_data : G.Edge.data -> G.Edge.data -> T -> T

  (* also checks that match is consistant w.r.t. other half of pat *)
  val match_edgehalf_name : G.Edge.Ehn.name -> G.Edge.Ehn.name -> T -> T
  val dir_match_edgehalf_name : G.Edge.Ehn.name -> G.Edge.Ehn.name -> T -> T
  (* doesn't check other half of pat is consistant *)
  val match_edgehalf_name' : G.Edge.Ehn.name -> G.Edge.Ehn.name -> T -> T
  val dir_match_edgehalf_name' : G.Edge.Ehn.name -> G.Edge.Ehn.name -> T -> T

  (* match edge name and data *)
  val match_edgehalf : G.Edge.Ehn.name * G.Edge.data 
      -> G.Edge.Ehn.name * G.Edge.data 
      -> T -> T
  (* requires that pat and tgt Ehn are the same half, (both Src or both Tgt) *)
  val dir_match_edgehalf : G.Edge.Ehn.name * G.Edge.data 
      -> G.Edge.Ehn.name * G.Edge.data 
      -> T -> T
  val match_edge : bool (* matched in reverse direction *) 
      -> G.Edge.name * G.Edge.data 
      -> G.Edge.name * G.Edge.data 
      -> T -> T

  (* match up vertices,  *)
  val match_vertex_data : G.Vertex.data -> G.Vertex.data -> T -> T
  val match_vertex_name : G.Vertex.name -> G.Vertex.name -> T -> T
  val match_vertex : G.Vertex.name * G.Vertex.data
                     -> G.Vertex.name * G.Vertex.data 
                     -> T -> T

  (* match pattern boundary point to target vertex/edge-point *)
  val match_boundary : G.Vertex.name -> G.Vertex.name -> T -> T

  (* instantiate a graph using the match, produces exactly the matched lhs
     component, or equivalently *)
  val inst_matched_graph : T -> G.T -> G.T 
  (* instantiate a substitution graph for the match: expand !-boxes, etc,
     keeping id map on names of boundary  *)
  val inst_subst_graph : T -> G.T -> G.T 


  (* bbox stuff *)
  val get_bbmap : T -> BBMatch.BBMap.T
  val add_bbox_drop : G.BBox.name -> T -> T;
  val add_bbox_kills : G.BBox.name list -> T -> T;
  val add_bbox_map : G.BBox.name -> G.BBox.name -> T -> T;
  val add_bbox_copy : (G.BBox.name (* old bbname *)
                 * (G.Vertex.name G.Vertex.NTab.T (* vertex copying *)
                      * G.Edge.name G.Edge.NTab.T) (* edge copying *)
                 * G.BBox.name) (* new bbox name *) 
                -> T -> T;
  
  val pretty : T -> Pretty.T
  val print : T -> unit
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor MatchFun(G : BANG_GRAPH) : MATCH 
= struct
  
  structure G = G;
  structure V = G.OVertex;
  structure E = G.Edge;
  
  structure BBox = G.BBox;
  structure BBMatch = BBoxMatchFun(G);
  structure BBMap = BBMatch.BBMap;
  
  structure Ehn = E.Ehn;
  structure EMap = E.Ehn.InjEndo;
  
  datatype T = Match of 
           {
            (* names context for fresh names when copying bboxes in pat *)
            names : (V.NSet.T * E.NSet.T * BBox.NSet.T),
            (* Internal vertex bijection from pat to tgt *)
            viso : V.InjEndo.T,
            (* map for boundary vertices pat to tgt vertices  *)
            bvmap: V.Map.T,
            (* 1-1, inj half-edge mapping *)
            emap: EMap.T,
            (* bang box expansion of pattern *)
            bbmatch : BBMatch.T,
            (* how variables in data are substiuted in the match *)
            subst: G.Param.subst
           };
  
  fun init (vs,es,bs) pat tgt = 
    Match {
      names = (V.NSet.union_merge (G.get_vnames pat) 
                  (V.NSet.union_merge (G.get_vnames tgt) vs),  
               E.NSet.union_merge (G.get_enames pat)
                  (E.NSet.union_merge (G.get_enames tgt) es), 
               BBox.NSet.union_merge (G.get_bbnames pat) 
                  (BBox.NSet.union_merge (G.get_bbnames tgt) bs)),
      viso = V.InjEndo.empty,
      bvmap = V.Map.empty,
      emap = EMap.empty,
      bbmatch = BBMatch.init_from_pat pat,
      subst = G.Param.empty_subst
    };
  
  (* updates *)
  fun update_names f (Match rep) = 
      Match {names = f (#names rep),
             bvmap = #bvmap rep, 
             emap = #emap rep,
             viso = #viso rep,
             bbmatch = #bbmatch rep,
             subst = #subst rep};
  fun update_bvmap f (Match rep) = 
      Match {names = #names rep,
             bvmap = f (#bvmap rep), 
             emap = #emap rep,
             viso = #viso rep,
             bbmatch = #bbmatch rep,
             subst = #subst rep};
  fun update_emap f (Match rep) = 
      Match {names = #names rep,
             bvmap = #bvmap rep, 
             emap = f (#emap rep),
             viso = #viso rep,
             bbmatch = #bbmatch rep,
             subst = #subst rep};
  fun update_viso f (Match rep) = 
      Match {names = #names rep,
             bvmap = #bvmap rep, 
             emap = #emap rep,
             viso = f (#viso rep),
             bbmatch = #bbmatch rep,
             subst = #subst rep};
  fun update_bbmatch f (Match rep) = 
      Match {names = #names rep,
             bvmap = #bvmap rep, 
             emap = #emap rep,
             viso = #viso rep,
             bbmatch = f (#bbmatch rep),
             subst = #subst rep};
  fun update_subst f (Match rep) = 
      Match {names = #names rep,
             bvmap = #bvmap rep, 
             emap = #emap rep,
             viso = #viso rep,
             bbmatch = #bbmatch rep,
             subst = f (#subst rep)};
  
  fun get_bvmap (Match rep) = #bvmap rep;
  fun get_emap (Match rep) = #emap rep;
  fun get_viso (Match rep) = #viso rep;
  fun get_names (Match rep) = #names rep;
  fun get_subst (Match rep) = #subst rep;
  val set_names = update_names o K;
  val set_subst = update_subst o K;
  
  fun get_bbmatch (Match rep) = #bbmatch rep;
  val get_bbmap = BBMatch.get_bbmap o get_bbmatch;
  
  val add_bbox_drop = update_bbmatch o BBMatch.add_drop;
  val add_bbox_kills = update_bbmatch o BBMatch.add_kills;
  val add_bbox_map = update_bbmatch oo BBMatch.add_bbmap;
  val add_bbox_copy = update_bbmatch o BBMatch.add_copy;
  
  (* True iff egde map already contains this mapping *)
  fun emap_contains m peh_teh = 
      EMap.contains (get_emap m) peh_teh;
  
  (* exception raised when things do not match *)
  exception match_exp of unit;
  
  (* checks that other half of the same patten edge is not mapped to 
     something else *)
  fun other_pat_half_is_consistant m peh teh = 
      (case EMap.lookup_domf (get_emap m) (E.Ehn.other peh) of 
         SOME teh2 => E.name_eq (E.Ehn.get_ename teh2, E.Ehn.get_ename teh)
       | NONE => true);
  
  (* match edge data, raising match_exp is data doesn't match *)
  fun match_edge_data ped ted m = 
      update_subst (G.Param.match_edge ped ted) m 
      handle G.Param.match_exp _ => raise match_exp ();
  
  (* *)
  fun match_edgehalf_name' peh teh m =  
      update_emap (EMap.add peh teh) m 
      handle EMap.add_exp (_,_,_) => raise match_exp ();
  
  (* *)
  fun dir_match_edgehalf_name' peh teh m =  
      if (G.Edge.Ehn.same_half peh teh) 
      then (update_emap (EMap.add peh teh) m
        handle EMap.add_exp (_,_,_) => raise match_exp ())
      else raise match_exp ();
  
  (* *)
  fun match_edgehalf_name peh teh m =  
      if other_pat_half_is_consistant m peh teh 
      then match_edgehalf_name' peh teh m 
      else raise match_exp ();
  
  fun dir_match_edgehalf_name peh teh m =
      if (G.Edge.Ehn.same_half peh teh) 
      then match_edgehalf_name peh teh m
      else raise match_exp ();
  
  (* match edge half name, with edge's data *)
  fun match_edgehalf (peh, ped) (teh, ted) m =
      m |> match_edgehalf_name peh teh
        |> match_edge_data ped ted;
  
  (* ensure that pat and tgt are for the same half of the edge 
     (both src or both tgt) *)
  fun dir_match_edgehalf (peh, ped) (teh, ted) m =
      m |> dir_match_edgehalf_name peh teh
        |> match_edge_data ped ted;
  
  (* match both halfs of the edge. If rev is true, then maps them 
     in opposite direction *)
  fun match_edge rev (pe, ped) (te, ted) m = 
      let val (peh1,peh2) = if rev then (Ehn.mk_end pe, Ehn.mk_start pe)
                                   else (Ehn.mk_start pe, Ehn.mk_end pe)
      in
        m |> match_edgehalf_name' peh2 (Ehn.mk_end te)
          |> match_edgehalf_name' peh1 (Ehn.mk_start te)
          |> match_edge_data ped ted
      end;
  
  
  (* *)
  fun match_vertex_data pvd tvd m =
      update_subst (G.Param.match_vertex pvd tvd) m 
      handle G.Param.match_exp _ => raise match_exp ();
  
  fun match_vertex_name pv tv m =
      update_viso (V.InjEndo.add pv tv) m
      handle V.InjEndo.add_exp _ => raise match_exp ();
  
  fun match_vertex (pv,pvd) (tv,tvd) m =
      m |> match_vertex_name pv tv 
        |> match_vertex_data pvd tvd; 
             
  
  fun match_boundary pb tb m = m |> update_bvmap (V.Map.add_to_dom pb tb);
  
  (* apply data substs of a match to all vertices and edges in graph *)
  fun subst m g = G.apply_data_subst (get_subst m) g;

  (* instantiate a graph using the match, produces exactly the matched lhs
     component, or equivalently *)
  fun inst_matched_graph m g = 
      g |> BBMatch.inst_lhs_bbox_match (get_bbmatch m)
        |> subst m;

  (* instantiate a substitution graph for the match: expand !-boxes, etc,
     keeping id map on names of boundary  *)
  fun inst_subst_graph m g = 
      g |> BBMatch.inst_rhs_bbox_match (get_names m) (get_bbmatch m)
        |> subst m;
  
  
  (* pretty printing *)
  fun pretty_names (vs,es,bs) = 
    Pretty.block 
      [Pretty.str "Vs: ", V.NSet.pretty vs,
       Pretty.str "; Es: ", E.NSet.pretty es,
       Pretty.str "; Bs: ", BBox.NSet.pretty bs];
  
  fun pretty m = 
      Pretty.chunks 
      ([Pretty.str "Match: ",
        (Pretty.indent 2 o Pretty.chunks)
        ([pretty_names (get_names m)]
         @ [Pretty.block 
             [Pretty.str "Boundary Map: ",
              V.Map.pretty (get_bvmap m)]]
         @ [BBMatch.pretty (get_bbmatch m)]
         @ [Pretty.block 
             [Pretty.str "Internal Vertex InjEndo: ",
              V.InjEndo.pretty (get_viso m)]]
          @ [EMap.pretty (get_emap m)])
      ]);
  
  val print = Pretty.writeln o pretty;
  
  structure Sharing : MATCH_SHARING 
  = struct
    structure G = G.SharingBangGraph
    structure BBMatch = BBMatch.Sharing
    structure EMap = EMap.Sharing
    type T = T;
  end;
    
end;