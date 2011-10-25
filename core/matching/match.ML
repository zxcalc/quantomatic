(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*   Matching between two graphs without bang boxes but with boundary      *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature MATCH_SHARING 
= sig
  type T
  structure G : OGRAPH_SHARING
  structure BBMatch : BBOX_MATCH_SHARING
  sharing BBMatch.G = G
end;

signature MATCH 
= sig
  structure G : BANG_GRAPH
  structure BBMatch : BBOX_MATCH

  type T (* the type of matches *)

  (* sharing via sharing structure *)
  structure Sharing : MATCH_SHARING
  sharing type Sharing.T = T
  sharing Sharing.BBMatch = BBMatch.Sharing
  sharing Sharing.G = G.SharingOGraph

  (* init = empty, but with pat and tgt's exterior as interface *)
  val init : (V.NSet.T * E.NSet.T 
               * B.NSet.T) (* other names to avoid when matching *)
             -> G.T -> G.T -> T 

  (* basic gets/sets/updates *)
  val get_emap : T -> HInjEndo.T
  val get_viso : T -> VInjEndo.T
  val get_evmap : T -> VMap.T
  val get_bbmatch : T -> BBMatch.T
  val get_names : T -> (V.NSet.T * E.NSet.T * B.NSet.T)
  val get_subst : T -> G.Param.subst
  val get_pluggings : T -> VInjEndo.T
  val set_names : (V.NSet.T * E.NSet.T * B.NSet.T) -> T -> T 
  val update_names : (   V.NSet.T * E.NSet.T * B.NSet.T
                      -> V.NSet.T * E.NSet.T * B.NSet.T)
                     -> T -> T
  val update_evmap : (VMap.T -> VMap.T) -> T -> T
  val update_emap : (HInjEndo.T -> HInjEndo.T) -> T -> T
  val update_pluggings : (VInjEndo.T -> VInjEndo.T) -> T -> T

  (* check is half edges are mapped this way already *)
  val emap_contains : T -> H.name * H.name -> bool
  val emap_contains_pat_ehn : T -> H.name -> bool

  (* exception raised when things do not match *)
  exception match_exp of unit;

  (* basic matching functions; raise match_exp on failure *)

  (* match edge data *)
  val match_edge_data : G.EData.data -> G.EData.data -> T -> T

  (* also checks that match is consistant w.r.t. other half of pat *)
  val match_edgehalf_name : H.name -> H.name -> T -> T
  val dir_match_edgehalf_name : H.name -> H.name -> T -> T
  (* doesn't check other half of pat is consistant *)
  val match_edgehalf_name' : H.name -> H.name -> T -> T
  val dir_match_edgehalf_name' : H.name -> H.name -> T -> T

  (* match edge name and data *)
  val match_edgehalf : H.name * G.EData.data 
      -> H.name * G.EData.data 
      -> T -> T
  (* requires that pat and tgt Ehn are the same half, (both Src or both Tgt) *)
  val dir_match_edgehalf : H.name * G.EData.data 
      -> H.name * G.EData.data 
      -> T -> T
  val match_edge : bool (* matched in reverse direction *) 
      -> E.name * G.EData.data 
      -> E.name * G.EData.data 
      -> T -> T
  val add_plugging : V.name -> V.name -> T -> T  
      
  (* match up vertices,  *)
  val match_vertex_data : G.VData.data -> G.VData.data -> T -> T
  val match_vertex_name : V.name -> V.name -> T -> T
  val match_vertex : V.name * G.VData.data
                     -> V.name * G.VData.data 
                     -> T -> T

  (* match pattern boundary point to target vertex/edge-point *)
  val match_boundary : V.name -> V.name -> T -> T


  (* apply substitutions for data that may occur in vertices/edges *)
  val datasubst : T -> G.T -> G.T 
  (* instantiate a graph using the match, produces exactly the matched lhs
     component, (or equivalent upto data equivalence) *)
  val inst_pat_graph : T -> G.T -> G.T 
  (* instantiate a substitution graph for the match: expand !-boxes, etc,
     keeping id map on names of boundary  *)
  val inst_replacement_graph : T -> G.T -> G.T 
  (* things involved in inst_replacement_graph *)
  (* given matching pattern edge-half and targetedge-half, return the 
     other pattern-edge-half (for a different edge), which would need to 
     plugged to the first pattern edge half in order to match the target 
     edge. *)
  val other_plugging_half_of_same_edge : T -> H.name 
      -> H.name -> H.name option;
  (* given a target edge, returns the pair of pattern edge-halfs that match 
     it's side (if they are different, and thus if the pattern half-edges 
     should be plugged together) *)
  val plugging_halfs_of_same_edge : 
      T -> E.name -> (H.name * H.name) option;   
  val rename_and_merge_edgepoint_vertices : T -> G.T -> G.T;
  val merge_edgehalfs : T -> G.T -> G.T 

  (* bbox stuff *)
  val get_bbmap : T -> BMap.T
  val add_bbox_drop : B.name -> T -> T;
  val add_bbox_kills : B.name list -> T -> T;
  val add_bbox_map : B.name -> B.name -> T -> T;
  val add_bbox_copy : (B.name (* old bbname *)
                 * (V.name V.NTab.T (* vertex copying *)
                      * E.name E.NTab.T) (* edge copying *)
                 * B.name) (* new bbox name *) 
                -> T -> T;

  (* Renaming *)
  val try_rename1_tgt_vname : V.name -> V.name 
                              -> T -> T option; 
  val try_rename1_pat_vname : V.name -> V.name 
                              -> T -> T option; 

  (* Compound renaming *)
  structure PatVertexCRnm : COMPOUND_RENAMING 
  where type obj = T and type renaming = V.Rnm.T;
  structure TgtVertexCRnm : COMPOUND_RENAMING 
  where type obj = T and type renaming = V.Rnm.T;
  (* TODO: add renaming for vertices and bboxes *)

  (* Pretty printing *)
  val pretty : T -> Pretty.T
  val print : T -> unit
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor Match(G : BANG_GRAPH) 
(* : MATCH *) 
= struct
  
  structure G = G;
  structure BBMatch = BBoxMatchFun(G);
  
  structure OVData = G.OVData;
  
  datatype T = Match of 
           {
            (* names context for fresh names when copying bboxes in pat *)
            names : (V.NSet.T * E.NSet.T * B.NSet.T),
            (* Internal vertex bijection from pat to tgt *)
            viso : VInjEndo.T,
            (* map for external vertices: (many to 1) pat to tgt vertices. This 
               does not include the vertcies in pluggings. *)
            evmap: VMap.T,
            (* a 1-1 map between external pattern vertices that should be 
               plugged together by the match  *)
            pluggings: VInjEndo.T,
            (* 1-1, inj half-edge mapping *)
            emap: HInjEndo.T,
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
               B.NSet.union_merge (G.get_bbnames pat) 
                  (B.NSet.union_merge (G.get_bbnames tgt) bs)),
      viso = VInjEndo.empty,
      evmap = VMap.empty,
      pluggings = VInjEndo.empty,
      emap = HInjEndo.empty,
      bbmatch = BBMatch.init_from_pat pat,
      subst = G.Param.empty_subst
    };
  
  (* updates *)
  fun update_names f (Match rep) = 
      Match {names = f (#names rep),
             evmap = #evmap rep, 
             pluggings = #pluggings rep,
             emap = #emap rep,
             viso = #viso rep,
             bbmatch = #bbmatch rep,
             subst = #subst rep};
  fun update_evmap f (Match rep) = 
      Match {names = #names rep,
             evmap = f (#evmap rep), 
             pluggings = #pluggings rep,
             emap = #emap rep,
             viso = #viso rep,
             bbmatch = #bbmatch rep,
             subst = #subst rep};
  fun update_emap f (Match rep) = 
      Match {names = #names rep,
             evmap = #evmap rep, 
             pluggings = #pluggings rep,
             emap = f (#emap rep),
             viso = #viso rep,
             bbmatch = #bbmatch rep,
             subst = #subst rep};
  fun update_viso f (Match rep) = 
      Match {names = #names rep,
             evmap = #evmap rep, 
             pluggings = #pluggings rep,
             emap = #emap rep,
             viso = f (#viso rep),
             bbmatch = #bbmatch rep,
             subst = #subst rep};
  fun update_bbmatch f (Match rep) = 
      Match {names = #names rep,
             evmap = #evmap rep, 
             pluggings = #pluggings rep,
             emap = #emap rep,
             viso = #viso rep,
             bbmatch = f (#bbmatch rep),
             subst = #subst rep};
  fun update_subst f (Match rep) = 
      Match {names = #names rep,
             evmap = #evmap rep, 
             pluggings = #pluggings rep,
             emap = #emap rep,
             viso = #viso rep,
             bbmatch = #bbmatch rep,
             subst = f (#subst rep)};
  fun update_pluggings f (Match rep) = 
      Match {names = #names rep,
             evmap = #evmap rep, 
             pluggings = f(#pluggings rep),
             emap = #emap rep,
             viso = #viso rep,
             bbmatch = #bbmatch rep,
             subst = #subst rep};
             
  fun get_evmap (Match rep) = #evmap rep;
  fun get_pluggings (Match rep) = #pluggings rep;
  fun get_emap (Match rep) = #emap rep;
  fun get_viso (Match rep) = #viso rep;
  fun get_names (Match rep) = #names rep;
  
  val get_vnames = #1 o get_names;
  val get_enames = #2 o get_names;
  val get_bbnames = #3 o get_names;

  fun get_subst (Match rep) = #subst rep;

  val set_names = update_names o K;
  val set_subst = update_subst o K;
  val set_viso = update_viso o K;
  val set_evmap = update_evmap o K;
  val set_pluggings = update_pluggings o K;
  
  fun get_bbmatch (Match rep) = #bbmatch rep;
  val get_bbmap = BBMatch.get_bbmap o get_bbmatch;
  
  val add_bbox_drop = update_bbmatch o BBMatch.add_drop;
  val add_bbox_kills = update_bbmatch o BBMatch.add_kills;
  val add_bbox_map = update_bbmatch oo BBMatch.add_bbmap;
  val add_bbox_copy = update_bbmatch o BBMatch.add_copy;
  
  fun add_vname n = update_names (fn (vns,ens,bns) => 
                                     (V.NSet.add n vns,ens,bns));
  fun add_ename n = update_names (fn (vns,ens,bns) => 
                                     (vns,E.NSet.add n ens,bns));
  fun add_bname n = update_names (fn (vns,ens,bns) => 
                                     (vns,ens,B.NSet.add n bns));

  fun add_plugging v1 v2 m = 
      m |> update_pluggings (VInjEndo.add v1 v2)
        |> update_evmap (VMap.surj_del_dom v1 o VMap.surj_del_dom v2);

  (* renaming *)
  (* TODO: rename within the bang-box *)
  fun try_rename1_tgt_vname n1 n2 m =
      (case (VInjEndo.try_rename1_cod n1 n2 (get_viso m)) of NONE => NONE
       | SOME viso2 => 
      (case (VMap.try_rename1_cod n1 n2 (get_evmap m)) of NONE => NONE
       | SOME evmap2 => 
       SOME (m |> set_viso viso2 |> set_evmap evmap2 |> add_vname n2)));

  fun try_rename1_pat_vname n1 n2 m =
      (case (VInjEndo.try_rename1_dom n1 n2 (get_viso m)) of NONE => NONE
       | SOME viso2 => 
      (case (VMap.try_rename1_dom n1 n2 (get_evmap m)) of NONE => NONE
       | SOME evmap2 => 
       SOME (m |> set_viso viso2 |> set_evmap evmap2 |> add_vname n2)));

  (* TODO: add renamings for edges and bboxes *)
  (* 
  fun try_rename1_tgt_ename n1 n2 m =
  fun try_rename1_pat_ename n1 n2 m =
  fun try_rename1_tgt_bname n1 n2 m =
  fun try_rename1_pat_bname n1 n2 m =
  *)

  (* compound renamings pat and tgt vertices *)
  structure TgtVertexCRnm = CompoundRenamingFun(
  struct 
    structure Nm = V
    type obj = T
    val try_rename1 = try_rename1_tgt_vname;
    val get_nameset = get_vnames
  end);
  
  structure PatVertexCRnm = CompoundRenamingFun(
  struct 
    structure Nm = V
    type obj = T
    val try_rename1 = try_rename1_pat_vname;
    val get_nameset = get_vnames
  end);
  

  (* TODO: add: 
  structure PatEdgeCRnm : COMPOUND_RENAMING 
  where type obj = T and type renaming = E.Rnm.T
  structure PatBBoxCRnm : COMPOUND_RENAMING 
  where type obj = T and type renaming = B.Rnm.T

  structure TgtEdgeCRnm : COMPOUND_RENAMING 
  where type obj = T and type renaming = E.Rnm.T
  structure TgtBBoxCRnm : COMPOUND_RENAMING 
  where type obj = T and type renaming = B.Rnm.T
  *)

  (* True iff egde map already contains this mapping *)
  fun emap_contains m peh_teh = 
      HInjEndo.contains (get_emap m) peh_teh;
  
  fun emap_contains_pat_ehn m pehn = 
      (case (HInjEndo.lookup_domf (get_emap m) pehn) of NONE => false
       | SOME _ => true);

  (* exception raised when things do not match *)
  exception match_exp of unit;
  
  (* checks that other half of the same patten edge is not mapped to 
     something else *)
  fun other_pat_half_is_consistant m peh teh = 
      (case HInjEndo.lookup_domf (get_emap m) (H.other peh) of 
         SOME teh2 => E.name_eq (H.get_ename teh2, H.get_ename teh)
       | NONE => true);
  
  (* match edge data, raising match_exp is data doesn't match *)
  fun match_edge_data ped ted m = 
      update_subst (G.Param.match_edge ped ted) m 
      handle G.Param.match_exp _ => raise match_exp ();
  
  (* *)
  fun match_edgehalf_name' peh teh m =  
      update_emap (HInjEndo.add peh teh) m 
      handle HInjEndo.add_exp (_,_,_) => raise match_exp ();
  
  (* *)
  fun dir_match_edgehalf_name' peh teh m =  
      if (H.same_half peh teh) 
      then (update_emap (HInjEndo.add peh teh) m
        handle HInjEndo.add_exp (_,_,_) => raise match_exp ())
      else raise match_exp ();
  
  (* *)
  fun match_edgehalf_name peh teh m =  
      if other_pat_half_is_consistant m peh teh 
      then match_edgehalf_name' peh teh m 
      else raise match_exp ();
  
  fun dir_match_edgehalf_name peh teh m =
      if (H.same_half peh teh) 
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
      let val (peh1,peh2) = if rev then (H.mk_end pe, H.mk_start pe)
                                   else (H.mk_start pe, H.mk_end pe)
      in
        m |> match_edgehalf_name' peh2 (H.mk_end te)
          |> match_edgehalf_name' peh1 (H.mk_start te)
          |> match_edge_data ped ted
      end;
  
  
  (* *)
  fun match_vertex_data pvd tvd m =
      update_subst (G.Param.match_vertex pvd tvd) m 
      handle G.Param.match_exp _ => raise match_exp ();
  
  fun match_vertex_name pv tv m =
      update_viso (VInjEndo.add pv tv) m
      handle VInjEndo.add_exp _ => raise match_exp ();
  
  fun match_vertex (pv,pvd) (tv,tvd) m =
      m |> match_vertex_name pv tv 
        |> match_vertex_data pvd tvd; 
  
  fun match_boundary pb tb m =                   
      m |> update_evmap (VMap.add_to_dom pb tb);
            
  (* apply data substs of a match to all vertices and edges in graph *)
  fun datasubst m g = G.apply_data_subst (get_subst m) g;

  (* breaks invariant of edge-points having directed arity <= 1; when 
     two edge-points have the same name they get merged. *)
  (* CHECK: seems to assume that codv must be named fresh w.r.t. graph??? *)
  fun rename_and_merge_edgepoint_vertices m g = 
      V.NTab.fold 
        (fn (codv,domvset) => 
          V.NSet.fold (fn v2 => G.merge_2vertices v2 codv) domvset
          o snd o G.add_named_vertex codv OVData.EVert)
        (VMap.get_codtab (get_evmap m))
        g;

  (* plug each pair of boundary vertices that need to be plugged. *)
  fun merge_edgehalfs m g = 
      g |> V.NTab.fold (fn (v1,v2) => snd o snd o G.plug_by_boundary v1 v2)
            (VInjEndo.get_domtab (get_pluggings m));
  
  (* given that pehn matches tehn, optionally return the opposite half-edge 
     that needs to be plugged to it (if it exists, else NONE). *)
  fun other_plugging_half_of_same_edge m pehn tehn =  
      (case H.NTab.lookup (HInjEndo.get_codtab (get_emap m)) (H.other tehn) 
        of NONE => NONE
         | SOME pehn2 => 
           if E.name_eq (H.get_ename pehn2, H.get_ename pehn)
           then NONE
           (* two different edges halfs got mapped to this edge, so the
              half-edges they go to (they must to half edges) should be
              plugged. *)
           else SOME pehn2);
  
  fun plugging_halfs_of_same_edge m te = 
      let val emap_cod = HInjEndo.get_codtab (get_emap m);
          val teh_start = H.mk_start te;
      in 
        (case H.NTab.lookup emap_cod teh_start 
           of NONE => NONE
            | SOME peh_start => 
            (case other_plugging_half_of_same_edge m peh_start teh_start
               of NONE => NONE
                | SOME peh_end => SOME (peh_start,peh_end)))
      end;   
           
  (* rename all vertices in the subst graphs - that is the pattern (lhs) and it's replacement (rhs), and in the match accordingly. This is in order that the substitution graph and match can be displayed in the same name-space as the target graph. (e.g. for highlighting the part of the graph being matched/replaced) *)      
  (* fun rename_match_and_subst_graphs (m,g_lhs,g_rhs) =
      let 
        val (vnames, enames, bnames) = (get_names m)

        val (vrn,g) = 
            g |> G.VertexCRnm.rename vrn
            g |> G.EdgeCRnm.rename (B.Rnm.init_from_avoids bnames);
            (brn,g) |> G.BBoxCRnm.rename (B.Rnm.init_from_avoids bnames);

        val m2 = 
            m |> PatVertexCRnm.do_renaming vrn
              |> PatVertexCRnm.do_renaming ern
              |> PatVertexCRnm.do_renaming brn;
      in 
        (m2, g2)
      end;
   *)

  (* instantiate a graph using the match, produces exactly the matched lhs
     component, or equivalently. TODO: edge names? *)
  fun inst_pat_graph m g = 
        g |> BBMatch.inst_pat_bbox_match (get_bbmatch m)
          |> merge_edgehalfs m
          |> rename_and_merge_edgepoint_vertices m
          |> datasubst m;

  (* instantiate a substitution graph for the match: expand !-boxes, etc,
     keeping id map on names of boundary. 
     Note: renames graph to avoid any names in match "m", so that boundary
     vertices can be renamed/merged without danger of clash to existing names.
     This also ensures that if you add this graph to the matched one, then 
     there will be no name-clash, except at the intended bits (edge-points to 
     be merged)
  *)
  (* I thought I could get away with dynamic renaming of the graph; but 
     giving up on that for now. And instead will assume (next function) that 
     g is name-fresh from tgt. This is the old function: 

   fun inst_replacement_graph m g = 
      let 
        val (vrn,g) =
          g|> G.VertexCRnm.rename (V.Rnm.mk_from_avoids (get_vnames m));
        val (ern,g) = 
          g |> G.EdgeCRnm.rename (E.Rnm.mk_from_avoids (get_enames m));
        val ((vnames,enames),g) = 
          ((V.Rnm.get_avoids vrn,E.Rnm.get_avoids ern),g) |> 
          BBMatch.inst_replacement_bbox_match (get_bbmatch m);
        val m' = m |> set_names (vnames,enames,get_bbnames m); 
      in g |> rename_and_merge_edgepoint_vertices m'
           |> datasubst m'
      end;
  *)

  fun inst_replacement_graph m g = 
      let
        val ((vnames,enames),g) = 
            BBMatch.inst_replacement_bbox_match (get_bbmatch m)
              ((get_vnames m,get_enames m),g)
        val m' = m |> set_names (vnames,enames,get_bbnames m); 
      in g |> rename_and_merge_edgepoint_vertices m'
           |> merge_edgehalfs m'
           |> datasubst m'
      end;
  
  (* pretty printing *)
  fun pretty_names (vs,es,bs) = 
    Pretty.block 
      [Pretty.str "Vs: ", V.NSet.pretty vs,
       Pretty.str "; Es: ", E.NSet.pretty es,
       Pretty.str "; Bs: ", B.NSet.pretty bs];
  
  fun pretty_pluggings pluggings = 
      Pretty.list "[" "]" 
        (V.NTab.fold (fn (a,b) => fn l => 
          [Pretty.block [Pretty.str "(", V.pretty_name a, Pretty.str "<->",
           V.pretty_name b, Pretty.str ")"]] @ l)
          (VInjEndo.get_domtab pluggings)
          []);
  
  fun pretty m = 
      Pretty.chunks 
      ([Pretty.str "Match: ",
        (Pretty.indent 2 o Pretty.chunks)
        ([pretty_names (get_names m)]
         @ [Pretty.block 
             [Pretty.str "Edge-point vertex Map: ",
              VMap.pretty (get_evmap m)]]
         @ [Pretty.block 
             [Pretty.str "Pluggings: ",
              pretty_pluggings (get_pluggings m)]]
         @ [BBMatch.pretty (get_bbmatch m)]
         @ [Pretty.block 
             [Pretty.str "Internal Vertex InjEndo: ",
              VInjEndo.pretty (get_viso m)]]
          @ [HInjEndo.pretty (get_emap m)])
      ]);
  
  val print = Pretty.writeln o pretty;
  
  structure Sharing : MATCH_SHARING 
  = struct
    structure G = G.SharingOGraph
    structure BBMatch = BBMatch.Sharing
    type T = T;
  end;
    
end;
