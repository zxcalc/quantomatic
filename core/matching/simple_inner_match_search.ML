(* The inner matching loop *)
(* see docs/matching_algo for details *)
functor SimpleInnerMatchSearch(
    structure MatchState : OGRAPH_MATCH_STATE
) : INNER_MATCH_SEARCH
= struct
  structure Log : LOG = Log(val init_level = 0);

  structure G = MatchState.G
  structure MatchState = MatchState
  
  exception InnerMatchError of string


  fun log_p pretty_f lvl name =
    Log.logf lvl (fn g => Pretty.string_of
        (Pretty.chunks [Pretty.str (name^":"),(pretty_f g)]))
  val log_graph = log_p G.pretty
  val log_vset = log_p V.NSet.pretty
  val log_vmap = log_p VInjEndo.pretty

  fun ind 0 str = str
    | ind n str = ind (n-1) (" "^str)
  
  fun add_edge_to_match lvl (ep,vp) (et,vt) ms = let
    val _ = Log.logf 2 (fn () => ind lvl
      ">> Edge "^(E.string_of_name ep)^" -> "^(E.string_of_name et)) ()
    val _ = Log.logf 2 (fn () => ind lvl
      ">> Node "^(V.string_of_name vp)^" -> "^(V.string_of_name vt)) ()
  in
    MatchState.add_edge_to_match (ep,vp) (et,vt) ms
  end
  fun add_circle_to_match lvl (cp,ct) ms = let
    val _ = Log.logf 2 (fn () => ind lvl
      ">> Circle "^(V.string_of_name cp)^" -> "^(V.string_of_name ct)) ()
  in
    MatchState.add_circle_to_match (cp,ct) ms
  end
  fun add_vertex_to_match lvl (vp,vt) ms = let
    val _ = Log.logf 2 (fn () => ind lvl
      ">> Node "^(V.string_of_name vp)^" -> "^(V.string_of_name vt)) ()
  in
    MatchState.add_vertex_to_match (vp,vt) ms
  end
  fun log_match_fail_v1 lvl msg v =
    Log.log 2 (ind lvl "!! "^msg^" ("^(V.string_of_name v)^")")
  fun log_match_fail_v2 lvl msg v1 v2 =
    Log.log 2 (ind lvl "!! "^msg^" ("^(V.string_of_name v1)^" -> "^(V.string_of_name v2)^")")
  fun log_match_fail_e2 lvl msg e1 e2 =
    Log.log 2 (ind lvl "!! "^msg^" ("^(E.string_of_name e1)^" -> "^(E.string_of_name e2)^")")
  (* this version is for things that failed straight away *)
  (* (ie: when branches aren't killed, but are never created) *)
  fun log_match_fail_e2' lvl msg e1 e2 =
    Log.log 4 (ind lvl "!! "^msg^" ("^(E.string_of_name e1)^" -> "^(E.string_of_name e2)^")")
  
  (*****************************************)
  (************ CIRCLE MATCHING ************)
  (*****************************************)
  

  fun u_circles_in_tgt ms = let
    val tgt = MatchState.get_tgt ms
  in fold (fn (s,t,_,_) =>
       if G.is_wire_vertex tgt s andalso V.name_eq (s,t) andalso      (* is a circle *)
       not (VInjEndo.cod_contains (MatchState.get_vmap ms) s) andalso (* not matched *)
       not (VInjEndo.cod_contains (MatchState.get_vmap ms) s)         (* in target *)
       then V.NSet.add s else I) (G.get_wire_list tgt) V.NSet.empty
  end

  fun match_circles' lvl ms = let
    val _ = Log.log 2 (ind lvl "== Matching next circle")
  in
    case V.NSet.get_min (MatchState.get_u_circles ms)
      of SOME cp => let
           val (pat,tgt) = (MatchState.get_pat ms, MatchState.get_tgt ms)
           val cpdata = G.get_vertex_data pat cp
           val ep = (the o E.NSet.tryget_singleton) (G.get_in_edges pat cp)
           val ((epdir,epdata),_) = G.get_edge pat ep
           val subst = MatchState.get_match_subst ms
           fun m_circ ct = let
             val ctdata = G.get_vertex_data tgt ct
             val et = (the o E.NSet.tryget_singleton) (G.get_in_edges tgt ct)
             val _ = Log.logf 4 (fn () => ind lvl "Attempting to match circle ("^
                (V.string_of_name cp)^ ","^(E.string_of_name ep)^") against ("^
                (V.string_of_name ct)^","^(E.string_of_name et)^")") ()
             val ((etdir,etdata),_) = G.get_edge tgt et
           in case (epdir = etdir,
                    perhaps_apply [G.VData.match_data (cpdata,ctdata),
                                   G.EData.match_data (epdata,etdata)] subst)
                of (true, SOME subst') =>
                      match_circles' (lvl+1)
                          (ms |> MatchState.update_u_circles (V.NSet.delete cp)
                              |> add_circle_to_match lvl (cp,ct)
                              |> MatchState.set_match_subst subst')
                 | (false, _) =>
                     (log_match_fail_e2' lvl "Circle directedness mismatch" ep et;
                      Seq.empty)
                 | (_, NONE) =>
                     (log_match_fail_e2' lvl "Circle EData mismatch" ep et;
                      Seq.empty)
           end
         in V.NSet.fold (Seq.append o m_circ) (u_circles_in_tgt ms) Seq.empty
         end
       | NONE => (* no circles left to match, return match state *)
                 (Log.log 2 "== All circles matched"; Seq.single ms)
  end
  val match_circles = match_circles' 0
  
  
  (*****************************************)
  (************* MAIN MATCHING *************)
  (*****************************************)
  
  
  (* Try to recursively add wire to matching, starting with the given head
     vertex and edge. Return NONE on failure. *)
  fun tryadd_wire lvl ms (vp,ep) (vt,et) = let
    val _ = Log.logf 4 (fn () => ind lvl "Attempting to match ("^
                       (V.string_of_name vp)^ ","^(E.string_of_name ep)^
                       ") against ("^
                       (V.string_of_name vt)^","^(E.string_of_name et)^")") ()
    val (pat, tgt) = (MatchState.get_pat ms, MatchState.get_tgt ms)
    val e_subst_maybe = let (* check edge compatibilty and update subst *)
      val ((dir_p,data_p), _) = G.get_edge pat ep
      val ((dir_t,data_t), _) = G.get_edge tgt et
      val pat_inedge = V.name_eq (G.get_edge_target pat ep, vp)
      val tgt_inedge = V.name_eq (G.get_edge_target tgt et, vt)
    in if dir_p = dir_t andalso (            (* directedness must match and... *)
            dir_p = G.UnDirected orelse      (* undirected or... *)
            (pat_inedge = tgt_inedge))       (* directions match *)
       then G.EData.match_data (data_p,data_t) (MatchState.get_match_subst ms)
       else NONE
    end
  in
  case e_subst_maybe
    of SOME e_subst =>
    let
      val new_vp = G.edge_get_other_vertex pat ep vp
      val new_vt = G.edge_get_other_vertex tgt et vt
      val vmap = MatchState.get_vmap ms
    in
      if V.NSet.contains (MatchState.get_p_nodeverts ms) new_vp
      then
        if VInjEndo.contains vmap (new_vp, new_vt)
        then SOME (ms |> add_edge_to_match lvl (ep,new_vp) (et,new_vt)
                      |> MatchState.update_ps_nodeverts (V.NSet.add new_vp)
                      |> MatchState.set_match_subst e_subst)
        else (log_match_fail_v2 lvl "Match conflict" new_vp new_vt; NONE)
      else
        if VInjEndo.cod_contains vmap new_vt
        then (* this is not a "valid wire matching" *)
          if VInjEndo.contains vmap (new_vp, new_vt)
          then raise ERROR "Hit the same match twice; this should not happen"
          else (log_match_fail_v2 lvl "Wire endpoint already matched"
                                  new_vp new_vt;
                NONE)
        else
          case G.VData.match_data (G.get_vertex_data pat new_vp,
                                   G.get_vertex_data tgt new_vt)
                                  e_subst
            of SOME v_subst =>
              (* since data matched, new_vp is a node- or wire-vertex iff new_vt is *)
              if G.is_wire_vertex pat new_vp
              then (* new_vp and new_vt are wire-vertices *)
                case (G.wv_get_other_edge pat new_vp ep, G.wv_get_other_edge tgt new_vt et)
                 (* pat and tgt are internal. add edge to match and recurse *)
                 of (SOME new_ep, SOME new_et) =>
                   tryadd_wire lvl
                      (ms |> add_edge_to_match lvl (ep,new_vp) (et,new_vt)
                          |> MatchState.update_u_wireverts (V.NSet.delete new_vp)
                          |> MatchState.set_match_subst v_subst)
                      (new_vp,new_ep) (new_vt,new_et)
                 (* pat is internal, tgt is bdry. fail *)
                  | (SOME _, NONE) =>
                      (log_match_fail_v2 lvl "Tgt wire too short" new_vp new_vt; NONE)
                 (* pat is boundary. add edge to match and finish *)
                  | (NONE, _) =>
                    SOME (ms |> add_edge_to_match lvl (ep,new_vp) (et,new_vt)
                             |> MatchState.update_u_wireverts (V.NSet.delete new_vp)
                             |> MatchState.set_match_subst v_subst)
              else (* new_vp and new_vt are node-vertices *)
                if V.NSet.contains (MatchState.get_u_nodeverts ms) new_vp
                then SOME (ms |> add_edge_to_match lvl (ep,new_vp) (et,new_vt)
                              |> MatchState.update_u_nodeverts (V.NSet.delete new_vp)
                              |> MatchState.update_p_nodeverts (V.NSet.add new_vp)
                              |> MatchState.update_ps_nodeverts (V.NSet.add new_vp)
                              |> MatchState.set_match_subst v_subst)
                else (* if this node-vert isn't scheduled for matching, leave its wire alone *)
                     (log_match_fail_v1 lvl "Not scheduled" new_vp; NONE)
             | NONE => (* vertex data did not match: new_vp -> new_vt *)
                       (log_match_fail_v2 lvl "OVData mismatch" new_vp new_vt; NONE)
    end
     | NONE => (* edge data did not match: ep -> et *)
               (log_match_fail_e2 lvl "EData mismatch" ep et; NONE)
  end
  
  
    
  fun match_pending doomedf match_state = let
    val _ = Log.log 2 "== Starting inner match"
  
    (* main matching loop *)
    fun match_main lvl ms = let
      val _ = Log.logf 3 (fn () => ind lvl "== Entering main loop") ()
      val _ = log_graph 4 "Pattern" (MatchState.get_pat ms)
      val _ = log_graph 4 "Target" (MatchState.get_tgt ms)
      val _ = log_vset 4 "U" (MatchState.get_u_nodeverts ms)
      val _ = log_vset 4 "P" (MatchState.get_p_nodeverts ms)
      val _ = log_vset 4 "Ps" (MatchState.get_ps_nodeverts ms)
      val _ = log_vmap 4 "M" (MatchState.get_vmap ms)
      
      (* remove nv from u_nodeverts, and schedule at all sites where it matches *)
      fun match_and_schedule_new nv = let
        val _ = Log.logf 3 (fn () => ind lvl
              "== Next unmatched: "^(V.string_of_name nv)) ()
        val ms' = ms |> MatchState.update_u_nodeverts  (V.NSet.delete nv)
                     |> MatchState.update_p_nodeverts  (V.NSet.add nv)
                     |> MatchState.update_ps_nodeverts (V.NSet.add nv)
        val unm_nv_tgt = V.NSet.subtract
          (G.get_node_vertices (MatchState.get_tgt ms'))
          (VInjEndo.get_codset (MatchState.get_vmap ms'))
        val nv_data = G.get_vertex_data (MatchState.get_pat ms') nv
        fun m_nodevert tnv =
          case G.VData.match_data (nv_data, G.get_vertex_data (MatchState.get_tgt ms') tnv)
                                  (MatchState.get_match_subst ms)
            of SOME subst =>
                match_main (lvl+1) (ms' |> MatchState.set_match_subst subst
                                        |> add_vertex_to_match lvl (nv, tnv))
             | NONE => Seq.empty
      in V.NSet.fold (Seq.append o m_nodevert) unm_nv_tgt Seq.empty
      end
      
      
      (* match all scheduled wires in the neighbourhood of the given node-vertex *)
      fun match_nhd lvl ms nvp = let
        val _ = Log.logf 3 (fn () => ind lvl
              "== Matching nhd of "^(V.string_of_name nvp)) ()
        val nvt = VInjEndo.domf (MatchState.get_vmap ms) nvp
        val (pat, tgt) = (MatchState.get_pat ms, MatchState.get_tgt ms)
            
        fun edge_sched e = V.NSet.contains (MatchState.get_u_wireverts ms)
                                           (G.edge_get_other_vertex pat e nvp)
        
        (* next wire in pattern that needs to be matched *)
        val (_,(ins,outs)) = G.get_vertex pat nvp
        val next_ep = E.NSet.get_exists edge_sched (E.NSet.union_merge ins outs)
        
        (* unmatched edges in target graph *)
        val (_,(ins,outs)) = G.get_vertex tgt nvt
        val unm_tgt = E.NSet.filter (not o (EInjEndo.cod_contains (MatchState.get_emap ms)))
                                    (E.NSet.union_merge ins outs)
      in case next_ep
           of SOME ep =>
               E.NSet.fold (fn et => (
                 case tryadd_wire lvl ms (nvp,ep) (nvt,et)
                   of SOME ms' => Seq.append (match_nhd (lvl+1) ms' nvp)
                    | NONE     => I
               )) unm_tgt Seq.empty
            | NONE =>
                match_main (lvl+1) (* remove from partially matched if unm_tgt is empty *)
                        (ms |> (if E.NSet.is_empty unm_tgt
                                then MatchState.update_p_nodeverts (V.NSet.delete nvp)
                                else I))
      end
      
      
      (* continue matching from the given partially-matched node-vertex *)
      fun continue_matching_from nvp =
        if doomedf ms nvp
        then (log_match_fail_v1 lvl "Doomed" nvp; Seq.empty)
        else match_nhd lvl
              (ms |> MatchState.update_ps_nodeverts (V.NSet.delete nvp))
              nvp
    
    (* process next node-vertex in schedule *)  
    in case V.NSet.get_min (MatchState.get_ps_nodeverts ms)        (* if there are partially matched node-verts scheduled, *)
         of SOME ps => continue_matching_from ps                   (* traverse from there... *)
          | NONE => (
              case V.NSet.get_min (MatchState.get_u_nodeverts ms)  (* otherwise find a new unmatched vertex and match/schedule *)
                of SOME un => match_and_schedule_new un
                 | NONE => (Log.log 3 (ind lvl "== Inner matching complete");
                     Seq.single ms)) (* nothing left to do *)
    end (* match_main *)
  
  (* match all circles, then enter main matching routine *)
  in Seq.maps (match_main 0) (match_circles match_state)
  end
  
  
  
  (*****************************************)
  (********** BARE WIRE MATCHING ***********)
  (*****************************************)
  
  (* grow the single, given edge into:
      - a circle consisting of two edges for circles,
      - otherwise a chain of three edges, return the middle edge *)
  fun grow_edge g h e = let
    val (dd, (s,t)) = G.get_edge h e
  in if V.name_eq (s,t) then
      let
        val h = h |> G.delete_edge e
        val (b1,h) = h |> G.add_vertex G.OVData.WVert
        val (e1,h) = h |> G.add_edge dd s b1
        val (mid_e, h) = h |> G.add_edge dd b1 t
        (* now repeat on g *)
        val g = g |> G.delete_edge e
                  |> G.add_named_vertex b1 G.OVData.WVert
                  |> G.add_named_edge e1 dd s b1
                  |> G.add_named_edge mid_e dd b1 t
      in ((b1,mid_e,t),g,h)
      end
    else
      let 
        val h = h |> G.delete_edge e
        val (b1,h) = h |> G.add_vertex G.OVData.WVert
        val (b2,h) = h |> G.add_vertex G.OVData.WVert
        val (e1,h) = h |> G.add_edge dd s b1
        val (mid_e,h) = h |> G.add_edge dd b1 b2
        val (e2,h) = h |> G.add_edge dd b2 t
        val g = g |> G.delete_edge e
                  |> G.add_named_vertex b1 G.OVData.WVert
                  |> G.add_named_vertex b2 G.OVData.WVert
                  |> G.add_named_edge e1 dd s b1
                  |> G.add_named_edge mid_e dd b1 b2
                  |> G.add_named_edge e2 dd b2 t
      in ((b1,mid_e,b2),g,h)
      end
  end
  
  
  (* match the given set of bare wires, expanding wire-vertices where necessary *)
  fun match_bare_wires' lvl ms = let
    val _ = Log.log 2 (ind lvl "== Starting bare wire match")
  in
    if E.NSet.is_empty (MatchState.get_u_bare_wires ms) then Seq.single ms
    else let
      val (pat, tgt) = (MatchState.get_pat ms, MatchState.get_tgt ms)
      (* for a normalised graph, this will always be the source end *)
      val bw = (the o E.NSet.get_min) (MatchState.get_u_bare_wires ms)
      val ((bw_dir, bw_data), (bw_s, bw_t)) = G.get_edge pat bw
      val (bw_sdata, bw_tdata) = (G.get_vertex_data pat bw_s, G.get_vertex_data pat bw_t)
      
      (* candidate edges for matching are:
           - the unique edge in a circle, bare wire, or boundary wire
           - the middle edge in an interior wire *)
      fun is_candidate_for_bw e = let
        val ((d,_),(s,t)) = G.get_edge tgt e
      in not (EInjEndo.cod_contains (MatchState.get_emap ms) e) andalso
         d = bw_dir andalso
         (G.is_boundary tgt s orelse G.is_boundary tgt t orelse    (* bare and boundary wires *)
          (G.is_wire_vertex tgt s andalso G.is_wire_vertex tgt t)) (* circles + middle edges *)
      end
      
      (* return all the edges of the right kind with matching dir_or_undir *)
      val edges = E.NSet.filter (is_candidate_for_bw) (G.get_enames tgt)
      fun match_bw e = let
        (* grow edge and grab relevant data *)
        val fulltgt = MatchState.get_full_tgt ms
        val ((s,mid_e,t), tgt', fulltgt') = grow_edge tgt fulltgt e
        val ((_, et_data), (tgt_s,tgt_t)) = G.get_edge tgt' mid_e
        val (tgt_sdata,tgt_tdata) = (G.get_vertex_data tgt' tgt_s, G.get_vertex_data tgt' tgt_t)
        
        (* match data for edge, source, and target *)
        val fmatch_d = perhaps_apply [G.EData.match_data (bw_data, et_data),
                                      G.VData.match_data (bw_sdata, tgt_sdata),
                                      G.VData.match_data (bw_tdata, tgt_tdata)]
        fun add_bw new_subst = let
          val ms' = ms |> MatchState.set_tgt tgt' (* save the new, edge-homeo equivalent target *)
                       |> MatchState.set_full_tgt fulltgt'
                       |> MatchState.set_match_subst new_subst
                       |> MatchState.update_u_bare_wires (E.NSet.delete bw)
        in (fn (es,et) =>
             ms' |> add_vertex_to_match lvl (bw_s,es)
                 |> add_edge_to_match lvl (bw,bw_t) (mid_e,et)
           )
        end
      in
        case fmatch_d (MatchState.get_match_subst ms)
          of SOME new_subst =>
               let val add_bw' = add_bw new_subst in
                  case bw_dir
                    of G.Directed => match_bare_wires' (lvl+1) (add_bw' (s,t))
                     | G.UnDirected =>
                         Seq.append (match_bare_wires' (lvl+1) (add_bw' (s,t)))
                                    (match_bare_wires' (lvl+1) (add_bw' (t,s)))
               end
           | NONE => Seq.empty
      end
    in E.NSet.fold (Seq.append o match_bw) edges Seq.empty
    end
  end
  val match_bare_wires = match_bare_wires' 0
  
  structure Sharing =
  struct
    structure G = G.SharingOGraph
    structure MatchState = MatchState.Sharing
  end
end
