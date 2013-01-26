(*THIS IS A TEST*)
(* The inner matching loop *)
(* see docs/matching_algo for details *)
functor MJB_SimpleInnerMatchSearchParallel(
    structure MatchState : OGRAPH_MATCH_STATE
) : INNER_MATCH_SEARCH
= struct
  local
  (*For all of the future calls that I need!*)
  open MJB_Simple_Future;
  in
  
  structure Log : LOG = Log(val init_level = 0);
  
  structure G = MatchState.G
  structure MatchState = MatchState
  
  exception InnerMatchError of string
  
  (*For ease of use*)
  structure Future = MJB_Simple_Future;
  structure Par_Seq = MJB_Safe_Parallel_Seq;
  structure Par_Tree = MJB_Par_Tree;
  structure Compactor = MJB_Compactor;
  structure Stopwatch = MJB_Stopwatch;
  
  (*Log the future data*)
  fun log_future_data data lvl =
    Log.logf lvl future_data_to_string data
  
  (*Use these to print in the right order...*)
  fun logf data lvl =
    (
    log_future_data data lvl;
    Log.logf lvl
    )
  fun log data lvl =
    (
    log_future_data data lvl;
    Log.log lvl
    )
  (*All log functions adjusted to log the future data first*)
  fun log_p data pretty_f lvl name =
    (
    log_future_data data lvl;
    Log.logf lvl (fn g => Pretty.string_of
        (Pretty.chunks [Pretty.str (name^":"),(pretty_f g)]))
    )
  fun log_graph data lvl =
    (
    log_future_data data lvl;
    log_p data G.pretty lvl
    )
  fun log_vset data lvl =
    (
    log_future_data data lvl;
    log_p data V.NSet.pretty lvl
    )
  fun log_vmap data lvl =
    (
    log_future_data data lvl;
    log_p data VInjEndo.pretty lvl
    )
  
  (*Indenting for printing depending on the level of recursion*)
  fun ind 0 str = str
    | ind n str = ind (n-1) (" "^str)
  
  fun add_edge_to_match data lvl (ep,vp) (et,vt) ms = let
    val _ = logf data 2 (fn () => ind lvl
      ">> Edge "^(E.string_of_name ep)^" -> "^(E.string_of_name et)) ()
    val _ = logf data 2 (fn () => ind lvl
      ">> Node "^(V.string_of_name vp)^" -> "^(V.string_of_name vt)) ()
  in
    MatchState.add_edge_to_match (ep,vp) (et,vt) ms
  end
  fun add_circle_to_match data lvl (cp,ct) ms = let
    val _ = logf data 2 (fn () => ind lvl
      ">> Circle "^(V.string_of_name cp)^" -> "^(V.string_of_name ct)) ()
  in
    MatchState.add_circle_to_match (cp,ct) ms
  end
  fun add_vertex_to_match data lvl (vp,vt) ms = let
    val _ = logf data 2 (fn () => ind lvl
      ">> Node "^(V.string_of_name vp)^" -> "^(V.string_of_name vt)) ()
  in
    MatchState.add_vertex_to_match (vp,vt) ms
  end
  fun log_match_fail_v1 data lvl msg v =
    log data 2 (ind lvl "!! "^msg^" ("^(V.string_of_name v)^")")
  fun log_match_fail_v2 data lvl msg v1 v2 =
    log data 2 (ind lvl "!! "^msg^" ("^(V.string_of_name v1)^" -> "^(V.string_of_name v2)^")")
  fun log_match_fail_e2 data lvl msg e1 e2 =
    log data 2 (ind lvl "!! "^msg^" ("^(E.string_of_name e1)^" -> "^(E.string_of_name e2)^")")
  (* this version is for things that failed straight away *)
  (* (ie: when branches aren't killed, but are never created) *)
  fun log_match_fail_e2' data lvl msg e1 e2 =
    log data 4 (ind lvl "!! "^msg^" ("^(E.string_of_name e1)^" -> "^(E.string_of_name e2)^")")
  
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

  fun match_circles' data lvl ms = let
    val _ = log data 2 (ind lvl "== Matching next circle")
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
             val _ = logf data 4 (fn () => ind lvl "Attempting to match circle ("^
                (V.string_of_name cp)^ ","^(E.string_of_name ep)^") against ("^
                (V.string_of_name ct)^","^(E.string_of_name et)^")") ()
             val ((etdir,etdata),_) = G.get_edge tgt et
           in case (epdir = etdir,
                    perhaps_apply [G.VData.match_data (cpdata,ctdata),
                                   G.EData.match_data (epdata,etdata)] subst)
                of (true, SOME subst') =>
                      match_circles' data (lvl+1)
                          (ms |> MatchState.update_u_circles (V.NSet.delete cp)
                              |> add_circle_to_match data lvl (cp,ct)
                              |> MatchState.set_match_subst subst')
                 | (false, _) =>
                     (log_match_fail_e2' data lvl "Circle directedness mismatch" ep et;
                      Seq.empty)
                 | (_, NONE) =>
                     (log_match_fail_e2' data lvl "Circle EData mismatch" ep et;
                      Seq.empty)
           end
        (*BRANCH POINT*)
         in
          Seq.maps m_circ (V.NSet.fold Seq.cons (u_circles_in_tgt ms) Seq.empty)
         end
       | NONE => (* no circles left to match, return match state *)
                 (log data 2 "== All circles matched"; Seq.single ms)
  end
  val match_circles = match_circles' (Future.new_future_data (SOME "<-Circles")) 0
  
  
  (*****************************************)
  (************* MAIN MATCHING *************)
  (*****************************************)
  
  
  (*Analysis tools...*)
  (*
  val tryadd_wire_c = Unsynchronized.ref 0;
  val match_and_schedule_new_c = Unsynchronized.ref 0;
  val match_main_c = Unsynchronized.ref 0;
  val continue_matching_from_c = Unsynchronized.ref 0;
  val match_pending_c = Unsynchronized.ref 0;
  val match_nhd_c = Unsynchronized.ref 0;
  *)
  (* Try to recursively add wire to matching, starting with the given head
     vertex and edge. Return NONE on failure. *)
  fun tryadd_wire data lvl ms (vp,ep) (vt,et) = let
    (*val _ = (tryadd_wire_c := (!tryadd_wire_c) + 1; ());*)
    (* val _ = PolyML.print("Within tryadd_wire"); *)
    val _ = logf data 4 (fn () => ind lvl "Attempting to match ("^
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
        then SOME (ms |> add_edge_to_match data lvl (ep,new_vp) (et,new_vt)
                      |> MatchState.update_ps_nodeverts (V.NSet.add new_vp)
                      |> MatchState.set_match_subst e_subst)
        else (log_match_fail_v2 data lvl "Match conflict" new_vp new_vt; NONE)
      else
        if VInjEndo.cod_contains vmap new_vt
        then (* this is not a "valid wire matching" *)
          if VInjEndo.contains vmap (new_vp, new_vt)
          then raise ERROR "Hit the same match twice; this should not happen"
          else (log_match_fail_v2 data lvl "Wire endpoint already matched"
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
                    let (*val _ = PolyML.print("Calling tryadd_wire from tryadd_wire")*) in
                    tryadd_wire data lvl
                      (ms |> add_edge_to_match data lvl (ep,new_vp) (et,new_vt)
                          |> MatchState.update_u_wireverts (V.NSet.delete new_vp)
                          |> MatchState.set_match_subst v_subst)
                      (new_vp,new_ep) (new_vt,new_et) end
                 (* pat is internal, tgt is bdry. fail *)
                  | (SOME _, NONE) =>
                      (log_match_fail_v2 data lvl "Tgt wire too short" new_vp new_vt; NONE)
                 (* pat is boundary. add edge to match and finish *)
                  | (NONE, _) =>
                    SOME (ms |> add_edge_to_match data lvl (ep,new_vp) (et,new_vt)
                             |> MatchState.update_u_wireverts (V.NSet.delete new_vp)
                             |> MatchState.set_match_subst v_subst)
              else (* new_vp and new_vt are node-vertices *)
                if V.NSet.contains (MatchState.get_u_nodeverts ms) new_vp
                then SOME (ms |> add_edge_to_match data lvl (ep,new_vp) (et,new_vt)
                              |> MatchState.update_u_nodeverts (V.NSet.delete new_vp)
                              |> MatchState.update_p_nodeverts (V.NSet.add new_vp)
                              |> MatchState.update_ps_nodeverts (V.NSet.add new_vp)
                              |> MatchState.set_match_subst v_subst)
                else (* if this node-vert isn't scheduled for matching, leave its wire alone *)
                     (log_match_fail_v1 data lvl "Not scheduled" new_vp; NONE)
             | NONE => (* vertex data did not match: new_vp -> new_vt *)
                       (log_match_fail_v2 data lvl "OVData mismatch" new_vp new_vt; NONE)
    end
     | NONE => (* edge data did not match: ep -> et *)
               (log_match_fail_e2 data lvl "EData mismatch" ep et; NONE)
  end
  
  (*
  PARALLELISATION DETAILS
  
  The two functions which can be parallelised across in their application are
  m_nodevert and match_nhd
  
  Note in particular that match_nhd is quite hard to separate, since it can recur on itself...
  
  Hence, the appropriate indexer for theses functions is:
  *)
  
  datatype node_functions =
        (*Requires the future data, the depth, the pattern vertex, the target vertex*)
        Match_Node_Vertex of MatchState.T * Future.future_data * int * V.name * V.name
        (*Requires the future data, the depth, and the pattern vertex*)
      | Match_Neighbourhood of MatchState.T * Future.future_data * int * V.name
        (*Try mapping across the *)
      | Map_Try_Add_Wire of MatchState.T * Future.future_data * int * V.name * V.name * E.name * E.name
        (*A dummy node to act as the root*)
      | Root of MatchState.T
      
  (*Apply the tryadd_wire algorithm across the list of edges...*)
  fun map_tryadd_wire ms data lvl nvp nvt ep et = case tryadd_wire data lvl ms (nvp,ep) (nvt,et)
             of SOME ms' => [Par_Tree.Node (Match_Neighbourhood (ms',data,lvl,nvp))]
              | NONE => []
      
  (*Counters for the histogram (showing the coarseness of nodes)*)
  (*
  val c1_10 = Unsynchronized.ref 0;
  val c10_100 = Unsynchronized.ref 0;
  val c100_1000 = Unsynchronized.ref 0;
  val c1000_10000 = Unsynchronized.ref 0;
  val c10000_100000 = Unsynchronized.ref 0;
  val c100000_1000000 = Unsynchronized.ref 0;
  val c1000000_ = Unsynchronized.ref 0;
  *)
  fun match_pending' data doomedf match_state = let
    (*val _ = (match_pending_c := (!match_pending_c) + 1; ());*)
    (*val _ = PolyML.print("Within match pending");*)
    val _ = log data 2 "== Starting inner match";
    
    (**************************************************************************************)
    (*Match and schedule new calls:*)
    (*Try to identify all of the possible vertices that nv in the pattern graph could map to
    in the target graph*)
    (**************************************************************************************)
    fun match_and_schedule_new data lvl nv ms =
      let
        (*val _ = (match_and_schedule_new_c := (!match_and_schedule_new_c) + 1; ());*)
        (*val _ = PolyML.print("Within match_and_schedule_new");*)
        val _ = logf data 3 (fn () => ind lvl
              "== Next unmatched: "^(V.string_of_name nv)) ()
        val ms' = ms |> MatchState.update_u_nodeverts  (V.NSet.delete nv)
                       |> MatchState.update_p_nodeverts  (V.NSet.add nv)
                       |> MatchState.update_ps_nodeverts (V.NSet.add nv)
        val unm_nv_tgt = V.NSet.subtract
          (G.get_node_vertices (MatchState.get_tgt ms'))
          (VInjEndo.get_codset (MatchState.get_vmap ms'))
        (*Another cons...*)
        fun cons x xs = x::xs;
      in
        (*Create a new node ready to call m_nodevert*)
        List.map (fn tnv => Par_Tree.Node (Match_Node_Vertex (ms',data,lvl,nv,tnv)))
          (V.NSet.fold cons unm_nv_tgt [])
      end
    
    (****************************************)
    (*This is a controlling matching loop...*)
    (****************************************)
    fun match_main data lvl ms =
      let
        (*val _ = (match_main_c := (!match_main_c) + 1; ());*)
        (*val _ = PolyML.print("Within match_main");*)
        val _ = logf data 3 (fn () => ind lvl "== Entering main loop") ()
        val _ = log_graph data 4 "Pattern" (MatchState.get_pat ms)
        val _ = log_graph data 4 "Target" (MatchState.get_tgt ms)
        val _ = log_vset data 4 "U" (MatchState.get_u_nodeverts ms)
        val _ = log_vset data 4 "P" (MatchState.get_p_nodeverts ms)
        val _ = log_vset data 4 "Ps" (MatchState.get_ps_nodeverts ms)
        val _ = log_vmap data 4 "M" (MatchState.get_vmap ms)
        (* continue matching from the given partially-matched node-vertex *)
        fun continue_matching_from data nvp =
          let
            (*val _ = (continue_matching_from_c:=(!continue_matching_from_c)+1;());*)
          in
          if doomedf ms nvp
          then (log_match_fail_v1 data lvl "Doomed" nvp; [])
          else 
            let
              val ms' = ms |> MatchState.update_ps_nodeverts (V.NSet.delete nvp)
            in
              [Par_Tree.Node (Match_Neighbourhood (ms',data,lvl,nvp))] (*match_nhd data lvl nvp ms'*)
            end end
      in
        (* process next node-vertex in schedule *)  
        case V.NSet.get_min (MatchState.get_ps_nodeverts ms)        (* if there are partially matched node-verts scheduled, *)
           of SOME ps => continue_matching_from data ps                   (* traverse from there... *)
            | NONE =>
                case V.NSet.get_min (MatchState.get_u_nodeverts ms)  (* otherwise find a new unmatched vertex and match/schedule *)
                  of SOME un => match_and_schedule_new data lvl un ms
                   | NONE => (log data 3 (ind lvl "== Inner matching complete");
                       [Par_Tree.Leaf ms]) (* nothing left to do *)
      end
          
    (*****************************************************************************************)
    (*One of the node functions. Assuming nvp has been matched to a vertex in the target graph,
    try to match its neighbourhood*)
    (*****************************************************************************************)
    and match_nhd data lvl nvp ms =
      let
        val _ = logf data 3 (fn () => ind lvl
                "== Matching nhd of "^(V.string_of_name nvp)) ();
        (*val _ = (match_nhd_c:=(!match_nhd_c)+1;());*)
        (*Get the target vertex that the supplied pattern vertex was mapped to*)
        val nvt = VInjEndo.domf (MatchState.get_vmap ms) nvp;
        (*Retrieve the pattern and target graphs*)
        val (pat, tgt) = (MatchState.get_pat ms, MatchState.get_tgt ms);
        (*Returns true if an edge exists to an unmatched wire vertex. NONE otherwise*)
        fun edge_sched e = V.NSet.contains (MatchState.get_u_wireverts ms)
                                            (G.edge_get_other_vertex pat e nvp);
        (*Get the input and output edges for the vertex in the pattern graph*)
        val (_,(ins,outs)) = G.get_vertex pat nvp;
        (*Return an unmatched edge in the pattern graph coming from nvp*)
        val next_ep = E.NSet.get_exists edge_sched (E.NSet.union_merge ins outs);
        (*Return the input and output edges of the vertex nvp was mapped to*)
        val (_,(ins,outs)) = G.get_vertex tgt nvt
        (*Return all of the unmatched edges from the vertex in the target graph*)
        val unm_tgt = E.NSet.filter (not o (EInjEndo.cod_contains (MatchState.get_emap ms)))
                                      (E.NSet.union_merge ins outs)
        fun cons x xs = x::xs;                              
        (*All the unmatched edges in the target graph as a list*)
        val unm_tgt_list = E.NSet.fold cons unm_tgt [];
      in
        case next_ep (*There is an edge to match in the pattern graph*)
          of SOME ep => ((*PolyML.print("Trying wires..");*)(*map_tryadd_wire ep unm_tgt_list*)
          map (fn et => Par_Tree.Node (Map_Try_Add_Wire (ms,data,lvl,nvp,nvt,ep,et))) unm_tgt_list)
           | NONE => (
              (*PolyML.print("Going to match_main");*)
              match_main data (lvl+1) (* remove from partially matched if unm_tgt is empty *)
                      (ms |> (if E.NSet.is_empty unm_tgt
                              then MatchState.update_p_nodeverts (V.NSet.delete nvp)
                              else I)))
      end
    
    (*********************************************************************************)
    (*Given a node vertex nv in the pattern graph and a vertex in the target graph tnv,
    see if they will match*)
    (*********************************************************************************)
    fun m_nodevert data lvl nv tnv ms =
      let
        val nv_data = G.get_vertex_data (MatchState.get_pat ms) nv
      in
        case G.VData.match_data (nv_data, G.get_vertex_data (MatchState.get_tgt ms) tnv)
                                (MatchState.get_match_subst ms) (*Probably could use ms'*)
          of SOME subst =>
              (*Must continue from here until children are produced...*)
              match_main data (lvl+1) (ms |> MatchState.set_match_subst subst
                                          |> add_vertex_to_match data lvl (nv, tnv))
           | NONE => []
      end
    
    (*The dummy root node*)
    fun root ms = Par_Tree.Node (Root ms);
    
    (*val max = 1000000;*)
    val max_coarse = 50;
    val increase_coarse = 6;
    val start_coarse = 4;
    
    (*The function which can apply itself to the different nodes...*)
    fun apply_to_node node = case node of
        Root ms => (*let
          val _ = c1_10:= 0;
          val _ = c10_100:= 0;
          val _ = c100_1000 := 0;
          val _ = c1000_10000 := 0;
          val _ = c10000_100000 := 0;
          val _ = c100000_1000000 := 0;
          val _ = c1000000_ :=0;
          val _ = PolyML.print("Max coarseness: " ^ (Int.toString(max_coarse)));
          val _ = PolyML.print("Increase coarseness: " ^ (Int.toString(increase_coarse)));
          val _ = PolyML.print("Start coarseness: " ^ (Int.toString(start_coarse)));
        in
          match_main data 0 ms
        end*)
          match_main data 0 ms
      | Match_Node_Vertex (ms,data,lvl,nv,tnv) =>
        (*let
          val sw = Stopwatch.start Stopwatch.new;
          val ans = m_nodevert data lvl nv tnv ms;
          val t = Time.toMicroseconds(Stopwatch.check sw);
          val _ = if (t>max) then (
          PolyML.print("Match_Node_Vertex took: " ^ (LargeInt.toString(t)));()) else ();
        in
          ans
        end*)
        m_nodevert data lvl nv tnv ms
      | Match_Neighbourhood (ms,data,lvl,nvp) =>
        (*let
          val sw = Stopwatch.start Stopwatch.new;
          val ans = match_nhd data lvl nvp ms;
          val t = Time.toMicroseconds(Stopwatch.check sw);
          val _ = if (t>max) then (
          PolyML.print("Match_Neighbourhood took: " ^ (LargeInt.toString(t)));()) else ();
        in
          ans           
        end*)
        match_nhd data lvl nvp ms
      | Map_Try_Add_Wire (ms,data,lvl,nvp,nvt,ep,et) =>
        (*let
          val sw = Stopwatch.start Stopwatch.new;
          val ans = map_tryadd_wire ms data lvl nvp nvt ep et;
          val t = Time.toMicroseconds(Stopwatch.check sw);
          val _ = if (t>max) then (
          PolyML.print("Map_Try_Add_Wire took: " ^ (LargeInt.toString(t)));()) else ();
        in
          ans
        end*)
        map_tryadd_wire ms data lvl nvp nvt ep et
    (*
    fun apply r c n =
      let val _ = if ((!c)>=(!r)*24) then
                  (c:=0;if ((!r)<=10) then r:=(!r)+1 else ();()) else (c:=(!c+1))
      in Difference_List.list_of (apply_to_node_coarse (!r) n) end
    *)
    (*
    fun apply n = Difference_List.list_of (apply_to_node_coarse 8 n)*)
    (*The parallel tree to use!*)
    (*82426724 on 30 vertices. Best so far!*)
    val num_cores = Thread.numProcessors();
    fun apply r c n =
      let val _ = if ((!c)>=(!r)*num_cores) then
                  (c:=0;if ((!r)<=max_coarse) then r:=(!r)+increase_coarse else ();()) else (c:=(!c+1))
      in
        (*let
          val sw = Stopwatch.start Stopwatch.new;
          val ans = Difference_List.list_of (apply_to_node_coarse (!r) n)
          val t = Time.toMicroseconds(Stopwatch.check sw);
          val _ = if (t<=10) then (c1_10:=(!c1_10+1)) else ();
          val _ = if (t>10 andalso t<=100) then (c10_100:=(!c10_100+1)) else ();
          val _ = if (t>100 andalso t<=1000) then (c100_1000:=(!c100_1000+1)) else ();
          val _ = if (t>1000 andalso t<=10000) then (c1000_10000:=(!c1000_10000+1)) else ();
          val _ = if (t>10000 andalso t<=100000) then (c10000_100000:=(!c10000_100000+1)) else ();
          val _ = if (t>100000 andalso t<=1000000) then (c100000_1000000:=(!c100000_1000000+1)) else ();
          val _ = if (t>1000000) then (c1000000_:=(!c1000000_+1)) else ();
          (*PolyML.print("Time of node was: " ^ (LargeInt.toString(t)));*)
        in
          ans
        end*)
        (Par_Tree.recur_node_function apply_to_node ((!r)+1)) n
      end;
    val par_tree = Par_Tree.new data;
    val compactor = Compactor.new_dynamic (Time.fromMicroseconds 5000);
    val compactor = Compactor.set_estimated_number compactor (SOME 1);
    val compactor = Compactor.prime compactor (SOME 3) false;
    val compactor = Compactor.do_not_prime compactor;
    val par_tree = Par_Tree.set_compactor par_tree compactor;
    val par_tree = Par_Tree.set_order_matters par_tree false;
  (* match all circles, then enter main matching routine *)
  (*BRANCH POINT*)
  in (*(TextIO.print("In");Par_Seq.sequential_seq_of (
    Par_Seq.maps (fn fd => fn x => Par_Seq.of_sequential_seq fd (match_main fd 0 x)) (
    Par_Seq.of_sequential_seq data (match_circles' data 0 match_state))))*)
    Seq.flat (Seq.map (fn ms => Par_Tree.compute_tree par_tree (root ms) (apply (Unsynchronized.ref start_coarse) (Unsynchronized.ref 0))) (match_circles match_state))
  end
  
  fun match_pending doomedf match_state =
    let
      val res = match_pending' (Future.new_future_data (SOME "<-Main match")) doomedf match_state;
      (*val res = Seq.list_of res; (*Force evaluation!*)
      val res = Seq.of_list res;
      val _ = PolyML.print("1-10: " ^ (Int.toString((!c1_10))));
      val _ = PolyML.print("10-100: " ^ (Int.toString((!c10_100))));
      val _ = PolyML.print("100-1000: " ^ (Int.toString((!c100_1000))));
      val _ = PolyML.print("1000-10000: " ^ (Int.toString((!c1000_10000))));
      val _ = PolyML.print("10000-100000: " ^ (Int.toString((!c10000_100000))));
      val _ = PolyML.print("100000-1000000: " ^ (Int.toString((!c100000_1000000))));
      val _ = PolyML.print("1000000+: " ^ (Int.toString((!c1000000_))));*)
      (*val _ = Seq.list_of res;
      val _ = PolyML.print("tryadd_wire called " ^ (Int.toString(!tryadd_wire_c)) ^ " times");
      val _ = PolyML.print("match_and_schedule_new called " ^ (Int.toString(!match_and_schedule_new_c)) ^ " times");
      val _ = PolyML.print("match_main called " ^ (Int.toString(!match_main_c)) ^ " times");
      val _ = PolyML.print("continue_matching_from called " ^ (Int.toString(!continue_matching_from_c)) ^ " times");
      val _ = PolyML.print("match_pending called " ^ (Int.toString(!match_pending_c)) ^ " times");
      val _ = PolyML.print("match_nhd called " ^ (Int.toString(!match_nhd_c)) ^ " times")*)
    in
      res
    end
  
  (*****************************************)
  (********** BARE WIRE MATCHING ***********)
  (*****************************************)
  
  (* grow the single, given edge into:
      - a circle consisting of two edges for circles,
      - otherwise a chain of three edges, return the middle edge *)
  fun grow_edge data g h e = let
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
  fun match_bare_wires' data lvl ms = let
    val _ = log data 2 (ind lvl "== Starting bare wire match")
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
      fun is_candidate_for_bw data e = let
        val ((d,_),(s,t)) = G.get_edge tgt e
      in not (EInjEndo.cod_contains (MatchState.get_emap ms) e) andalso
         d = bw_dir andalso
         (G.is_boundary tgt s orelse G.is_boundary tgt t orelse    (* bare and boundary wires *)
          (G.is_wire_vertex tgt s andalso G.is_wire_vertex tgt t)) (* circles + middle edges *)
      end
      
      (* sequence of all the edges of the right kind with matching dir_or_undir *)
      val edges = E.NSet.fold_rev
        (fn e => if is_candidate_for_bw data e then Seq.cons e else I)
        (G.get_enames tgt) Seq.empty
      fun match_bw data e = let
        (* grow edge and grab relevant data *)
        val fulltgt = MatchState.get_full_tgt ms
        val ((s,mid_e,t), tgt', fulltgt') = grow_edge data tgt fulltgt e
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
             ms' |> add_vertex_to_match data lvl (bw_s,es)
                 |> add_edge_to_match data lvl (bw,bw_t) (mid_e,et)
           )
        end
      in
        case fmatch_d (MatchState.get_match_subst ms)
          of SOME new_subst =>
               let val add_bw' = add_bw new_subst in
                  case bw_dir
                    of G.Directed => match_bare_wires' data (lvl+1) (add_bw' (s,t))
                     | G.UnDirected =>
                         Seq.append (match_bare_wires' data (lvl+1) (add_bw' (s,t)))
                                    (match_bare_wires' data (lvl+1) (add_bw' (t,s)))
               end
           | NONE => Seq.empty
      end
    in
      (*Parallelise at the lower levels*)
      (*
      if (lvl<=1) then
        Par_Seq.sequential_seq_of(
        Par_Seq.maps (fn fd => fn a => Par_Seq.of_sequential_seq fd (match_bw fd a)) (
        Par_Seq.of_sequential_seq data edges))
      else*)
        Seq.maps (match_bw data) edges
        end
    (*end*)
  end
  val match_bare_wires = match_bare_wires' (Future.new_future_data (SOME "<-Bare wires")) 0
  
  structure Sharing =
  struct
    structure G = G.SharingOGraph
    structure MatchState = MatchState.Sharing
  end
  end (*End of local*)
end
