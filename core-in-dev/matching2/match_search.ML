(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  MatchSearch and Matching Algorithm *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
signature MATCH_SEARCH
= sig
  structure Graph : OGRAPH
  structure Edge : EDGE
  structure Vertex : OVERTEX
  structure Filter : FILTER
  
  type state
  type match
  
  val m_adj : Graph.T -> match -> Vertex.name list
  
  val match_next : state -> match Seq.seq 
  
  val match_init : Graph.T -> Graph.T -> Filter.T -> state
  
  val match : Graph.T -> Graph.T -> Filter.T -> match Seq.seq
  
end;


functor MatchSearchFun(
  structure Graph : OGRAPH
  and Filter : FILTER
  sharing Filter.Graph.SharingBasicGraph = Graph.SharingBasicGraph
) : MATCH_SEARCH
= struct
  structure Graph = Graph;
  structure Edge = Graph.Edge;
  structure Vertex = Graph.OVertex;
  structure BndIso = NameIsoFun (
    structure Dom = Vertex and Cod = Edge 
    val translate_name = Edge.mk o Vertex.dest );
  structure Filter = Filter;
  
  type match = Vertex.Amorph.T*Edge.Amorph.T*BndIso.T;
  
  datatype state = MState of ({
    last : match,
    target : Graph.T,
    pattern : Graph.T,
    fixed : (Vertex.name*Vertex.name) list,
    filter : Filter.T,
    forbidden : Vertex.NSet.T
  });
  
  fun edge2vtx_outside g s e vs = let val (_,(a,b)) = Graph.get_edge g e
                                  in if Vertex.NSet.contains s a
                                     then (b::vs)
                                     else (a::vs)
                                  end;
                          
  fun get_match (MState s) = #last s
  fun get_target (MState s) = #target s
  fun get_pattern (MState s) = #pattern s
  fun get_fixed (MState s) = #fixed s
  fun get_filter (MState s) = #filter s
  fun get_forbidden (MState s) = #forbidden s
  
  fun is_forbidden s n = Vertex.NSet.contains (get_forbidden s) n
  
  fun map_target s f = MState ({ last = get_match s,
                              target  = (get_target s) |> f,
                              pattern = get_pattern s,
                              fixed   = get_fixed s,
                              filter  = get_filter s,
                              forbidden=get_forbidden s});
                              
  fun set_match m s = MState ({ last = m,
                              target  = get_target s,
                              pattern = get_pattern s,
                              fixed   = get_fixed s,
                              filter  = get_filter s,
                              forbidden=get_forbidden s});
                              
  fun fix s f = MState ({ last = get_match s,
                              target  = get_target s,
                              pattern = get_pattern s,
                              fixed   = f::(get_fixed s),
                              filter  = get_filter s,
                              forbidden=get_forbidden s});
                              
  fun forbid s f = MState ({ last = get_match s,
                              target  = get_target s,
                              pattern = get_pattern s,
                              fixed   = get_fixed s,
                              filter  = get_filter s,
                              forbidden=(get_forbidden s)
                                        |> Vertex.NSet.add f }); 
  
            
  (* TODO: handle boundary vertices in target *)
  fun get_single_match_unify_next_edge (v,e,b) t g h x gs hs otherend = let
      val n1 = Option.valOf
              (Edge.NSet.get_local_bot gs)
      fun edge_eq en1 en2 =
             let val ((dir1,d1),_) = Graph.get_edge g en1
                 val ((dir2,d2),_) = Graph.get_edge h en2
             in dir1=dir2 andalso Edge.data_eq(d1,d2) end
      fun alleqs en1 es2 = Seq.filter (edge_eq en1) 
                                      (Edge.NSet.seq_of es2)
    in case (((Graph.is_boundary g) o (otherend n1)) g) of
       true => Seq.maps get_single_match_unify_next
                   (Seq.maps (fn n2 => Seq.single
                      ((v,
                        e |> Edge.Amorph.add n1 n2,
                        b |> BndIso.add (otherend n1 g) n2),
                        t, (* no need to unify boundary vertex *)
                        g |> Graph.delete_vertex (otherend n1 g),
                        h |> Graph.delete_edge n2,
                        x)
                       handle Vertex.Amorph.add_exp(_,_,_) => Seq.empty)
               (alleqs n1 hs)) 
     | false => Seq.maps get_single_match_unify_next
                   (Seq.maps (fn n2 => Seq.single
                      ((v |> Vertex.Amorph.add (otherend n1 g) (otherend n2 h),
                        e |> Edge.Amorph.add n1 n2,
                        b),
                       Seq.append t  (* push unification of the target vertex of
                                        this edge onto the unification queue *)
                       (Seq.single (otherend n1 g,
                                    otherend n2 h)),
                       g |> Graph.delete_edge n1,
                       h |> Graph.delete_edge n2,
                       x)
                       handle Vertex.Amorph.add_exp(_,_,_) => Seq.empty)
               (alleqs n1 hs)) end
  and get_single_match_unify_next ((v,e,b),t,g,h,x) = (case Seq.pull t of
      (* Nothing to unify; start unifying new connectivity component *)
      NONE                => let
         val gbot = the (Vertex.NSet.get_local_bot (Graph.get_vnames g))
         val hmin = Vertex.NSet.get_min (Graph.get_vnames h)
         fun maketuple n = ((v,e,b),Seq.single (gbot,n),g,h,x);
      in Seq.map maketuple (Vertex.NSet.seq_of (Graph.get_vnames h)) end 
      (* Something to unify; return lazy list of all possible next steps *)
    | SOME ((vng,vnh),t') => let
         (* point undirected edges towards us *)
         fun attract e gr = let
             val ((dir,_),_) = Graph.get_edge gr e
           in if dir=Graph.UnDirected
              then Graph.swap_edge_dir e gr
              else gr end
         val (_,(_,gout)) = Graph.get_vertex g vng
         val (_,(_,hout)) = Graph.get_vertex h vnh
         val g = g |> Edge.NSet.fold attract gout
         val h = h |> Edge.NSet.fold attract hout
         (* some other helper stuff *)
         val (gd,(gin,gout)) = Graph.get_vertex g vng
         val (hd,(hin,hout)) = Graph.get_vertex h vnh
         fun esrc n x = let val (_,(f,t)) = Graph.get_edge x n
                   in f end;
         fun esnk n x = let val (_,(f,t)) = Graph.get_edge x n
                   in t end;
      in if Vertex.data_eq (gd,hd) andalso (not (Vertex.NSet.contains x vnh))
         then case (Edge.NSet.is_empty gin,
                    Edge.NSet.is_empty hin,
                    Edge.NSet.is_empty gout,
                    Edge.NSet.is_empty hout) of
            (* no (more) edges; match up *)
            (true,true,true,true) =>
              Seq.single ((v |> Vertex.Amorph.add vng vnh,
                           e,
                           b),
                        t',
                        g |> Graph.delete_vertex vng,
                        h |> Graph.delete_vertex vnh,
                        x)
            (* some outgoing edges remaining *)
          | (_,_,false,false) =>
            get_single_match_unify_next_edge (v,e,b) t g h x gout hout esnk
            (* some incoming edges remaining *)
          | (false,false,_,_) => 
            get_single_match_unify_next_edge (v,e,b) t g h x gin hin esrc
            (* edge mismatch *)
          | _ => Seq.empty
         else Seq.empty (* Mismatching vertex data or forbidden target *)
       end handle Graph.no_such_vname_exp(_,_,_) (* consequence of multiple edges. 
                                              Vertex has already been successfully
                                              unified; skip. *)
               => get_single_match_unify_next ((v,e,b),t',g,h,x)
    ) handle Vertex.Amorph.add_exp(_,_,_) => Seq.empty
  
  (* v,e are working copies of the isomorphism to be constructed;
     t is the queue of vertex pairs in the source and target graph to be
     unified *)
  fun get_single_match_sub ((v,e,b),t,g,h,x)
  = case (Graph.is_empty g,Graph.is_empty h) of
      (* pattern graph is empty; done *)
      (true,_) => Seq.single (v,e,b)
      (* neither graph is empty; get possible next unification steps and
         proceed matching *)
    | (false,false) => Seq.maps get_single_match_sub
                             (get_single_match_unify_next ((v,e,b),t,g,h,x))
      (* target graph is empty; no match *)
    | _ => Seq.empty 
  ;
  
  fun get_single_match st = get_single_match_sub (
                        (Vertex.Amorph.empty,Edge.Amorph.empty,BndIso.empty),
                        Seq.of_list (get_fixed st),
                        get_pattern st,
                        get_target st,
                        get_forbidden st);
  
  fun m_adj g (v,e,b) =(* Vertex.NSet.of_list ( *)
                          Edge.NSet.fold
                            (edge2vtx_outside g (Vertex.Amorph.get_cod_nset v)) 
                            (BndIso.get_cod_nset b)
                            []
                       (* ) *);
                       
  fun map2 f g p xq =
    Seq.make (fn () =>
      (case Seq.pull xq of
        NONE => NONE
      | SOME (x, xq') => SOME (f p x, map2 f g (g p x) xq')));
  fun maps2 f = Seq.flat ooo map2 f;
  
  fun match_select st v = let 
           fun is_allowed a b = (not (Graph.is_boundary (get_pattern st) a
                              orelse Graph.is_boundary (get_target st)  b))
                             andalso (not (is_forbidden st b))
           fun filter_and_match w = if (is_allowed w v)
                               andalso Filter.apply (get_filter st) 
                                                    (get_pattern st)
                                                    (get_target st)
                                                    w v
                                    then ((Pretty.writeln (
                                           Pretty.block [Vertex.pretty_name w,
                                                         Vertex.pretty_name v,
                                                         Vertex.NSet.pretty (get_forbidden st)]
                                          ));let
                                           val st'= fix st (w,v)
                                           val ms = get_single_match st'
                                         in case Seq.pull ms of
                                            (* no match; quit branch *)
                                            NONE       => Seq.empty 
                                            (* got a match; party *)
                                          | SOME (m,_) => Seq.make (fn()=> SOME
                                                           (m,(match_next 
                                                               (st' |> set_match m))))
                                            end)
                                    (* match one :: match_next below it *)
                                    else Seq.empty
        in Seq.maps filter_and_match
                    ((Vertex.NSet.seq_of o Graph.get_vnames o get_pattern) st)
           end  
  and match_next st = let
           val adj = ((m_adj (get_target st)) o get_match) st
           val s = if (List.length adj = 0) 
                   then (Vertex.NSet.list_of o Graph.get_vnames o get_target) st
                   else adj
           fun remove st n = forbid st n
        in maps2 match_select remove st (Seq.filter (not o is_forbidden st)
                                                    (Seq.of_list s)
                                                    ) end;
                    
  
  fun match_init p t f = MState { last    = (Vertex.Amorph.empty,
                                             Edge.Amorph.empty,
                                             BndIso.empty),
                                  target  = t,
                                  pattern = p,
                                  fixed   = [],
                                  filter  = f,
                                  forbidden = Vertex.NSet.empty };
                                  
  val match = match_next ooo match_init;

end;