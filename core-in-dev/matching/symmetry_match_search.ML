

signature MATCH_SEARCH
= sig
  structure Match : MATCH 
  structure BangGraph : BANG_GRAPH
  structure Edge : EDGE
  structure Vertex : OVERTEX
  structure Rule : RULE

  (* sharing Vertex = BangGraph.RGVertex *)
  (* not clear why I need this this way round - but get type strange error the other way *)
  sharing BangGraph.SharingBangGraph = Match.Sharing.G;
  sharing Vertex.SharingWithIData = BangGraph.OVertex.SharingWithIData;
  sharing Edge.EdgeSharingWithData = BangGraph.Edge.EdgeSharingWithData;
  
  
  
end;

functor MatchSearchFun(
  structure BangGraph : BANG_GRAPH 
  structure Match : MATCH
  structure Rule : RULE
  sharing Match.Sharing.G = BangGraph.SharingBangGraph
  sharing Rule.Sharing.Graph = BangGraph.SharingBangGraph
)
= struct
  
  structure BangGraph = BangGraph;
  structure Vertex = BangGraph.OVertex;
  structure Edge = BangGraph.Edge;
  structure Rule = Rule;
  structure G = BangGraph;
  structure V = Vertex;
  structure E = Edge;
  structure Match = Match;

  datatype T = 
         MState of 
         {
          match : Match.T,
          c_verts : V.name Seq.seq,
          r_verts : V.name Seq.seq,
          pat : G.T,
          tgt : G.T,
          order : (Vertex.name option * Vertex.name Rule.tree) list,
          avail : V.NSet.T
         }
  
  fun get_match (MState rep) = #match rep;
  fun get_pattern (MState rep) = #pat rep;
  fun get_target (MState rep) = #tgt rep;
  fun get_avail (MState rep) = #avail rep;
  fun get_order (MState rep) = #order rep;
         
  fun update_match f (MState rep) = MState { match = f (#match rep),
                                             c_verts = #c_verts rep,
                                             r_verts = #r_verts rep,
                                             pat = #pat rep,
                                             tgt = #tgt rep,
                                             order = #order rep,
                                             avail = #avail rep }
  fun update_avail f (MState rep) = MState { match = #match rep,
                                             c_verts = #c_verts rep,
                                             r_verts = #r_verts rep,
                                             pat = #pat rep,
                                             tgt = #tgt rep,
                                             order = #order rep,
                                             avail = f (#avail rep) }
                                             
  fun set_c_verts e (MState rep) = MState { match = #match rep,
                                            c_verts = e,
                                            r_verts = #r_verts rep,
                                            pat = #pat rep,
                                            tgt = #tgt rep,
                                            order = #order rep,
                                            avail = #avail rep }

  fun set_r_verts e (MState rep) = MState { match = #match rep,
                                            c_verts = #c_verts rep,
                                            r_verts = e,
                                            pat = #pat rep,
                                            tgt = #tgt rep,
                                            order = #order rep,
                                            avail = #avail rep }
                                            
  fun set_order e (MState rep) = MState { match = #match rep,
                                          c_verts = #c_verts rep,
                                          r_verts = #r_verts rep,
                                          pat = #pat rep,
                                          tgt = #tgt rep,
                                          order = e,
                                          avail = #avail rep }
                                            
                                            
  fun map2 f g p xq =
    Seq.make (fn () =>
      (case Seq.pull xq of
        NONE => NONE
      | SOME (x, xq') => SOME (f p x, map2 f g (g p x) xq')));
  fun maps2 f = Seq.flat ooo map2 f; 
  
  fun map' f xq =
    Seq.make (fn () =>
      (case Seq.pull xq of
        NONE => NONE
      | SOME (x, xq') => SOME (f (x,xq'), map' f xq')));
  fun maps' f = Seq.flat o map' f;
                                            
  fun match_vertex vp vt st =
      let val dp = BangGraph.get_vertex_data (get_pattern st) vp
          val dt = BangGraph.get_vertex_data (get_target st) vt
      in (Pretty.writeln (Pretty.block [V.pretty_name vp, Pretty.str " -> ", V.pretty_name vt]);
          Seq.single (st |> update_match (Match.match_vertex (vp,dp) (vt,dt))
                         |> update_avail (V.NSet.delete vt) ) )
      end handle Match.match_exp _ => Seq.empty;
      
  fun match_edge ep et st =
      let val dp = BangGraph.get_edge_data (get_pattern st) ep
          val dt = BangGraph.get_edge_data (get_target st) et
      in  Seq.single (st |> update_match (Match.match_edge false (ep,dp) (et,dt)))
      end handle Match.match_exp _ => Seq.empty;
      
  exception zip_exp of unit;
  fun zip (l::ls) (r::rs) = (l,r)::(zip ls rs)
    | zip [] [] = []
    | zip _ _ = raise zip_exp ();
      
  fun match_edgeset ps ts st =
      let val plist = E.NSet.list_of ps
          val tlist = E.NSet.list_of ts
      in fold (fn x => Seq.maps (uncurry match_edge x)) (zip plist tlist)
                                                        (Seq.single st)
      end handle zip_exp () => Seq.empty;

  fun edge_classes_between g v1 v2 =
      let val (ins1,outs1,undir1) = G.adj_edge_classes g v1
          val (ins2,outs2,undir2) = G.adj_edge_classes g v2
      in (E.NSet.intersect ins1 outs2,
          E.NSet.intersect ins2 outs1,
          E.NSet.intersect undir1 undir2)
      end;
      
  (* Something like vertex -> state -> state Seq.seq;
     given a vertex to be added to a state, find all ways of matching up all
     the edges going from that vertex back into already-matched areas,
     possibly none. Can also perform data-matching. *)
  fun anchor vp vt st =
      let val targets = V.NSet.intersect (G.adj_vnames (get_pattern st) vp)
                                         ((V.InjEndo.get_domset
                                         o Match.get_viso
                                         o get_match) st)
          fun anchor_to vap st =
              let val (ap,bp,cp) = edge_classes_between (get_pattern st) vp vap
                  val (at,bt,ct) = edge_classes_between (get_target st) vt
                                                        ((V.InjEndo.domf
                                                        o Match.get_viso
                                                        o get_match) st vap)
              in st |> match_edgeset ap at
                    |> Seq.maps (match_edgeset bp bt)
                    |> Seq.maps (match_edgeset cp ct)
              end;
      in V.NSet.fold (fn x => Seq.maps (anchor_to x))
                     targets
                     (st |> match_vertex vp vt)
      end;

  (* NONE-rooted orbit. vertex -> state -> state Seq.seq *)
  fun traverse_first v st = Seq.maps (fn x => match_vertex v x st)
                                     (V.NSet.seq_of (get_avail st));
  
  (* need: current_eset, return_eset in state *)
  (* SOME-rooted orbit. tree -> vertex -> state -> state Seq.seq *)
  fun traverse_next' (Rule.Tree(r,sub)) root adj st =
      let fun descend (t::ts) adj' st = Seq.maps (descend ts adj')
                                                 (traverse_next' t root adj' st)
            | descend _ _ st = Seq.single st
          fun select ts (x,xq) = Seq.maps (descend ts xq) (anchor r x st)
      in maps' (select sub) adj end;
  
  fun traverse_next tree root st = 
      let val adjac = root |> V.InjEndo.domf (Match.get_viso (get_match st))
                           |> G.adj_vnames (get_target st)
                           |> V.NSet.intersect (get_avail st)
                           |> V.NSet.seq_of
      in traverse_next' tree root adjac 
                       (st |> set_c_verts adjac
                           |> set_r_verts adjac)
      end;
  
  fun match' st = if ((length o get_order) st)=0 then Seq.single (get_match st) else
      let val (root,tree as Rule.Tree(v1,_))::oes = get_order st
          val st = st |> set_order oes
      in Seq.maps match' (case root of NONE => traverse_first v1 st
                                     | _    => traverse_next tree (the root) st)
      end; 
  
  fun match names rule g = match' (MState({match = Match.init names
                                                             (Rule.get_lhs rule)
                                                             g,
                                         c_verts = Seq.empty,
                                         r_verts = Seq.empty,
                                             pat = Rule.get_lhs rule,
                                             tgt = g,
                                           order = Rule.get_order rule,
                                           avail = G.get_vnames g }));
  
end;

structure DB_SMS = MatchSearchFun(structure BangGraph = RGGraph and Match = MatchFun(RGGraph) and Rule=DB_Rule);
