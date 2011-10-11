(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Symmetry-aware matching *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*
  This matching algorithm generates only distinct (under symmetry)
  matches of the pattern in the target graph.
  
  FIXME: !box support needed.
*)

functor SymmetryRuleMatchSearch(
  structure Match : MATCH
  structure Rule : RULE
  sharing Rule.Sharing.Graph = Match.Sharing.G
) : RULE_MATCH_SEARCH
= struct
  
  structure Rule = Rule;
  structure Match = Match;
  structure G = Match.G;
  structure V = G.OVertex;
  structure E = G.Edge;

  datatype T = 
         MState of 
         {
          match : Match.T,
          lb : V.name,
          pat : G.T,
          tgt : G.T,
          order : (V.name Rule.otype * V.name Rule.tree) list,
          avail : V.NSet.T,
          constr : Rule.constr
         }
  
  fun get_match (MState rep) = #match rep;
  fun get_lower_bound (MState rep) = #lb rep;
  fun get_pattern (MState rep) = #pat rep;
  fun get_target (MState rep) = #tgt rep;
  fun get_avail (MState rep) = #avail rep;
  fun get_order (MState rep) = #order rep;
  fun get_constr (MState rep) = #constr rep;
         
  fun update_match f (MState rep) = MState { match = f (#match rep),
                                             lb = #lb rep,
                                             pat = #pat rep,
                                             tgt = #tgt rep,
                                             order = #order rep,
                                             avail = #avail rep,
                                             constr = #constr rep}
  fun update_avail f (MState rep) = MState { match = #match rep,
                                             lb = #lb rep,
                                             pat = #pat rep,
                                             tgt = #tgt rep,
                                             order = #order rep,
                                             avail = f (#avail rep),
                                             constr = #constr rep }
                                             
  fun set_lower_bound e (MState rep) = MState { match = #match rep,
                                            lb = e,
                                            pat = #pat rep,
                                            tgt = #tgt rep,
                                            order = #order rep,
                                            avail = #avail rep,
                                            constr = #constr rep }
                       
  fun set_order e (MState rep) = MState { match = #match rep,
                                          lb = #lb rep,
                                          pat = #pat rep,
                                          tgt = #tgt rep,
                                          order = e,
                                          avail = #avail rep,
                                          constr = #constr rep }
                                          
  fun set_constr e (MState rep) = MState { match = #match rep,
                                          lb = #lb rep,
                                          pat = #pat rep,
                                          tgt = #tgt rep,
                                          order = #order rep,
                                          avail = #avail rep,
                                          constr = e }
                                            
                                            
  fun map2 f g p xq =
    Seq.make (fn () =>
      (case Seq.pull xq of
        NONE => NONE
      | SOME (x, xq') => SOME (f p x, map2 f g (g p x) xq')));
  fun maps2 f = Seq.flat ooo map2 f; 
  
  (* Like Seq.map, except it also passes the tail of the Seq. *)
  fun map' f xq =
    Seq.make (fn () =>
      (case Seq.pull xq of
        NONE => NONE
      | SOME (x, xq') => SOME (f (x,xq'), map' f xq')));
  fun maps' f = Seq.flat o map' f;
   
  (* Matches two vertices in the state. If a lower bound is set and the
     orbit is constrained,
     it will reject any target vertex below it. *) 
  fun match_vertex vp vt st = if (get_constr st)=Rule.Constrained
                         andalso V.name_ord (vt,get_lower_bound st)=LESS
                            then Seq.empty
                            else
      let val dp = G.get_vertex_data (get_pattern st) vp
          val dt = G.get_vertex_data (get_target st) vt
      in (Pretty.writeln (Pretty.block [V.pretty_name vp, Pretty.str " -> ", V.pretty_name vt,
                                        Pretty.str (if (get_constr st)=Rule.Constrained then " C: " else " U: "),
                                        V.pretty_name (get_lower_bound st)]);
          (* note: without this check, we get (almost) classical subgraphs 
             (square is subgraph of pyramid etc. - not quite K_4 yet) *)
          if (G.get_arity (get_pattern st) vp)=(G.get_arity (get_target st) vt)
          then Seq.single (st |> update_match (Match.match_vertex (vp,dp)
                                                                  (vt,dt))
                              |> update_avail (V.NSet.delete vt) ) 
          else Seq.empty )
      end handle Match.match_exp _ => Seq.empty;
  
  (* Matches two edges. *)
  fun match_edge ep et st =
      let val dp = G.get_edge_data (get_pattern st) ep
          val dt = G.get_edge_data (get_target st) et
      in  Seq.single (st |> update_match (Match.match_edge false (ep,dp) (et,dt)))
      end handle Match.match_exp _ => Seq.empty;
      
  exception zip_exp of unit;
  fun zipl (l::ls) (r::rs) = (l,r)::(zipl ls rs)
    | zipl [] _ =  []
    | zipl _ _ = raise zip_exp ();
      
  (* ASSUME all edges interchangeable *)
  (* Matches two sets of edges, assumed to point in the same direction. *)
  (* TODO: Verify assumption (safe if |patedges|<=|tgtedges| by aritychk) *)
  fun match_edgeset ps ts st =
      let val plist = E.NSet.list_of ps
          val tlist = E.NSet.list_of ts
      in fold (fn x => Seq.maps (uncurry match_edge x)) (zipl plist tlist)
                                                        (Seq.single st)
      end handle zip_exp _ => Seq.empty;

  fun edge_classes_between g v1 v2 =
      let val (ins1,outs1,undir1) = G.adj_edge_classes g v1
          val (ins2,outs2,undir2) = G.adj_edge_classes g v2
      in (E.NSet.intersect ins1 outs2,
          E.NSet.intersect ins2 outs1,
          E.NSet.intersect undir1 undir2)
      end;
      
  (* Matches vp with vt and matches all edges between the identified
     vertices and other already-matched vertices. *)
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
  
  (* Matches a boundary vertex bvp against a half-edge eht. *)
  fun bind bvp eht st =
      let val ehp =(  E.Ehn.other
                    o the o E.Ehn.NSet.get_local_bot
                    o G.get_vname_ehns (get_pattern st)) bvp
          val vt  =(  G.get_ehn_vname (get_target st)
                    o E.Ehn.other) eht
          val ((dt,Dt),(_,_)) = G.get_edge (get_target  st) (E.Ehn.get_ename eht)
          val ((dp,Dp),(_,_)) = G.get_edge (get_pattern st) (E.Ehn.get_ename ehp)
      in  Pretty.writeln (Pretty.block [V.pretty_name bvp, Pretty.str " => ", E.Ehn.pretty_name eht]);
          Seq.single (
          st |> update_match (case (dt,dp) of
            (G.Directed,G.Directed)     => (  Match.dir_match_edgehalf (ehp,Dp)
                                                                       (eht,Dt)
                                            o Match.match_boundary bvp vt)
          | (G.UnDirected,G.UnDirected) => (  Match.match_edgehalf (ehp,Dp)
                                                                   (eht,Dt)
                                            o Match.match_boundary bvp vt)
          | _                           => raise Match.match_exp () ))
          handle Match.match_exp _ => Seq.empty
      end;

  (* Match an initial orbit = vertex, setting it as a new lower bound. *)
  fun traverse_initial v st = Seq.maps (fn x => st |> match_vertex v x
                                                   |> Seq.map (set_lower_bound x))
                                       (V.NSet.seq_of (get_avail st));
  
  (* Traverse a rooted orbit tree and match all of it against
     target graph vertices. *)
  fun traverse_rooted' (Rule.Tree(r,sub)) root adj st =
      let fun descend (t::ts) adj' st = Seq.maps (descend ts adj')
                                                 (traverse_rooted' t root
                                                                   adj' st)
            | descend _ _ st = Seq.single st
          fun select ts (x,xq) = Seq.maps (descend ts xq) (anchor r x st)
      in maps' (select sub) adj end;
  
  fun traverse_rooted tree root st = 
      let val adjac = root |> V.InjEndo.domf (Match.get_viso (get_match st))
                           |> G.adj_vnames (get_target st)
                           |> V.NSet.intersect (get_avail st)
                           |> V.NSet.seq_of
      in traverse_rooted' tree root adjac st
      end;
      
  (* Traverse a necessarily rooted boundary tree and match all of it
     against target graph half-edges. *)
  fun traverse_boundary' (Rule.Tree(r,sub)) root adj st =
      let fun descend (t::ts) adj' st = Seq.maps (descend ts adj')
                                                 (traverse_boundary' t root
                                                                     adj' st)
            | descend _ _ st = Seq.single st
          fun select ts (x,xq) = Seq.maps (descend ts xq) (bind r x st)
      in maps' (select sub) adj end;
  
  fun traverse_boundary tree root st = 
      let val adjac = root |> V.InjEndo.domf (Match.get_viso (get_match st))
                           |> G.get_vname_ehns (get_target st)
                           |> E.Ehn.NSet.seq_of
      in traverse_boundary' tree root adjac st
      end;
  
  (* Traverse the head of the order tree, then proceed to traverse its tail. *)
  fun match'' st = case get_order st of [] => Seq.single (get_match st)
    | (root,tree as Rule.Tree(v1,_))::oes =>
      let val st = st |> set_order oes
      in Seq.maps match''
                  (case root of
                     Rule.Initial   c  => traverse_initial v1 (set_constr c st)
                   | Rule.Rooted (c,r) => traverse_rooted tree r (set_constr c st)
                   | Rule.Boundary  r  => traverse_boundary tree r st
                   | Rule.Boxed  (b,s) => Seq.empty )
      end; 
      
  (* Ramify by the appropriate self-applications of the rule. *)
  fun chain_match sa m =
      let fun chain_one s = m |> Match.update_evmap (V.Map.premap (V.InjEndo.destr (fst s)))
                            (*  |> Match.update_subst (compose (snd s)) *)
      in  Seq.map chain_one (Seq.of_list sa) end;
  
  fun match' st r = Seq.maps (chain_match (Rule.get_selfapps r)) (match'' st)
  
  (* Obtain a lazy list of matches, given a list of names to avoid, 
     of a rule in a target graph g. *)
  fun match names rule g = match' (MState({match = Match.init names
                                                             (Rule.get_lhs rule)
                                                             g,
                                              lb = V.default_name,
                                             pat = Rule.get_lhs rule,
                                             tgt = g,
                                           order = Rule.get_order rule,
                                           avail = G.get_vnames g,
                                          constr = Rule.Arbitrary })) rule;
  
  structure Sharing : RULE_MATCH_SEARCH_SHARING =
  struct
    structure Match = Match.Sharing;
    structure Rule = Rule.Sharing;
  end;

end;

