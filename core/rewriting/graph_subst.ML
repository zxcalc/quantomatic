(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Computing substitutions of graphs: replacing a matched graph with a 
    new graph that shares the same interface. *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature GRAPH_SUBST_SHARING =
sig
  type graph
  type match
end;
 
signature GRAPH_SUBST
= sig
  type graph
  type match
  
  structure Sharing : GRAPH_SUBST_SHARING
  sharing type graph = Sharing.graph; 
  sharing type match = Sharing.match; 
  
  (* rewriting pattern graph *)
  val rewrite : graph (* old graph *)
                -> graph (* lhs *) 
                -> match (* match of lhs (not inst) *) 
                -> graph (* replace with this rhs (not inst yet) *)
                -> graph (* new, rewritten graph *)

  val rewrite' : graph (* old graph *)
                -> graph (* lhs (not inst) *) 
                -> match (* match of lhs *) 
                -> graph (* replace with rhs (not yet inst) *)
                -> graph (* new rhs, as it is named/expanded in result graph *) 
                   * graph (* new, rewritten graph *)
  
  (* internal *)
  val datasubst_and_remove_match : match -> graph -> graph

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor GraphSubstFun(Match : MATCH)
= struct
  type match = Match.T;
  type graph = Match.G.T;
  
  structure G = Match.G;
  structure BBMatch = Match.BBMatch;
  
  structure Sharing = struct
    type match = match;
    type graph = graph;    
  end;

  fun datasubst_and_remove_match m g = 
       let val internal_matched_vns = 
              VInjEndo.get_codset (Match.get_viso m);
      in 
        (* rename rhs to avoid names of "g", except for rhs  *)
        g |> V.NSet.fold G.delete_vertex internal_matched_vns 
          |> Match.datasubst m 
      end;

  (* implicit rhs; efficiency: 
     allows lhs computation to be performed just once, then many rhs' 
     to be given.
  *)
  fun rewrite' g lhs m = 
      let val g' = datasubst_and_remove_match m g
      in (fn rhs => 
          let val rhs' = Match.inst_replacement_graph m rhs;
              (* now rename the rhs to avoid any names that were in g.
                 We don't touch names of edge-points and boundary as these are names that must also occur in g: vertices get merged on these names. 
                 
                 TODO(ldixon): remove this once layout code isn't dependent on fresh names. 
              *)
              val rhs_boundary = G.get_boundary rhs';
              val rhs_edgepoints = G.get_edgepoints rhs';
              val ignore_vset = 
                  V.NSet.union_merge rhs_boundary rhs_edgepoints; 
              val ignore_eset = E.NSet.empty;
              val v_rnm = V.Rnm.mk ignore_vset (G.get_vnames g)
                          V.NTab.empty;
              val e_rnm = E.Rnm.mk ignore_eset (G.get_enames g)
                          E.NTab.empty;
              val (_, rhs'') = rhs' |> snd o G.VertexCRnm.rename v_rnm
                                    |> G.EdgeCRnm.rename e_rnm;
          in 
            (rhs'', G.merge_by_vertices rhs'' g') end) 
      end;

 fun rewrite g lhs m = snd o (rewrite' g lhs m);

end;


