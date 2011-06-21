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
  structure V = G.Vertex;
  structure E = G.Edge;
  structure BBMatch = Match.BBMatch;
  structure BBMap = BBMatch.BBMap;
  structure BBox = G.BBox;
  
  structure Sharing = struct
    type match = match;
    type graph = graph;    
  end;

  fun datasubst_and_remove_match m g = 
       let val internal_matched_vns = 
              V.InjEndo.get_codset (Match.get_viso m);
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
      in (fn rhs => let val rhs' = Match.inst_replacement_graph m rhs in 
                    (rhs', G.merge_by_vertices rhs' g') end) 
      end;

 fun rewrite g lhs m = snd o (rewrite' g lhs m);

end;


