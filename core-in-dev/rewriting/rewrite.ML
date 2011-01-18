(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Computing rewrites of graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

signature REWRITE_SHARING =
sig
  type graph
  type match
end;
 
signature REWRITE
= sig
  type graph
  type match
  
  structure Sharing : RULE_SHARING
  
  (* rewriting pattern graph *)
  val rewrite : graph (* old graph *)
                -> graph (* lhs *) 
                -> match (* match of lhs *) 
                -> graph (* replace with rhs *)
                -> graph (* new, rewritten graph *)

  val rewrite' : graph (* old graph *)
                -> graph (* lhs *) 
                -> match (* match of lhs *) 
                -> graph (* replace with rhs *)
                -> graph (* new rhs, as it is named/expanded in result graph *) 
                   * graph (* new, rewritten graph *)

  (* rename subst w.r.t. matching of tgt, including expand bboxes 
     after subst, lhs graph is identical to tgt graph, and shared same boundary as rhs graph (which has also been renamed/substituted appropriately)
  *)
  val inst : match -> graph -> graph
  
  (* assume rule has been renamed (by matchify) to correspond to tgt graph,
     now add the rhs and delete the lhs *)
  val del_lhs : {lhs: graph} graph -> graph
  val add_rhs : {rhs: graph} graph -> graph

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor RewriteFun(structure Match : MATCH)
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

  fun rewrite' g m lhs rhs = 
      (* rename rhs to avoid names of "g", except for rhs  *)

  (* rename a subst graph pair to avoid all names in a graph g *)
  fun rename_gpair_apart g (lhs,rhs) = 
      let val (rn,lhs2) = G.rename_apart g lhs
          val (rn2, rhs2) = G.rename rn rhs
      in (rn2, (lhs2,rhs2)) end;
  

  (* IMPROVE: use depenv to only update changes parts of graph *)
  fun apply_vertex_data_subst match g = 
      Vertex.NSet.fold
        (RGGraph.update_vertex_data
           (Match.MCtxt.subst_in_vertex_data (Match.get_mctxt match)))
        (RGGraph.get_vnames g) g;

  
  (* FIXME: use above functions *)
  
  (* rename rule w.r.t. matching of tgt - assumes valid rule and rule is
     named distinct from tgt. *)
  (* Assumes: expanded pat in match is name disjoint from tgt! *)
  fun inst_rule rule match = 
      let 
        (* val rule = *) 
        val vmap = Match.get_vmap match;
        val emap = Match.get_emap match;
        val bbmatch = Match.get_bbmatch match;
        val names = Match.get_names match;
  

  
        val lhs = (get_lhs rule)
                    |> inst_lhs_bbox_match bbmatch
                    (* rename (merges matched leaf/boundary vertices) so they 
                       have the same names as vertices in the graph they match *)
                    (* Assumes: rule is names disjoint from tgt! *)
                    |> (Vertex.NTab.fold 
                          (fn (n1,n2) => RGGraph.rename_or_merge_vname n1 n2)
                          vmap)
                    (* rename/merge edges. note: merge only on mapping to loops *)
                    |> (Edge.Ehn.NTab.fold 
                          (fn (ehn1,ehn2) => 
                           fn lhs => 
                              let val e1 = (RGGraph.Edge.Ehn.get_ename ehn1) in
                                case RGGraph.lookup_edge lhs e1 
                                 (* ignore cases where edge has already
                                 been renamed; each edge has two halves,
                                 so it was just renamed earlier by its
                                 other half. *)
                                 of NONE => lhs
                                  | SOME _ => 
                                    (case RGGraph.try_rename1_ename 
                                            e1 (RGGraph.Edge.Ehn.get_ename ehn2) lhs
                                      of NONE => (* already renamed by other half, 
                                                  so drop the edge *)
                                         RGGraph.delete_edge e1 lhs
                                       | SOME lhs2 => lhs2)
                              end)
                          (Match.EMap.get_domtab emap))
                    (* instantiate angle expressions *)
                    |> apply_vertex_data_subst
  
        (*  *)
        val old_rhs = get_rhs rule
        val rhs = (get_rhs rule) 
                    |> inst_rhs_bbox_match names bbmatch
                    (* rename/merge boundary vertices to match in tgt graph 
                       assumes rhs names are disjoint from tgt *)
                    |> (fn g => Vertex.NSet.fold 
                          (fn n1 => RGGraph.rename_or_merge_vname 
                                      n1 (Vertex.NTab.get vmap n1))
                          (RGGraph.get_boundary g) g)
                    (* orient edges so that input/output of subgraph 
                       is maintained *)
                    |> orient_edge_dirs (RGGraph.get_in_and_out_vnames lhs)
                    (* apply angle instantiations *)
                    |> apply_vertex_data_subst
      in
        rule |> set_lhs lhs
             |> set_rhs rhs
      end;
  
  (* renamings of rule so that it macthes tgt *)
  fun rule_matches rule tgt = 
      let val (rn, rule') = (rename_rule_apart tgt rule)
      in Seq.map (inst_rule rule')
                 (matches rule' tgt)
      end;
  
  fun rule_matches_within rule vns tgt = 
      let val (rn, rule') = (rename_rule_apart tgt rule)
      in Seq.map (inst_rule rule')
                 (matches rule' (RGGraph.cut_subgraph_of_selection vns tgt))
      end;
  
  (* assume rule has been renamed (by matchify) to correspond to tgt graph,
     now add the rhs and delete the lhs *)
  fun add_rhs rule tgt = RGGraph.merge_by_vertices (get_rhs rule) tgt;
  fun del_lhs rule tgt = 
      let val lhs = (get_lhs rule) 
          val internal_lhs_vns = 
              Vertex.NSet.subtract (RGGraph.get_vnames lhs) (RGGraph.get_boundary lhs)
      in  
        (* delete all edges - can't rely on vertex deletion alone as
           edges between bounary nodes may be removed *)
        tgt |> Edge.NSet.fold RGGraph.delete_edge (RGGraph.get_enames lhs)
            |> Vertex.NSet.fold RGGraph.delete_vertex internal_lhs_vns
      end;
  
  (* rewriting pattern graph *)
  fun do_rewrite g matched_rule =
      g |> (add_rhs matched_rule)
        |> (del_lhs matched_rule);
  
  (* all rewrites of g using rule *)
  fun rewrites rule g = 
      Seq.map (do_rewrite g)
              (rule_matches rule g);
  
  fun rewrites_within rule vns g = 
      Seq.map (do_rewrite g)
        (rule_matches_within rule vns g);
  
  exception not_applicable_exp of string * T * RGGraph.T;
  
  fun fst_rewrite rule g = 
      (case Seq.pull (rewrites rule g) of 
         NONE => raise not_applicable_exp ("find_and_apply_rewrite",rule,g)
       | SOME (g2,g2s) => g2);
  
  fun fst_rewrite_within rule vns g = 
      (case Seq.pull (rewrites_within rule vns g) of 
         NONE => raise not_applicable_exp ("find_and_apply_rewrite_within",rule,
                                           RGGraph.cut_subgraph_of_selection vns g)
       | SOME (g2,g2s) => g2);
  
  val print = Pretty.writeln o pretty;
end;


structure DB_Rewrite = RewriteFun(MatchSearch);
structure Rewrite : RULE = DB_Rewrite;
