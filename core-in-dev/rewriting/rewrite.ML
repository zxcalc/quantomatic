(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Computing rewrites of graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(* 

QUESTION: are any axioms non-symmetric in application? Do all rewrites
necessarily rewrite symmetrically? or can you have lhs symmetries
without the rhs ones? (yes in general, but what about quantum
computation?)

*)

signature REWRITE_SHARING =
sig
  structure Name : NAMES_SHARING
  structure RGGraph : RG_GRAPH_SHARING
  structure Match : MATCH_SHARING
  (*structure MatchSearch : MATCH_SEARCH_SHARING *)
end
 

signature REWRITE
= sig
  
  structure Name : SSTR_NAMES
  
  structure MatchSearch : MATCH_SEARCH
  (* these are really in MatchSearch - this is just a local abbreviation *)
  structure RGGraph : RG_GRAPH
  structure Match : MATCH
  
  structure Sharing : RULE_SHARING
  sharing Sharing.Name = Name
  sharing Sharing.RGGraph = RGGraph
  sharing Sharing.Match = Match
  (* sharing Sharing.MatchSearch = MatchSearch *)
  
  sharing RGGraph.SharingBangGraph = MatchSearch.Graph.SharingBangGraph;
  (* sharing Match.Sharing = MatchSearch.Match.Sharing; *)
  
  type T (* rule *)
  
  exception bad_rule_exp of string * RGGraph.T * RGGraph.T;
  val mk : (RGGraph.T * RGGraph.T) -> T;
  
  val get_lhs : T -> RGGraph.T
  val get_rhs : T -> RGGraph.T
  
  (* rename lhs and rhs of rule to be distinct from graph *)
  val rename_rule_apart : RGGraph.T -> T -> (Vertex.renaming * Edge.renaming) * T
  
  (* swaps lhs and rhs *)
  val symmetric : T -> T
  
  (* rule is a trivial identity : lhs matches rhs *)
  val is_trivial : T -> bool
  
  (* find all matches, evaluated lazily *)
  val matches : T -> RGGraph.T -> Match.T Seq.seq
  (* gives back renamed (renamed to match graph) rules *)
  val rule_matches : T -> RGGraph.T -> T Seq.seq
  val rule_matches_within : T -> RGGraph.Vertex.NSet.T -> RGGraph.T -> T Seq.seq
  
  (* rename rule w.r.t. matching of tgt *)
  val inst_rule : T -> Match.T -> T
  
  (* assume rule has been renamed (by matchify) to correspond to tgt graph,
     now add the rhs and delete the lhs *)
  val add_rhs : T -> RGGraph.T -> RGGraph.T
  val del_lhs : T -> RGGraph.T -> RGGraph.T
  
  (* rewriting pattern graph *)
  val rewrites : T -> RGGraph.T -> RGGraph.T Seq.seq
  val rewrites_within : T -> RGGraph.Vertex.NSet.T -> RGGraph.T -> RGGraph.T Seq.seq
  
  (* apply rule which has been instantiated to this graph *)
  val do_rewrite : RGGraph.T -> T -> RGGraph.T
  
  exception not_applicable_exp of string * T * RGGraph.T;
  val fst_rewrite : T -> RGGraph.T -> RGGraph.T
  val fst_rewrite_within : T -> RGGraph.Vertex.NSet.T -> RGGraph.T -> RGGraph.T
  
  (* 
  val do_rewrite_rrhs : (* rewriting a rules rhs *)
      T (* use this rule *)
      -> Match.T (* with this matching *)
      -> T (* to rewrite the rhs of this rule *)
      -> T (* to produce this new rule *)
  
  val rewritings : T -> (MatchSearch.T * RGGraph.T) -> (MatchSearch.T * RGGraph.T) Seq.seq
  val rewritings_rrhs : T -> (MatchSearch.T * T) -> (MatchSearch.T * T) Seq.seq
  *)
  
  val pretty : T -> Pretty.T
  val print : T -> unit
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor RewriteFun(structure Match : MATCH and Rule : BASIC_RULE)
= struct
  
  structure Name = struct
  open SStrName;
  val default_name = SStrName.mk "new-rule-1"
  end
  
  structure MatchSearch = MatchSearch;
  (* local stuff for abbreviations *)
  structure RGGraph = MatchSearch.Graph;
  (* structure Match = MatchSearch.Match; *) (**)
  structure Vertex = MatchSearch.Vertex;
  structure Edge = MatchSearch.Edge;
  
  structure Sharing = struct
    structure Name = Name.Sharing
    structure RGGraph = RGGraph.SharingBangGraph
    (*structure Match = Match.Sharing*) (**)
  end
  
  datatype T = Rule of { lhs : RGGraph.T,
                         rhs : RGGraph.T,
                         lhs_aut : (RGGraph.VUmorph.T*RGGraph.EUmorph.T) list,
                         selfapps : RGGraph.VUmorph.T list };
  
  exception bad_rule_exp of string * RGGraph.T * RGGraph.T;
  fun mk (lhs,rhs) = 
      let val lhsbndry : Vertex.NSet.T = RGGraph.get_boundary lhs in
      (* check that rule is well-formed! *)
      if Vertex.NSet.eq lhsbndry (RGGraph.get_boundary rhs) then 
        let (* rename rhs; shared bvertex names, all else is distinct. *)
          val vrn = Vertex.mk_renaming lhsbndry (RGGraph.get_vnames lhs) 
                                       Vertex.NTab.empty
          val ern = Edge.mk_renaming Edge.NSet.empty 
                                     (RGGraph.get_enames lhs) 
                                     Edge.NTab.empty
          val (rn, rhs') = RGGraph.rename (vrn,ern) rhs
          val lhsauts = (Seq.list_of oo RGGraph.get_isomorphisms) lhs lhs
          val rhsauts = (Seq.list_of oo RGGraph.get_isomorphisms) rhs rhs
          val lhsba = map (RGGraph.VUmorph.restrict_dom_to lhsbndry o fst) lhsauts
          val rhsba = map (RGGraph.VUmorph.restrict_dom_to lhsbndry o fst) rhsauts
          fun filterorbits (x::xs) os = let val orbit = map (RGGraph.VUmorph.compose x) rhsba 
                                            fun eq a b = ((RGGraph.VUmorph.Dom.NTab.fold 
                                                           (fn (d,c) => RGGraph.VUmorph.add d c) (RGGraph.VUmorph.get_domtab a) b;true)
                                                         handle RGGraph.VUmorph.add_exp(_,_,_) => false)
                                            val xs' = filter_out
                                                        (fn a => exists (eq a) orbit)
                                                        xs
                                        in filterorbits xs' (x::os) end
            | filterorbits [] os = os
        in Rule { lhs = lhs, rhs = rhs',
                  lhs_aut = lhsauts,
                  selfapps = filterorbits lhsba [] } end
      else
        raise bad_rule_exp ("mk: Left and right hand side boundaries are different", lhs, rhs)
      end;
  
  (* 
  fun rename_interior_vs xvs (avoidvs, avoides, avoidbs) g = 
       let (* rename rhs; shared bvertex names, all else is distinct. *)
          val vrn = Vertex.mk_renaming xvs avoidvs Vertex.NTab.empty
    in RGGraph.rename (vrn,Edge.Rnm.empty) g  end;
  *)
  
  (* match anything = no set matching *)
  (* val any_match = Match.empty; *)
  
  fun get_lhs (Rule rep) = #lhs rep;
  fun get_rhs (Rule rep) = #rhs rep;
  
  fun update_lhs f (Rule rep) = 
      Rule { lhs = f (#lhs rep), rhs = #rhs rep, lhs_aut = #lhs_aut rep, selfapps = #selfapps rep };
  fun update_rhs f (Rule rep) = 
      Rule { lhs = #lhs rep, rhs = f (#rhs rep), lhs_aut = #lhs_aut rep, selfapps = #selfapps rep };
  
  val set_lhs = update_lhs o K;
  val set_rhs = update_rhs o K;
  
  (* THINK: cache this info? *)
  (* 
  fun get_rhs_newnodes r = 
      let val (lhsns,rhsns) = RGGraph.get_vnames (get_rhs r)
      in Vertex.NSet.subtract rhsns lhsns end;
  fun get_lhs_delnodes r =
      let val (lhsns,rhsns) = RGGraph.get_vnames (get_lhs r)
      in Vertex.NSet.subtract lhsns rhsns  end;
  fun get_rhs_newedges r =
      let val (lhsns,rhsns) = RGGraph.get_enames (get_rhs r)
      in Edge.NSet.subtract rhsns lhsns end;
  fun get_lhs_deledges r =
      let val (lhsns,rhsns) = RGGraph.get_enames (get_lhs r)
      in Edge.NSet.subtract lhsns rhsns  end;
  *)
  
  (* swap lhs and rhs of a rule *)
  fun symmetric r = mk (get_rhs r, get_lhs r);
  
  (* rename a rule to avoid all names in a graph *)
  fun rename_rule_apart g rule = 
      let 
        val (rn,lhs2) = RGGraph.rename_apart g (get_lhs rule)
        val (rn2, rhs2) = RGGraph.rename rn (get_rhs rule)
      in 
        (rn2, rule |> set_lhs lhs2
                   |> set_rhs rhs2)
      end;
  
  (* find all matches between a rule's lhs and a tgt, evaluated lazily *)
  fun matches rule tgt = 
      let val rhs = (get_rhs rule);
          val vs = RGGraph.get_vnames rhs;
          val es = RGGraph.get_enames rhs;
          val bs = RGGraph.get_bbnames rhs;
      in
        MatchSearch.match (vs,es,bs) (get_lhs rule) tgt 
          MatchSearch.Vertex.NTab.empty
      end;
  
  (* a rule is trivially true if the left and right hand sides match.
     Proof is by observing that bounary nodes can only be matched by
     boundary nodes and the types of the lhs and rhs must be equal to be
     a rule. *)
  fun is_trivial rule = raise ERROR "not implemented"; (* FIXME *)
  (* 
      let val init_renaming = 
          val rhs' = RGGraph.rename_apart
      in
      (case Seq.pull (MatchSearch.match (get_lhs rule) (get_rhs rule) 
                                       MatchSearch.Vertex.NTab.empty)
        of NONE => false
         | _ => true);
  *)
  
  (* clever efficient renaming of lhs to tgt *)
  (* 
        (* terminates because renaming is not cyclic (no swaps!), well formed
           because provide an ordering on how renaming happens which avoids
           clashes. *)
        fun matchify_lhs_namelist [] lhs = lhs
          | matchify_lhs_namelist ((n1,n2)::rns) lhs = 
            RGGraph.rename_vname n1 n2 lhs;
            
        (* rename lhs to be same as tgt graph *)
        fun matchify_lhs_name (waitlist,vmap,lhs) = 
            (case (Vertex.NTab.pull_elem vmap) of 
               NONE => matchify_lhs_namelist waitlist lhs
             | SOME ((n1,n2),vmap') => 
               if RGGraph.has_vname lhs n2 then 
                 matchify_lhs_name ((n1,n2)::waitlist) vmap' lhs
               else 
                 matchify_lhs_name waitlist vmap' (RGGraph.rename_vname n1 n2 lhs))
   *)
  
  (* invs are vertices that should only have edges going in, and outvs
     are those where all edges should go out of the vertex. *)
  fun orient_edge_dirs (invs,outvs) g = 
      g |> (Vertex.NSet.fold 
              (fn n => 
                  let val (_,(ines,outes)) = RGGraph.get_vertex g n
                  in Edge.NSet.fold RGGraph.swap_edge_dir outes
                  end)
            invs)
        |> (Vertex.NSet.fold 
              (fn n => 
                  let val (_,(ines,outes)) = RGGraph.get_vertex g n
                  in Edge.NSet.fold RGGraph.swap_edge_dir ines
                  end)
              outvs);
  
  structure BBMatch = Match.BBMatch;
  structure BBox = RGGraph.BBox;
  structure BBMap = BBMatch.BBMap;
  
  
  (* given a bbox matching against a graph, apply it: copy, kill and drop bboxes. 
     When copying, use the specified names. *)
  fun inst_lhs_bbox_match bbmatch g = 
      let 
      in
        g |> BBox.NTab.fold 
              (fn (bn2,bn1) => 
                let 
                  val (vtab,etab) = 
                    BBox.NTab.get (BBMatch.get_crnm bbmatch) bn2
                  val vrnm = Vertex.Rnm.mk_from_nmap vtab
                  val ernm = Edge.Rnm.mk_from_nmap etab
                in RGGraph.copy1_box_given_rnm bn1 (bn2,(vrnm,ernm)) end)
              (BBMap.get_domtab (BBMatch.get_copies bbmatch))
          |> BBox.NSet.fold RGGraph.kill_box (BBMatch.get_kill bbmatch)
          |> BBox.NSet.fold RGGraph.drop_box (BBMatch.get_drop bbmatch)
      end;
  
  
  (*
    val evs = G.get_vnames g
        val erntab = 
            BBox.NTab.fold 
              (fn (bbn, (vrn,ern)) => bbn)
              bbrntab            
  V.NTab.get_nameset 
  *)
  
  (* makes sure to copy external/boundary vertex names exactly, but other vertices 
     are named arbitrarily, but fresh. *)
  fun inst_rhs_bbox_match (avoid_vs,avoid_es,bs) bbmatch g = 
      let 
        val bb_xmaps = BBMatch.get_xmaps bbmatch;
        val bb_xvs = BBMatch.get_xvs bbmatch; (* all exterior vs *)
      in
        ((avoid_vs,avoid_es),g)
          |> BBox.NTab.fold (* copy bang-boxes, preserve interface names *)
              (fn (bn2,bn1) => fn ((avoid_vs,avoid_es),g) =>
               let 
                 val bb_nmap = RGGraph.VAuto.get_domtab 
                                (BBox.NTab.get bb_xmaps bn2);
                 val vrn = Vertex.Rnm.mk Vertex.NSet.empty avoid_vs bb_nmap;
                 val ern = Edge.Rnm.mk_from_avoidset avoid_es;
                 val ((vrn,ern),g) = 
                     RGGraph.copy1_box_given_ext_rnm bn1 bn2 ((vrn,ern),g)
               in 
                 ((Vertex.Rnm.get_avoids vrn, Edge.Rnm.get_avoids ern), g)
               end)
              (BBMap.get_domtab (BBMatch.get_copies bbmatch))
          |> snd (* ignore/throw away new names *)
          |> BBox.NSet.fold RGGraph.kill_box (BBMatch.get_kill bbmatch)
          |> BBox.NSet.fold RGGraph.drop_box (BBMatch.get_drop bbmatch)
      end;
  
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
  
        (* IMPROVE: use depenv to only update changes parts of graph *)
        fun apply_vertex_data_subst g = 
            Vertex.NSet.fold
              (RGGraph.update_vertex_data
                 (Match.MCtxt.subst_in_vertex_data (Match.get_mctxt match)))
              (RGGraph.get_vnames g) g;
  
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
  
  (* 
                    (* THINK: should this renaming be avoided by providing
                       matching with the names of the rhs before-hand, so that
                       copying will never clash with rhs interior names? *)
                    (* rename interior of rhs to avoid clashes created by 
                       !-box copies of exterior vertices *)
                    |> rename_interior_vs
                         (RGGraph.get_boundary old_rhs)
                         names
                         (* (BBMatch.get_xvs bbmatch) *)
                    (* |> snd *) (* ignore renaming *)
  *)
  
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
  
  
  
  (* 
  val do_rewrite_rrhs : (* rewriting a rules rhs *)
   
  val rewritings : T -> (match * RGGraph.T) -> (match * RGGraph.T) Seq.seq
  val rewritings_rrhs : T -> (match * T) -> (match * T) Seq.seq
  *)
  
  fun pretty (Rule rep) = 
      Pretty.chunks 
      [Pretty.block [Pretty.str "Rule: "],
       Pretty.block [Pretty.str "LHS: ", RGGraph.pretty (#lhs rep)],
       Pretty.block [Pretty.str "RHS: ", RGGraph.pretty (#rhs rep)]];
  
  val print = Pretty.writeln o pretty;
end;


structure DB_Rewrite = RewriteFun(MatchSearch);
structure Rewrite : RULE = DB_Rewrite;
