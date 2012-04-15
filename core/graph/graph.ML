(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  extending Graphs have interesting/useful functions  *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

(*  GRAPH is BASIC_GRAPH + useful utilities/API *)
signature GRAPH = 
sig

include BASIC_GRAPH

(* renaming internal names in a graph, used for variables within a vertex. *)
structure CRnmX : COMPOUND_RENAMING
  sharing type CRnmX.obj = T
  sharing type CRnmX.renaming = Param.CRnmX.renaming

(* graph has not vertices (and thus no edges) *)
val is_empty : T -> bool 

(* getting stuff from graphs (gets raise exceptions if no entry) *)
val lookup_vertex : T -> V.name 
									-> (VData.data * (E.NSet.T * E.NSet.T)) option
val lookup_edge : T -> E.name 
									-> ((dir_or_undir * EData.data) 
                      * (V.name * V.name)) option
(* getting stuff from graphs (exception if not there) *)
val get_vertex : T -> V.name -> (VData.data * (E.NSet.T * E.NSet.T))
val get_edge : T -> E.name -> ((dir_or_undir * EData.data) 
                                  * (V.name * V.name))

val get_vertex_data : T -> V.name -> VData.data
val get_edge_data : T -> E.name -> EData.data

(* setting data *)
val set_vertex_data : VData.data -> V.name -> T -> T
val set_edge_data : EData.data -> E.name -> T -> T
(* val set_edge_dir_or_undir : dir_or_undir -> E.name -> T -> T *)

val apply_data_subst : Param.subst -> T -> T

(* check if two graphs are exactly the same (names, data, and structure) *)
val exact_eq : T -> T -> bool

(* adding a vertex gives back unique name for it and the new graph *)
val add_vertex : VData.data -> T -> V.name * T 
val doadd_vertex : VData.data -> T -> T
(* raises vname_already_exists_exp on clash *)
val new_named_vertex : VData.data -> V.name -> T -> T  

(* V.names must already exist, else raises: no_such_vname_exp *)
val add_edge : dir_or_undir * EData.data -> V.name -> V.name -> T -> E.name * T
val doadd_edge : dir_or_undir * EData.data -> V.name -> V.name -> T -> T
val new_named_edge 
    : dir_or_undir * EData.data -> E.name -> V.name -> V.name -> T -> T (* can raise *)
val swap_edge_dir : E.name -> T -> T

(* low level restructuring of graph *)
val update_vertex_enames : ((E.NSet.T * E.NSet.T) -> (E.NSet.T * E.NSet.T))
                          -> V.name -> T -> T 
val update_edge_vnames : ((V.name * V.name) -> (V.name * V.name))
                       -> E.name -> T -> T 

(* get vertex and edge names *)
val get_vnames : T -> V.NSet.T
val get_enames : T -> E.NSet.T

(* add graph names to name sets *)
val add_to_vnames : T -> V.NSet.T -> V.NSet.T
val add_to_enames : T -> E.NSet.T -> E.NSet.T

(* *)
val has_vname : T -> V.name -> bool;
val has_ename : T -> E.name -> bool;

(* renaming *)
val try_rename1_xname : X.name -> X.name -> T -> T option
val try_rename1_vname : V.name -> V.name -> T -> T option
val try_rename1_ename : E.name -> E.name -> T -> T option

structure VertexCRnm : COMPOUND_RENAMING 
where type obj = T and type renaming = V.Rnm.T
structure EdgeCRnm : COMPOUND_RENAMING 
where type obj = T and type renaming = E.Rnm.T

val rename : (V.renaming * E.renaming) -> T 
    -> (V.renaming * E.renaming) * T 
(* rename the 2nd graph to avoid all names in the first one *)
val rename_apart : T -> T -> (V.renaming * E.renaming) * T 

(* rename internal data apart *)
val rename_graph_data_apart : T -> T -> CRnmX.renaming * T 

(* 
val rename_vnames : V.NSet.T -> V.renaming * T -> V.renaming * T
val rename_enames : E.NSet.T -> E.renaming * T -> E.renaming * T
*)

(* merge two graphs which have distinct egdes by shared vertices:
   vertices with same name are kept only once, edges must be
   distinct. *)
val merge_by_vertices : T -> T -> T

(* merge_2vertices: all edges of first get moved to second, and first gets 
   removed. *)
val merge_2vertices : V.name -> V.name -> T -> T

(* remove the first graph from the second, assumes first is a subgraph *)
val delete_subgraph : T -> T -> T

(* getting graph internal representation... *)
val get_vertex_list : T -> (V.name * (VData.data 
                   * (E.NSet.T (* edges coming into this vertex *)
                      * E.NSet.T))) (* edges leaving this vertex *)
                    list;
val get_edge_list : T -> (E.name * ((dir_or_undir * EData.data)
                   * (V.name (* from this vertex *)
                      * V.name))) (* to this vertex *)
                    list

(* edges between two vertices *)
val has_edges_between : T -> V.name -> V.name -> bool
val enames_between : T -> V.name -> V.name -> E.NSet.T
val edges_between : T -> V.name -> V.name 
      -> (dir_or_undir * EData.data) E.NTab.T (* in either dir *)
val halfedges_between : T -> V.name -> V.name 
      -> (dir_or_undir * EData.data) H.NTab.T (* half edge going from fst vertex name *)
val dir_enames_between : T -> V.name -> V.name 
      -> E.NSet.T (* from fst to snd vertex *) 
         * E.NSet.T (* from snd to fst vertex *) 
val dir_edges_between : T -> V.name -> V.name 
      -> (dir_or_undir * EData.data) E.NTab.T (* from fst to snd vertex *) 
         * (dir_or_undir * EData.data) E.NTab.T (* from snd to fst vertex *) 

(* val hens_between : T -> V.name -> V.name -> H.NSet.T *)
val get_vname_ehns : T -> V.name -> H.NSet.T
val get_ehn_vname : T -> H.name -> V.name
val get_ehn_data : T -> H.name -> EData.data
(* val get_ehn_dir_or_undir : T -> H.name -> dir_or_undir *)

(* folding over a subgraph (in no particular order, except at least one edge of 
    a vertex before it's adjacent edges) *)
val fold_subgraph : 
  (* fold this over every vertex *)
  (V.name * VData.data -> 'a -> 'a)
  (* fold over every internal edge *)
  -> (H.halfid * (V.name * VData.data) (* seen this vertex *)
      -> V.name (* other vertex *)
      -> E.NSet.name * (dir_or_undir * EData.data) (* edge *) 
      -> 'a -> 'a) (* data to update *)
  (* fold over every edge leaving the subgraph *)
  -> (H.halfid * (V.name * VData.data) (* seen this vertex *)
      -> V.name (* other vertex *)
      -> E.NSet.name * (dir_or_undir * EData.data) (* edge *) 
      -> 'a -> 'a) (* data to update *)
  (* fold over the subgraph identified by this subset of vertices *)
  -> V.NSet.T 
  -> T (* in this graph *) 
  -> 'a -> 'a (* to update this data *)


(* get adjacent vertices to some vertex *)
val adj_vnames : T -> V.name -> V.NSet.T
val adj_vnames' : T -> (E.NSet.T * E.NSet.T) 
    -> V.NSet.T ->  V.NSet.T
    
(* get incoming, outgoing, undirected edges adjacent to vertex *)
val adj_edge_classes: T -> V.name -> (E.NSet.T*E.NSet.T*E.NSet.T)

(* fold over adjacents; the edge-half argument of the folding function is the one closest to the adjacent vertex names. e.g. folding of ins and outs of vertex x: 
       if "x <-e- y" then: "f (Ehn.mk_start e,...)"
       if "x -e-> y" then: "f (Ehn.mk_end e,...)"   *)
val fold_adj : (H.name * (dir_or_undir * EData.data) 
                -> V.name -> 'a -> 'a) 
      -> T -> V.name -> 'a -> 'a

(* filtered set of adj vertex names *)
val filtered_adj_vnames : (V.name -> bool) 
      -> T -> V.name -> V.NSet.T

(* all incident edges and where they go to, by edge name *)
val adj_etab_to_vnames : T -> V.name 
      -> V.name E.NTab.T -> V.name E.NTab.T

(* the vertices connected to the given set, but outside it *) 
val incident_vertices : T -> V.NSet.T -> V.NSet.T

(* get adj vertices (from edge info) and from vertex name *)

(* singletons have no other incident edges - they only have the one 
   to the given vertex *)
val adj_singleton_vs : T -> V.name 
                        -> H.name V.NTab.T  (* singletons *)
                           * V.NSet.T (* other adj *)
val adj_singleton_vs' : T -> (E.NSet.T * E.NSet.T)
                         -> H.name V.NTab.T (* singletons *)
                            * V.NSet.T (* other adj *)

val adj_vertices_with_edges : T -> V.name 
                   -> ((dir_or_undir * EData.data) E.NTab.T) V.NTab.T
val adj_vertices_with_edges' : T -> (V.name * (E.NSet.T * E.NSet.T))
                    -> ((dir_or_undir * EData.data) E.NTab.T) V.NTab.T

val adj_vertices_with_enames : T -> V.name 
                               -> E.NSet.T V.NTab.T
val adj_vertices_with_enames' : T -> (E.NSet.T * E.NSet.T)
                                -> E.NSet.T V.NTab.T

val get_self_loops : T -> V.name -> EData.data E.NTab.T

val in_enames : T -> V.name -> E.NSet.T
val out_enames : T -> V.name -> E.NSet.T
val pred_vnames : T -> V.name -> V.NSet.T
val succ_vnames : T -> V.name -> V.NSet.T

val get_adj_arities : T -> V.name -> Arity.T V.NTab.T

val get_arity : T -> V.name -> Arity.T

(* depth-first traversal *)
val dft : (E.name -> 'a -> 'a) -> T -> 'a -> 'a
val dft_with_roots : (E.name -> 'a -> 'a) -> T -> 'a -> ('a * V.NSet.T)

(* directed acyclic graphs *)
val convert_to_dag : T -> T
val spanning_dag : T -> T
val spanning_tree : T -> T (* another spanning tree, using DAG traversal *)
val get_dag_ranks : T -> V.name list list

(* copy a set of vertices, copying all adjacent edges. *)
val copy1_vset : V.NSet.T 
    -> (V.Rnm.T * E.Rnm.T) * T 
    -> (V.Rnm.T * E.Rnm.T) * T

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  extra functions for Basic Graphs: provides rest of the GRAPH signature *)
functor GraphToolsFun(BasicGraph : BASIC_GRAPH) 
(*  : adds stuff needed to make a GRAPH (but old graph stuff is hidden)  *)
= struct local open BasicGraph; in

  val is_empty = V.NTab.is_element_empty o get_vertices;
  
  val get_vertex_list = V.NTab.list_of o get_vertices;
  val get_edge_list = E.NTab.list_of o get_edges;
  
  val get_vnames = V.NTab.get_nameset o get_vertices;
  val get_enames = E.NTab.get_nameset o get_edges;
  
  val add_to_vnames = V.NSet.union_merge o get_vnames;
  val add_to_enames = E.NSet.union_merge o get_enames;

  (* implicit: g n *)
  val lookup_vertex = V.NTab.lookup o get_vertices
  val lookup_edge = E.NTab.lookup o get_edges
  
  (* *)
  fun get_vertex g n = 
      case lookup_vertex g n 
       of NONE => raise no_such_vname_exp ("get_vertex", n, g)
        | SOME x => x;
  fun get_edge g en = 
      case lookup_edge g en 
       of NONE => raise no_such_ename_exp ("get_edge", en, g)
        | SOME x => x;
  
  fun get_ehn_vname g ehn = 
      let val (_,(s,t)) = get_edge g (H.get_ename ehn)
      in if H.is_start ehn then s else t end;
  
  fun get_ehn_data g ehn = snd (fst (get_edge g (H.get_ename ehn)));
  
  val get_vertex_data = fst oo get_vertex;
  val get_edge_data = (snd o fst) oo get_edge;
  
  fun get_vname_ehns g v = 
      let val (d,(ines,outes)) = (get_vertex g v)
      in
        H.NSet.empty 
          |> E.NSet.fold (H.NSet.add o H.mk_start) outes 
          |> E.NSet.fold (H.NSet.add o H.mk_end) ines
      end;
  
  val set_vertex_data = update_vertex_data o K;
  val set_edge_data = update_edge_data o K;
  val set_edge_data = update_edge_data o K;
  
  
  (* change structure of graph *)
  fun update_vertex_enames f n g = 
      update_vertices (V.NTab.map_entry (fn (nd,ios) => (nd,f ios)) n) g;
  fun update_edge_vnames f en g = 
      update_edges (E.NTab.map_entry (fn (e,fromto) => (e,f fromto)) en) g;
  
  
  (* implicit args: nd g *)
  val add_vertex = add_named_vertex V.default_name;
  val doadd_vertex = snd oo add_vertex;
  fun new_named_vertex nd n g =
      let val (n2,g2) = add_named_vertex n nd g in
        if V.name_eq (n2, n) then g2 else 
        raise vname_already_exists_exp (n, g)
      end;
  
  (* implicit args: g n *)
  val has_vname = V.NTab.contains_name o get_vertices;
  val has_ename = E.NTab.contains_name o get_edges;
  
  (* implicit args: e n1 n2 g *)
  val add_edge = add_named_edge E.default_name;
  val doadd_edge = snd oooo add_edge;
  fun new_named_edge e en n1 n2 g =
      let val (en2,g2) = add_named_edge en e n1 n2 g in
        if E.name_eq (en2, en) then g2 else 
        raise ename_already_exists_exp (en,g)
      end;
  
  
  fun update_vertex_ins_and_outs f n = 
      update_vertices (V.NTab.map_entry 
                         (fn (nd,ins_and_outs) => (nd,f ins_and_outs)) n);

  (* IMPROVE: more efficient way by more direct manipulation of 
     underlying data; might we optimise away update_edges for cases 
     when edges have no data? *)
  fun apply_data_subst subst g =  
      g |> (V.NSet.fold
               (update_vertex_data (Param.subst_in_vertex subst))
               (get_vnames g))
        |> (E.NSet.fold
               (update_edge_data (Param.subst_in_edge subst))
               (get_enames g));



  
  (* swap direction of edge en *)
  fun swap_edge_dir en g = 
      let 
        val (d,(fromn, ton)) = get_edge g en
      in
        g |> (update_vertex_ins_and_outs 
                (fn (eins,eouts) => (E.NSet.add en eins,E.NSet.delete en eouts)) 
                fromn)
          |> (update_vertex_ins_and_outs 
                (fn (eins,eouts) => (E.NSet.delete en eins,E.NSet.add en eouts)) 
                ton)
          |> update_edges (E.NTab.map_entry (K (d,(ton, fromn))) en)
      end;
  
  
  (* merge graphs, assumes disjoint edge names; adds new vertices, keeps
     vertices with same name already in second graph. *)
  fun merge_by_vertices g1 g2 = 
      g2 |> (V.NTab.fold 
               (fn (vn,(d,_)) => fn g3 => 
                   if has_vname g3 vn then g3 else new_named_vertex d vn g3)
               (get_vertices g1))
         |> (E.NTab.fold 
               (fn (en,(d,(vn1,vn2))) => fn g3 => 
                   if has_ename g3 en then 
                     raise ename_already_exists_exp (en, g3)
                   else new_named_edge d en vn1 vn2 g3)
               (get_edges g1));

  (* merge vertices, removes v1, keeps v1. deletes all edges to v1, add them as 
     new edges to v2 (with the same name). *)
  fun merge_2vertices v1 v2 g = 
      let val (_,(ines,outes)) = get_vertex g v1
      in 
        g |> E.NSet.fold
              (fn en => fn g => 
                  let val (e,(fromv,tov)) = get_edge g en
                  in g |> delete_edge en
                       |> add_named_edge en e fromv v2
                       |> snd
                  end)
              ines
          |> E.NSet.fold
              (fn en => fn g => 
                  let val (e,(fromv,tov)) = get_edge g en
                  in g |> delete_edge en
                       |> add_named_edge en e v2 tov
                       |> snd
                  end)
              outes
          |> delete_vertex v1
      end;

  (* delete subg from fromg; assumes edge and vertex names of subg are a
     subset of fromg; else delete will raise exception *)
  fun delete_subgraph subg fromg = 
      fromg |> (E.NSet.fold 
                  delete_edge 
                  (get_enames subg))
            |> (V.NSet.fold 
                  delete_vertex
                  (get_vnames subg));
  
  fun try_rename1_xname en1 en2 g = 
      SOME (rename_xname en1 en2 g)
      handle X.Rnm.invalid_renaming_exp _ => NONE;

  fun try_rename1_vname en1 en2 g = 
      SOME (rename_vname en1 en2 g)
      handle vname_already_exists_exp _ => NONE;
  
  fun try_rename1_ename en1 en2 g = 
      SOME (rename_ename en1 en2 g)
      handle ename_already_exists_exp _ => NONE;
  
  (* compound renamings: for renaming vertices/edges in a graph *)
  structure VertexCRnm = CompoundRenamingFun(
  struct 
    structure Nm = V
    type obj = T
    val try_rename1 = try_rename1_vname;
    val get_nameset = get_vnames
  end);
  
  structure EdgeCRnm = CompoundRenamingFun(
  struct 
    structure Nm = E
    type obj = T
    val try_rename1 = try_rename1_ename;
    val get_nameset = get_enames
  end);
  
  (*  *)
  fun rename (vrn,ern) g = 
      let 
        val (vrn2,g') = VertexCRnm.rename vrn g
        val (ern2,g'') = EdgeCRnm.rename ern g'
      in ((vrn2,ern2),g'') end;
  
  (* rename g2 apart from g1 *)
  fun rename_apart g1 g2 = 
      let 
        val vrn = V.Rnm.mk_from_avoids 
            (V.NSet.union_merge (get_vnames g1) (get_vnames g2))
        val ern = E.Rnm.mk_from_avoids 
            (E.NSet.union_merge (get_enames g1) (get_enames g2))
      in rename (vrn,ern) g2 end;
  
  (* rename internal data apart *)
  (* IMPROVE: don't do the slow fold over everything; use dependency info in 
     NameDeps to rename only the needed parts, etc. *)
  structure CRnmX : COMPOUND_RENAMING
  = struct 
    type obj = T
    type renaming = Param.CRnmX.renaming;
    val empty_renaming = Param.CRnmX.empty_renaming;
  
    fun rename rnm g = 
      (rnm,g)
        |> (V.NTab.fold
             (fn (vn,(d,es)) => fn (rnm, g) => 
                 let val (rnm2,d2) = Param.VDataCRnmX.rename rnm d 
                 in (rnm2, set_vertex_data d2 vn g) end
             ) (get_vertices g))
        |> (E.NTab.fold
             (fn (en,((dir,d),vs)) => fn (rnm,g) => 
                 let val (rnm2,d2) = Param.EDataCRnmX.rename rnm d 
                 in (rnm2,set_edge_data d2 en g) end
             ) (get_edges g));
  
    fun mk_renaming g rnm = 
      rnm
        |> (V.NTab.fold
             (fn (vn,(d,es)) => Param.VDataCRnmX.mk_renaming d)  
             (get_vertices g))
        |> (E.NTab.fold
             (fn (en,((dir,d),vs)) => Param.EDataCRnmX.mk_renaming d)
             (get_edges g));
  
    fun do_renaming rnm g = 
        g |> (V.NTab.fold
             (fn (vn,(d,es)) => 
                 update_vertex_data (Param.VDataCRnmX.do_renaming rnm) vn)
             (get_vertices g))
          |> (E.NTab.fold
             (fn (en,((dir,d),vs)) =>  
                 update_edge_data (Param.EDataCRnmX.do_renaming rnm) en)
             (get_edges g));
    
    val restrict_to_avoiding = Param.CRnmX.restrict_to_avoiding;
  end;
  
  (* *)
  fun rename_graph_data_apart g1 g2 = 
      let val rnm = CRnmX.restrict_to_avoiding 
                      (CRnmX.mk_renaming g1 CRnmX.empty_renaming)
      in CRnmX.rename rnm g2 end;
  
  (* fold: pass over every vertex (fv) edge between the selected vertices 
     (fe_in); and over every edge going out of the selected vertex set (fe_out).
     There is an invariant which is that edges are considered only after at 
     last one end-point is considered. The considered end-point is passed as a
     parameter to the edge-fold functions. *)
  (* the (') function, starts form vertex v *) 
  fun fold_subgraph' fv fe_in fe_out vs g a = 
      let fun edgef (x as (h_end,(v,vd))) seen_edges e a = 
              if E.NSet.contains seen_edges e then a
              else 
                let val (ed,(fromv,tov)) = get_edge g e in
                  case h_end of 
                    H.TargetEnd => 
                    if V.NSet.contains vs fromv then fe_in x tov (e,ed) a
                    else fe_out x tov (e,ed) a
                  | H.SourceEnd => 
                    if V.NSet.contains vs fromv then fe_in x fromv (e,ed) a
                    else fe_out x fromv (e,ed) a
                end;
      in
        V.NSet.fold 
          (fn v => fn (seen_edges,a) =>
            let val (vd,(in_es,out_es)) = get_vertex g v in 
               (seen_edges |> E.NSet.union_merge out_es 
                           |> E.NSet.union_merge in_es,
                a |> fv (v,vd)
                  |> E.NSet.fold 
                      (edgef (H.TargetEnd,(v,vd)) seen_edges) in_es
                  |> E.NSet.fold 
                      (edgef (H.SourceEnd,(v,vd)) seen_edges) out_es)
            end)
          vs (E.NSet.empty,a)
      end;
  
  (* ignore the 'seen' edges *)
  fun fold_subgraph fv fe_in fe_out vs g a = 
      snd (fold_subgraph' fv fe_in fe_out vs g a);
  
  (* fold a function over the adjacent vertex names. 
     The edge-half is the half connected to the adj vertex. 
     e.g. folding of ins and outs of vertex x: 
       if "x <-e- y" then: "f (H.mk_start e,...)"
       if "x -e-> y" then: "f (H.mk_end e,...)"  *)
  fun fold_adj' f graph (ins,outs) a = 
      a |> (E.NSet.fold 
           (fn e => let val (d,(from,to)) = get_edge graph e 
            in f (H.mk_start e,d) from end) ins)
        |> (E.NSet.fold 
           (fn e => let val (d,(from,to)) = get_edge graph e 
            in f (H.mk_end e,d) to end) outs);
  
  (* all vertices adjacent to vertex named n; 
     g = graph; f = function; a = accumulator.
     The edge-half is the half connected to the adj vertex. *)
  fun fold_adj f g n a = 
      fold_adj' f g (snd (get_vertex g n)) a;
  
  (* create table of edge name to adj *)
  val adj_etab_to_vnames = 
      fold_adj (fn (eh, d) => fn n2 => fn etab => 
                   let val e = H.get_ename eh
                   in if E.NTab.contains etab e then etab
                      else E.NTab.update (e,n2) etab
                   end);
  
  (* IMPROVE: use fold function ! *)
  (* make a table: (EData.data E.NTab.T) V.NTab.T
  holding every edge's data that is connects n and to the key V.NTab, 
  the V.NTab has every vertex adjacent to n. (Note: ignores direction) *)
  fun adj_vertices_with_edges' g (n,(ins,outs)) = 
      V.NTab.empty
        |> (E.NSet.fold 
              (fn e => fn ntab => 
               let val (d,(from,to)) = get_edge g e
               in if V.name_eq (from,n) then ntab (* no self-loops *)
                  else if V.NTab.contains ntab from
                  then 
                    V.NTab.map_entry 
                      (E.NTab.ins (e,d)) (* no dup exp for well-formed graphs *)
                      from ntab
                  else V.NTab.ins (from,E.NTab.ins (e,d) E.NTab.empty) ntab
               end)
              ins)
        |> (E.NSet.fold 
              (fn e => fn ntab => 
               let val (d,(from,to)) = get_edge g e
               in if V.name_eq (to,n) then ntab (* no self-loops *)
                  else if (V.NTab.contains ntab to) then   
                    V.NTab.map_entry 
                      (E.NTab.ins (e,d)) (* no dup exp for well-formed graphs *)
                    to ntab
                  else V.NTab.ins (to,E.NTab.ins (e,d) E.NTab.empty) ntab
               end)
              outs);
  
  fun get_self_loops' g n ins = 
      E.NTab.empty
        |> (E.NSet.fold 
              (fn e => fn etab => 
               let val ((bd,d),(from,to)) = get_edge g e
               in if V.name_eq (n,from) 
                  then E.NTab.ins (e,d) etab else etab
               end)
              ins)
  
  fun get_self_loops g n = 
      get_self_loops' g n (fst (snd (get_vertex g n)));
  
  fun adj_vertices_with_edges g n = 
      adj_vertices_with_edges' g (n,snd (get_vertex g n));
  
  (* IMPROVE: use fold function to simplify this *)
  fun adj_vertices_with_enames' g (ins,outs) = 
      V.NTab.empty
        |> (E.NSet.fold 
              (fn e => fn ntab => 
               let val (_,(from,to)) = get_edge g e
               in if (V.NTab.contains ntab from) then 
                    V.NTab.map_entry 
                      (E.NSet.add e) (* no dup exp for well-formed graphs *)
                      from ntab
                  else 
                    V.NTab.ins (from,E.NSet.add e E.NSet.empty) ntab
               end)
              ins)
        |> (E.NSet.fold 
              (fn e => fn ntab => 
               let val (d,(from,to)) = get_edge g e
               in if (V.NTab.contains ntab to) then 
                    V.NTab.map_entry 
                      (E.NSet.add e) (* no dup exp for well-formed graphs *)
                      to ntab
                  else 
                    V.NTab.ins (to,E.NSet.add e E.NSet.empty) ntab
               end)
              outs);
  
  fun adj_vertices_with_enames g n = 
      adj_vertices_with_enames' g (snd (get_vertex g n));
  
  (* IMPROVE: use fold function to simplify this *)
  (* find all adjacent vertices which have no other egdes than the one
     (from the given ins/outs) of this vertex *)
  fun adj_singleton_vs' g (ins,outs) = 
      (V.NTab.empty, V.NSet.empty)
        |> (E.NSet.fold 
              (fn e => fn (adjinfo as (stab, notsset)) => 
               let val (_,(from,to)) = get_edge g e
               in if V.NSet.contains notsset from then adjinfo
                  else if V.NTab.contains stab from then 
                    (V.NTab.delete from stab, 
                     V.NSet.add from notsset)
                  else 
                    (V.NTab.ins (from,H.mk_start e) stab, 
                     notsset)
               end)
              ins)
        |> (E.NSet.fold 
              (fn e => fn (adjinfo as (stab, notsset)) => 
               let val (_,(from,to)) = get_edge g e
               in if V.NSet.contains notsset to then adjinfo
                  else if V.NTab.contains stab to then 
                    (V.NTab.delete to stab, V.NSet.add to notsset)
                  else 
                    (V.NTab.ins (to,H.mk_end e) stab, notsset)
               end)
              outs);
  fun adj_singleton_vs g n = adj_singleton_vs' g (snd (get_vertex g n));
  
  
  (* all vertices adjacent to vertex *)
  val adj_vnames' = fold_adj' (K V.NSet.add);
  fun adj_vnames g n = fold_adj (K V.NSet.add) g n V.NSet.empty;
  
  (* adjacent edge classes *)
  fun adj_edge_classes g n = let
         val (_,(ins,outs)) = get_vertex g n
         fun add_if_undir e ns = ns |> let val ((d,_),_) = get_edge g e
                                       in if d = UnDirected then E.NSet.add e
                                                            else I
                                       end
         val undirs = E.NSet.empty |> E.NSet.fold add_if_undir ins
                                      |> E.NSet.fold add_if_undir outs
      in (E.NSet.subtract ins undirs,E.NSet.subtract outs undirs,undirs) end;
  
  (* filtered set of adj vertex *)
  fun filter_adj_fn f (e : H.name * (dir_or_undir * EData.data)) n s = 
      if (f n) then V.NSet.add n s else s;
  val filtered_adj_vnames' = fold_adj' o filter_adj_fn;
  fun filtered_adj_vnames f g n = 
      fold_adj (filter_adj_fn f) g n V.NSet.empty;;
  
  
  (* some more convenience functions for directed graphs *)
  fun in_enames g = fst o snd o (get_vertex g);
  fun out_enames g = snd o snd o (get_vertex g);
  
  (* names of adj vertices with edges going to them *)
  fun succ_vnames g vname =
      adj_vnames' g (E.NSet.empty, out_enames g vname) V.NSet.empty;
  
  (* names of adj vertices with edges coming in from them *)
  fun pred_vnames g n =
      adj_vnames' g (in_enames g n, E.NSet.empty) V.NSet.empty;
  
  (* vertices incident into vns *)
  (* the vertices connected to the given set, but outside it *) 
  fun incident_vertices g vns = 
      V.NSet.fold
        (fn n => 
            V.NSet.fold 
              (fn n2 => 
                  if V.NSet.contains vns n2 then I
                  else V.NSet.add n2)
              (adj_vnames g n))
        vns V.NSet.empty;
  
  (* edge names between two vertcies *)
  fun enames_between g n1 n2 = 
      let 
        val (_,(ins1,outs1)) = get_vertex g n1
        val (_,(ins2,outs2)) = get_vertex g n2
      in
        E.NSet.union_merge
          (E.NSet.intersect ins1 outs2)
          (E.NSet.intersect ins2 outs1)
      end;    
  
  (* 
  fun hens_between g n1 n2 = 
      let 
        val (_,(ins1,outs1)) = get_vertex g n1
        val (_,(ins2,outs2)) = get_vertex g n2
      in
        H.NSet.union_merge
          (E.NSet.intersect ins1 outs2)
          (E.NSet.intersect ins2 outs1)
      end;
  *)
  
  
  (* *)
  fun has_edges_between g n1 n2 = 
      let 
        val (_,(ins1,outs1)) = get_vertex g n1
        val (_,(ins2,outs2)) = get_vertex g n2
      in
         (E.NSet.nonempty_intersect ins2 outs1 orelse 
          E.NSet.nonempty_intersect ins1 outs2)
      end;
  
  (* edge names between two vertcies *)
  fun dir_enames_between g n1 n2 = 
      let 
        val (_,(ins1,outs1)) = get_vertex g n1
        val (_,(ins2,outs2)) = get_vertex g n2
      in
         (E.NSet.intersect ins2 outs1,
          E.NSet.intersect ins1 outs2)
      end;
  
  val enames_between = uncurry E.NSet.union_merge ooo dir_enames_between;
  
  
  fun edges_between g n1 n2 = 
      E.NSet.fold 
        (fn e => E.NTab.ins (e, fst (get_edge g e)))
        (enames_between g n1 n2)
        E.NTab.empty;
  
  fun halfedges_between g n1 n2 = 
      let val (lr,rl) = dir_enames_between g n1 n2 in
        H.NTab.empty
          |> (E.NSet.fold 
                (fn e => H.NTab.ins 
                           (H.mk_start e, fst (get_edge g e))) lr)
          |> (E.NSet.fold 
                (fn e => H.NTab.ins 
                           (H.mk_end e, fst (get_edge g e))) rl)
      end;
  
  fun dir_edges_between g n1 n2 = 
      let val (lr,rl) = dir_enames_between g n1 n2 in
        (E.NSet.fold 
          (fn e => E.NTab.ins (e, fst (get_edge g e))) lr
          E.NTab.empty,
         E.NSet.fold 
          (fn e => E.NTab.ins (e, fst (get_edge g e))) rl
          E.NTab.empty)
      end;
  
  
  (* two graphs are exactly the same, including data, edge direction and
     names of vertices and edges. *) 
  fun exact_eq g1 g2 =
      let 
        val e1s = get_edges g1
        val e2s = get_edges g2
        val v1s = get_vertices g1
        val v2s = get_vertices g2
      in
        (* check same vertex names *)
        (V.NSet.eq (V.NTab.get_nameset v1s)
                        (V.NTab.get_nameset v2s)) andalso 
        (* check same data, and same incident edges structure *)
        (V.NTab.forall 
           (fn (n,(d,(fromes,toes))) => 
               case V.NTab.lookup v2s n of 
                 NONE => false
               | SOME (d2,(fromes2,toes2)) => 
                 (VData.data_eq (d,d2)) andalso 
                 (* now check structure *)
                 (E.NSet.eq fromes fromes2) andalso
                 (E.NSet.eq toes toes2) )
           v1s) andalso 
        (* now check same edge data *)
        (E.NTab.forall (* note that we dont re-check structure *)
           (fn (e,((bd,ed),_)) => 
               (* from above, we know we have same edge names: could use get *)
               case E.NTab.lookup e2s e of 
                 NONE => false
               (* now check have same edge data *)
               | SOME ((bd2,ed2),_) => EData.data_eq (ed,ed2))
           e1s)
      end;
  
  (* Traverse the graph depth-first, folding on fld. The output is the
   * new structure and a set of all the chosen root vertices. *)
  fun dft_with_roots fld graph base = let
    val out_edges = fn v => E.NSet.list_of (#2(#2(get_vertex graph v)));
    fun tree [] base rem = (base, rem) (* leaf/singleton *)
      | tree (e::es) base rem =
        let val targ = #2(#2(get_edge graph e))
        in if V.NSet.contains rem targ then
             let
               (* recurse on children *)
               val (base', rem') = tree (out_edges targ) (fld e base)
                                        (rem|>V.NSet.delete targ)
             in tree es base' rem' (* recurse on siblings *)
             end
           else (base, rem)  (* cycle *)
        end
    fun forest base rem roots =
        if V.NSet.is_empty rem then (base, roots)
        else let val fst = the (rem |> V.NSet.get_first)
                 val (base', rem') = tree (out_edges fst) base
                                          (rem|>V.NSet.delete fst)
             in forest base' rem' (roots|>V.NSet.add fst)
             end
  in forest base (get_vnames graph) V.NSet.empty
  end
                                      
  (* DFT without roots *)
  fun dft fld graph base = #1 (dft_with_roots fld graph base)
  
  
  (* The following are several useful functions for working with graphs
   * that are directed acyclic *)
  
  
  (* find the "longest" directed acyclic graph that spans the given graph, applying
   * the three given functions to add edges of each kind.
   *   back_edge : an edge that creates a directed cycle
   *   fw_edge   : an edge that creates a cycle but not a directed cycle
   *   new_edge  : an edge that creates no cycles *)
  fun dag_traverse back_edge fw_edge new_edge graph = let
      val out_elist = E.NSet.list_of o (out_enames graph)
  
      (* generate a dag from the given edge list *)
      fun dag depth (e::es) dg vtab =
    let
        val (edata, (s, t)) = get_edge graph e
        val (dg', vtab') =
      (case V.NTab.lookup vtab t
        (* if target hasn't been seen, add edge and recurse *)
        of NONE => dag (depth + 1) (out_elist t)
           (dg |> new_edge e edata s t)
           (vtab |> V.NTab.update (t, depth + 1))
         (* if target has been seen, only add edge if its rank is lower *)
         | SOME d => if (depth < d orelse d = ~1)
               then (dg |> fw_edge e edata s t, vtab)
               else (dg |> back_edge e edata s t, vtab))
    in dag depth es dg' vtab'
    end
        | dag _ [] dg vtab = (dg, vtab)
      
      (* pull a fresh vertex *)
      fun fresh_vert vtab = let
    val vnames = get_vnames graph
    fun fv NONE = NONE
      | fv (SOME v) = (case V.NTab.lookup vtab v
            of NONE => SOME v
             | _ => fv (V.NSet.next_bigger vnames v))
      in fv (V.NSet.get_min vnames)
      end
      
      (* iteratively run dag until we run out of vertices *)
      fun dag_until_done dg vtab =
    case fresh_vert vtab
     of NONE => dg
      | (SOME v) => let
      val vtab = (V.NTab.map_all (K (K ~1)) vtab)
               |> V.NTab.update (v,0)
      val (dg',vtab') = dag 0 (out_elist v) dg vtab
        in dag_until_done dg' vtab'
        end
      val init_dg = E.NSet.fold delete_edge (get_enames graph) graph
  in dag_until_done init_dg V.NTab.empty
  end
  
  (* several common uses for dag_traverse *)
  local
      fun ignore_edge _ _ _ _ = I
      fun keep_edge e edata s t g = #2(g|>add_named_edge e edata s t)
      (* flip_edge also removes self-loops *)
      fun flip_edge e edata s t g = if V.name_ord (s,t) = EQUAL then g
            else #2(g|>add_named_edge e edata t s)
  in
      (* remove self-loops and reverse back-edges *)
      val convert_to_dag = dag_traverse flip_edge keep_edge keep_edge
      (* remove self-loops and back-edges *)
      val spanning_dag = dag_traverse ignore_edge keep_edge keep_edge
      val spanning_tree = dag_traverse ignore_edge ignore_edge keep_edge
  end
  
  (* will probably not terminate if dag isn't directed acyclic *)
  fun get_dag_ranks dag = let
      val outputs = V.NSet.filter
           (fn v => E.NSet.is_empty (out_enames dag v))
           (get_vnames dag)
      val vnames = V.NSet.list_of
           (V.NSet.subtract (get_vnames dag) outputs)
      fun test seen v = not (V.NSet.contains seen v)
            andalso V.NSet.forall
            (V.NSet.contains seen)
            (pred_vnames dag v)
      fun mk seen =
    case filter (test seen) vnames
     of []=>[] | lst => lst :: mk (V.NSet.add_list lst seen)
  in mk V.NSet.empty @ [V.NSet.list_of outputs]
  end

  (* assumes ern has all edge names of the graph in avoid set, 
     else may raise no_such_ename_exp (when copying/renaming an 
     edge that is already the graph) *)
  fun copy1edge (ehn, dir_and_data) v v2 ((vrn,ern), g) =
      let 
        val e = H.get_ename ehn;
        val (new_e_opt,ern') = E.Rnm.rename1 e ern;
        val (e',g') = 
            (case new_e_opt of NONE => 
               raise no_such_ename_exp ("renaming doesn't have edge in avoid set",e,g)
             | SOME e' => 
               (e', if H.is_start ehn 
                    then new_named_edge dir_and_data e' v2 v g 
                    else new_named_edge dir_and_data e' v v2 g));
      in ((vrn,ern'),g') end;

  (* copy a set of vertices, making copy having names fresh w.r.t. 
     vrn and ern (edge and vertex renaming). 
     copies edges to all vertices outside the selected set. 
     assumes: (get_Xnames g) <= get_avoids(Xrn); for X = {e, v}.
  *)
  fun copy1_vset vset ((vrn,ern),g) = 
      V.NSet.fold 
        (fn v => fn ((vrn,ern),g) => (* copy each vertex *)
            let
              val (new_v_opt,vrn') = V.Rnm.rename1 v vrn
              val (v',g') = (case new_v_opt of NONE => 
                         raise no_such_vname_exp ("renaming doesn't have vertex in avoid set",v,g)
                       | SOME v' => 
                         (v',new_named_vertex (get_vertex_data g v) v' g))
            in
              fold_adj (* for each connected edge *)
                (fn (ehn, dir_and_data) => fn v2 => 
                 fn (r_g as ((vrn,ern), g)) =>
                    (* has the other end of the edge already been added? *)
                    if V.NSet.contains vset v2 then 
                      (case V.Rnm.lookup vrn v2 of 
                           NONE => r_g
                         (* add edges to any other adjacent stuff that has already been copied *)
                         | SOME v2' => copy1edge (ehn, dir_and_data) v' v2' r_g)
                     else (* if outside of vset, copy edge to vertex outside *)
                        copy1edge (ehn, dir_and_data) v' v2 r_g
                ) g' v ((vrn',ern),g')
             end
        ) vset ((vrn,ern), g);

  
  fun init_arity (eh,(UnDirected,ed)) = 
      Arity.update_undir (fn x => x + 1) Arity.zero
    | init_arity (eh,(Directed,ed)) = 
      if H.is_start eh then Arity.update_in (fn x => x + 1) Arity.zero
      else Arity.update_out (fn x => x + 1) Arity.zero;

  fun add_edge_to_arity (eh,(UnDirected,ed)) arity = 
      Arity.update_undir (fn x => x + 1) arity 
    | add_edge_to_arity (eh,(Directed,ed)) arity =
      if H.is_start eh then Arity.update_in (fn x => x + 1) arity 
      else Arity.update_out (fn x => x + 1) arity; 

  fun get_adj_arities g n = 
      let   
        fun add_one_arity e n2 ntab =
            if V.NTab.contains_name ntab n2 then
              V.NTab.map_entry (add_edge_to_arity e) n2 ntab
            else V.NTab.update (n2,init_arity e) ntab;
      in 
        fold_adj add_one_arity g n V.NTab.empty
      end;
      
  fun get_arity g n =
      let   
        fun add_one_arity e n2 arity =
            add_edge_to_arity e arity
      in 
        fold_adj add_one_arity g n Arity.zero
      end;
end; end; (* local -- hides opened Graph data types; structure *)




(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  test that GraphToolsFun does what signature says... *) 
functor GraphFun(BasicGraph : BASIC_GRAPH) 
: GRAPH
= struct
  open BasicGraph;
  structure Tools = GraphToolsFun(BasicGraph);
  open Tools;
end;
