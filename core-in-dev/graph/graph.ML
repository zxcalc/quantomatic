(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  extending Graphs have interesting/useful functions  *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

(*  GRAPH is BASIC_GRAPH + useful utilities/API *)
signature GRAPH = 
sig

include BASIC_GRAPH

(* renaming internal names in a graph *)
structure CRnmINames : COMPOUND_RENAMING
  sharing type CRnmINames.obj = T
  sharing type CRnmINames.renaming = Param.CRnmINames.renaming

(* graph has not vertices (and thus no edges) *)
val is_empty : T -> bool 

(* getting stuff from graphs (gets raise exceptions if no entry) *)
val lookup_vertex : T -> Vertex.name 
									-> (Vertex.data * (Edge.NSet.T * Edge.NSet.T)) option
val lookup_edge : T -> Edge.name 
									-> ((dir_or_undir * Edge.data) 
                      * (Vertex.name * Vertex.name)) option
(* getting stuff from graphs (exception if not there) *)
val get_vertex : T -> Vertex.name -> (Vertex.data * (Edge.NSet.T * Edge.NSet.T))
val get_edge : T -> Edge.name -> ((dir_or_undir * Edge.data) 
                                  * (Vertex.name * Vertex.name))

val get_vertex_data : T -> Vertex.name -> Vertex.data
val get_edge_data : T -> Edge.name -> Edge.data

(* setting data *)
val set_vertex_data : Vertex.data -> Vertex.name -> T -> T
val set_edge_data : Edge.data -> Edge.name -> T -> T
(* val set_edge_dir_or_undir : dir_or_undir -> Edge.name -> T -> T *)

(* check if two graphs are exactly the same (names, data, and structure) *)
val exact_eq : T -> T -> bool

(* adding a vertex gives back unique name for it and the new graph *)
val add_vertex : Vertex.data -> T -> Vertex.name * T 
val doadd_vertex : Vertex.data -> T -> T
val new_named_vertex : Vertex.name -> Vertex.data -> T -> T  (* can raise *)

(* Vertex.names must already exist, else raises: no_such_vname_exp *)
val add_edge : dir_or_undir * Edge.data -> Vertex.name -> Vertex.name -> T -> Edge.name * T
val doadd_edge : dir_or_undir * Edge.data -> Vertex.name -> Vertex.name -> T -> T
val new_named_edge 
    : Edge.name -> dir_or_undir * Edge.data -> Vertex.name -> Vertex.name -> T -> T (* can raise *)
val swap_edge_dir : Edge.name -> T -> T

(* low level restructuring of graph *)
val update_vertex_enames : ((Edge.NSet.T * Edge.NSet.T) -> (Edge.NSet.T * Edge.NSet.T))
                          -> Vertex.name -> T -> T 
val update_edge_vnames : ((Vertex.name * Vertex.name) -> (Vertex.name * Vertex.name))
                       -> Edge.name -> T -> T 

(* get vertex and edge names *)
val get_vnames : T -> Vertex.NSet.T
val get_enames : T -> Edge.NSet.T

(* *)
val has_vname : T -> Vertex.name -> bool;
val has_ename : T -> Edge.name -> bool;

(* renaming *)
val try_rename1_ename : Edge.name -> Edge.name -> T -> T option
val try_rename1_vname : Vertex.name -> Vertex.name -> T -> T option

structure VertexCRnm : COMPOUND_RENAMING 
where type obj = T and type renaming = Vertex.Rnm.T
structure EdgeCRnm : COMPOUND_RENAMING 
where type obj = T and type renaming = Edge.Rnm.T

val rename : (Vertex.renaming * Edge.renaming) -> T 
    -> (Vertex.renaming * Edge.renaming) * T 
(* rename the 2nd graph to avoid all names in the first one *)
val rename_apart : T -> T -> (Vertex.renaming * Edge.renaming) * T 

(* rename internal data apart *)
val rename_graph_data_apart : T -> T -> CRnmINames.renaming * T 

(* 
val rename_vnames : Vertex.NSet.T -> Vertex.renaming * T -> Vertex.renaming * T
val rename_enames : Edge.NSet.T -> Edge.renaming * T -> Edge.renaming * T
*)

(* merge two graphs which have distinct egdes by shared vertices:
   vertices with same name are kept only once, edges must be
   distinct. *)
val merge_by_vertices : T -> T -> T

(* remove the first graph from the second, assumes first is a subgraph *)
val delete_subgraph : T -> T -> T

(* getting graph internal representation... *)
val get_vertex_list : T -> (Vertex.name * (Vertex.data 
                   * (Edge.NSet.T (* edges coming into this vertex *)
                      * Edge.NSet.T))) (* edges leaving this vertex *)
                    list;
val get_edge_list : T -> (Edge.name * ((dir_or_undir * Edge.data)
                   * (Vertex.name (* from this vertex *)
                      * Vertex.name))) (* to this vertex *)
                    list

(* edges between two vertices *)
val has_edges_between : T -> Vertex.name -> Vertex.name -> bool
val enames_between : T -> Vertex.name -> Vertex.name -> Edge.NSet.T
val edges_between : T -> Vertex.name -> Vertex.name 
      -> (dir_or_undir * Edge.data) Edge.NTab.T (* in either dir *)
val halfedges_between : T -> Vertex.name -> Vertex.name 
      -> (dir_or_undir * Edge.data) Edge.Ehn.NTab.T (* half edge going from fst vertex name *)
val dir_enames_between : T -> Vertex.name -> Vertex.name 
      -> Edge.NSet.T (* from fst to snd vertex *) 
         * Edge.NSet.T (* from snd to fst vertex *) 
val dir_edges_between : T -> Vertex.name -> Vertex.name 
      -> (dir_or_undir * Edge.data) Edge.NTab.T (* from fst to snd vertex *) 
         * (dir_or_undir * Edge.data) Edge.NTab.T (* from snd to fst vertex *) 

(* val hens_between : T -> Vertex.name -> Vertex.name -> Edge.Ehn.NSet.T *)
val get_vname_ehns : T -> Vertex.name -> Edge.Ehn.NSet.T
val get_ehn_vname : T -> Edge.Ehn.name -> Vertex.name
val get_ehn_data : T -> Edge.Ehn.name -> Edge.data
(* val get_ehn_dir_or_undir : T -> Edge.Ehn.name -> dir_or_undir *)

(* folding over a subgraph (in no particular order, except at least one edge of 
    a vertex before it's adjacent edges) *)
val fold_subgraph : 
  (* fold this over every vertex *)
  (Vertex.name * Vertex.data -> 'a -> 'a)
  (* fold over every internal edge *)
  -> (Edge.Ehn.halfid * (Vertex.name * Vertex.data) (* seen this vertex *)
      -> Vertex.name (* other vertex *)
      -> Edge.NSet.name * (dir_or_undir * Edge.data) (* edge *) 
      -> 'a -> 'a) (* data to update *)
  (* fold over every edge leaving the subgraph *)
  -> (Edge.Ehn.halfid * (Vertex.name * Vertex.data) (* seen this vertex *)
      -> Vertex.name (* other vertex *)
      -> Edge.NSet.name * (dir_or_undir * Edge.data) (* edge *) 
      -> 'a -> 'a) (* data to update *)
  (* fold over the subgraph identified by this subset of vertices *)
  -> Vertex.NSet.T 
  -> T (* in this graph *) 
  -> 'a -> 'a (* to update this data *)


(* get adjacent vertices to some vertex *)
val adj_vnames : T -> Vertex.name -> Vertex.NSet.T
val adj_vnames' : T -> (Edge.NSet.T * Edge.NSet.T) 
    -> Vertex.NSet.T ->  Vertex.NSet.T
    
(* get incoming, outgoing, undirected edges around vertex *)
val adj_edge_classes: T -> Vertex.name -> (Edge.NSet.T*Edge.NSet.T*Edge.NSet.T)

(* fold over adjacents *)
val fold_adj : (Edge.Ehn.name * (dir_or_undir * Edge.data) 
      -> Vertex.name -> 'a -> 'a) 
      -> T -> Vertex.name -> 'a -> 'a

(* filtered set of adj vertex names *)
val filtered_adj_vnames : (Vertex.name -> bool) 
      -> T -> Vertex.name -> Vertex.NSet.T

(* all incident edges and where they go to, by edge name *)
val adj_etab_to_vnames : T -> Vertex.name 
      -> Vertex.name Edge.NTab.T -> Vertex.name Edge.NTab.T

(* the vertices connected to the given set, but outside it *) 
val incident_vertices : T -> Vertex.NSet.T -> Vertex.NSet.T

(* get adj vertices (from edge info) and from vertex name *)

(* singletons have no other incident edges - they only have the one 
   to the given vertex *)
val adj_singleton_vs : T -> Vertex.name 
                        -> Edge.Ehn.name Vertex.NTab.T  (* singletons *)
                           * Vertex.NSet.T (* other adj *)
val adj_singleton_vs' : T -> (Edge.NSet.T * Edge.NSet.T)
                         -> Edge.Ehn.name Vertex.NTab.T (* singletons *)
                            * Vertex.NSet.T (* other adj *)

val adj_vertices_with_edges : T -> Vertex.name 
                   -> ((dir_or_undir * Edge.data) Edge.NTab.T) Vertex.NTab.T
val adj_vertices_with_edges' : T -> (Vertex.name * (Edge.NSet.T * Edge.NSet.T))
                    -> ((dir_or_undir * Edge.data) Edge.NTab.T) Vertex.NTab.T

val adj_vertices_with_enames : T -> Vertex.name 
                               -> Edge.NSet.T Vertex.NTab.T
val adj_vertices_with_enames' : T -> (Edge.NSet.T * Edge.NSet.T)
                                -> Edge.NSet.T Vertex.NTab.T

val get_self_loops : T -> Vertex.name -> Edge.data Edge.NTab.T

val in_enames : T -> Vertex.name -> Edge.NSet.T
val out_enames : T -> Vertex.name -> Edge.NSet.T
val pred_vnames : T -> Vertex.name -> Vertex.NSet.T
val succ_vnames : T -> Vertex.name -> Vertex.NSet.T

val get_adj_arities : T -> Vertex.name -> Arity.T Vertex.NTab.T

(* depth-first traversal *)
val dft : (Edge.name -> 'a -> 'a) -> T -> 'a -> 'a
val dft_with_roots : (Edge.name -> 'a -> 'a) -> T -> 'a -> ('a * Vertex.NSet.T)

(* directed acyclic graphs *)
val convert_to_dag : T -> T
val spanning_dag : T -> T
val spanning_tree : T -> T (* another spanning tree, using DAG traversal *)
val get_dag_ranks : T -> Vertex.name list list

(* unification morphisms *)
structure VUmorph : UMORPH;
sharing VUmorph.Entity.SharingWithData = Vertex.SharingWithData;
structure EUmorph : UMORPH;
sharing EUmorph.Entity.SharingWithData = Edge.SharingWithData;


(* graph isomorphisms *)
val get_isomorphisms : T -> T 
    -> (Vertex.InjEndo.T * Edge.InjEndo.T * Param.unif) Seq.seq 

val is_isomorphic : T -> T -> bool

(* copy a set of vertices, copying all adjacent edges. *)
val copy1_vset : Vertex.NSet.T 
    -> (Vertex.Rnm.T * Edge.Rnm.T) * T 
    -> (Vertex.Rnm.T * Edge.Rnm.T) * T

end;





(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  extra functions for Basic Graphs: provides rest of the GRAPH signature *)
functor GraphToolsFun(BasicGraph : BASIC_GRAPH) 
(*  : adds stuff needed to make a GRAPH (but old graph stuff is hidden)  *)
= struct

local
open BasicGraph;
in
structure Ehn = Edge.Ehn;

val is_empty = Vertex.NTab.is_element_empty o get_vertices;

val get_vertex_list = Vertex.NTab.list_of o get_vertices;
val get_edge_list = Edge.NTab.list_of o get_edges;

val get_vnames = Vertex.NTab.get_nameset o get_vertices;
val get_enames = Edge.NTab.get_nameset o get_edges;

(* implicit: g n *)
val lookup_vertex = Vertex.NTab.lookup o get_vertices
val lookup_edge = Edge.NTab.lookup o get_edges

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
    let val (_,(s,t)) = get_edge g (Edge.Ehn.get_ename ehn)
    in if Edge.Ehn.is_start ehn then s else t end;

fun get_ehn_data g ehn = snd (fst (get_edge g (Edge.Ehn.get_ename ehn)));

val get_vertex_data = fst oo get_vertex;
val get_edge_data = (snd o fst) oo get_edge;

fun get_vname_ehns g v = 
    let val (d,(ines,outes)) = (get_vertex g v)
    in
      Ehn.NSet.empty 
        |> Edge.add_start_ehns outes 
        |> Edge.add_end_ehns ines
    end;

val set_vertex_data = update_vertex_data o K;
val set_edge_data = update_edge_data o K;
val set_edge_data = update_edge_data o K;


(* change structure of graph *)
fun update_vertex_enames f n g = 
    update_vertices (Vertex.NTab.map_entry (fn (nd,ios) => (nd,f ios)) n) g;
fun update_edge_vnames f en g = 
    update_edges (Edge.NTab.map_entry (fn (e,fromto) => (e,f fromto)) en) g;


(* implicit args: nd g *)
val add_vertex = add_named_vertex Vertex.default_name;
val doadd_vertex = snd oo add_vertex;
fun new_named_vertex n nd g =
    let val (n2,g2) = add_named_vertex n nd g in
      if Vertex.name_eq (n2, n) then g2 else 
      raise vname_already_exists_exp (n, g)
    end;

(* implicit args: g n *)
val has_vname = Vertex.NTab.contains_name o get_vertices;
val has_ename = Edge.NTab.contains_name o get_edges;

(* implicit args: e n1 n2 g *)
val add_edge = add_named_edge Edge.default_name;
val doadd_edge = snd oooo add_edge;
fun new_named_edge en e n1 n2 g =
    let val (en2,g2) = add_named_edge en e n1 n2 g in
      if Edge.name_eq (en2, en) then g2 else 
      raise ename_already_exists_exp (en,g)
    end;


fun update_vertex_ins_and_outs f n = 
    update_vertices (Vertex.NTab.map_entry 
                       (fn (nd,ins_and_outs) => (nd,f ins_and_outs)) n);

(* swap direction of edge en *)
fun swap_edge_dir en g = 
    let 
      val (d,(fromn, ton)) = get_edge g en
    in
      g |> (update_vertex_ins_and_outs 
              (fn (eins,eouts) => (Edge.NSet.add en eins,Edge.NSet.delete en eouts)) 
              fromn)
        |> (update_vertex_ins_and_outs 
              (fn (eins,eouts) => (Edge.NSet.delete en eins,Edge.NSet.add en eouts)) 
              ton)
        |> update_edges (Edge.NTab.map_entry (K (d,(ton, fromn))) en)
    end;


(* merge graphs, assumes disjoint edge names adds new vertices, keeps
   vertices with same name already in second graph. *)
fun merge_by_vertices g1 g2 = 
    g2 |> (Vertex.NTab.fold 
             (fn (vn,(d,_)) => fn g3 => 
                 if has_vname g3 vn then g3 else new_named_vertex vn d g3)
             (get_vertices g1))
       |> (Edge.NTab.fold 
             (fn (en,(d,(vn1,vn2))) => fn g3 => 
                 if has_ename g3 en then 
                   raise ename_already_exists_exp (en, g3)
                 else new_named_edge en d vn1 vn2 g3)
             (get_edges g1));

(* delete subg from fromg; assumes edge and vetrex names of subg are a
   subset of fromg; else delete will raise exception *)
fun delete_subgraph subg fromg = 
    fromg |> (Edge.NSet.fold 
                delete_edge 
                (get_enames subg))
          |> (Vertex.NSet.fold 
                delete_vertex
                (get_vnames subg));



fun try_rename1_vname en1 en2 g = 
    SOME (rename_vname en1 en2 g)
    handle vname_already_exists_exp _ => NONE;

fun try_rename1_ename en1 en2 g = 
    SOME (rename_ename en1 en2 g)
    handle ename_already_exists_exp _ => NONE;

(* compound renamings: for renaming vertices/edges in a graph *)
structure VertexCRnm = CompoundRenamingFun(
struct 
  structure Nm = Vertex
  type obj = T
  val try_rename1 = try_rename1_vname;
  val get_nameset = get_vnames
end);

structure EdgeCRnm = CompoundRenamingFun(
struct 
  structure Nm = Edge
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
      val vrn = Vertex.Rnm.mk_from_avoidset (get_vnames g1)
      val ern = Edge.Rnm.mk_from_avoidset (get_enames g1)
    in rename (vrn,ern) g2 end;


(* rename internal data apart *)
(* IMPROVE: don't do the slow fold over everything; use dependency info in 
   NameDeps to rename only the needed parts, etc. *)
structure CRnmINames : COMPOUND_RENAMING
= struct 
  type obj = T
  type renaming = Param.CRnmINames.renaming;
  val empty_renaming = Param.CRnmINames.empty_renaming;

  fun rename rnm g = 
    (rnm,g)
      |> (Vertex.NTab.fold
           (fn (vn,(d,es)) => fn (rnm, g) => 
               let val (rnm2,d2) = Param.VDataCRnmINames.rename rnm d 
               in (rnm2, set_vertex_data d2 vn g) end
           ) (get_vertices g))
      |> (Edge.NTab.fold
           (fn (en,((dir,d),vs)) => fn (rnm,g) => 
               let val (rnm2,d2) = Param.EDataCRnmINames.rename rnm d 
               in (rnm2,set_edge_data d2 en g) end
           ) (get_edges g));

  fun mk_renaming g rnm = 
    rnm
      |> (Vertex.NTab.fold
           (fn (vn,(d,es)) => Param.VDataCRnmINames.mk_renaming d)  
           (get_vertices g))
      |> (Edge.NTab.fold
           (fn (en,((dir,d),vs)) => Param.EDataCRnmINames.mk_renaming d)
           (get_edges g));

  fun do_renaming rnm g = 
      g |> (Vertex.NTab.fold
           (fn (vn,(d,es)) => 
               update_vertex_data (Param.VDataCRnmINames.do_renaming rnm) vn)
           (get_vertices g))
        |> (Edge.NTab.fold
           (fn (en,((dir,d),vs)) =>  
               update_edge_data (Param.EDataCRnmINames.do_renaming rnm) en)
           (get_edges g));
  
  val restrict_to_avoiding = Param.CRnmINames.restrict_to_avoiding;
end;

(* *)
fun rename_graph_data_apart g1 g2 = 
    let val rnm = CRnmINames.restrict_to_avoiding 
                    (CRnmINames.mk_renaming g1 CRnmINames.empty_renaming)
    in CRnmINames.rename rnm g2 end;

(* fold: pass over every vertex (fv) edge between the selected vertices 
   (fe_in); and over every edge going out of the selected vertex set (fe_out).
   There is an invariant which is that edges are considered only after at 
   last one end-point is considered. The considered end-point is passed as a
   parameter to the edge-fold functions. *)
(* the (') function, starts form vertex v *) 
fun fold_subgraph' fv fe_in fe_out vs g a = 
    let fun edgef (x as (h_end,(v,vd))) seen_edges e a = 
            if Edge.NSet.contains seen_edges e then a
            else 
              let val (ed,(fromv,tov)) = get_edge g e in
                case h_end of 
                  Edge.Ehn.TargetEnd => 
                  if Vertex.NSet.contains vs fromv then fe_in x tov (e,ed) a
                  else fe_out x tov (e,ed) a
                | Edge.Ehn.SourceEnd => 
                  if Vertex.NSet.contains vs fromv then fe_in x fromv (e,ed) a
                  else fe_out x fromv (e,ed) a
              end;
    in
      Vertex.NSet.fold 
        (fn v => fn (seen_edges,a) =>
          let val (vd,(in_es,out_es)) = get_vertex g v in 
             (seen_edges |> Edge.NSet.union_merge out_es 
                         |> Edge.NSet.union_merge in_es,
              a |> fv (v,vd)
                |> Edge.NSet.fold 
                    (edgef (Edge.Ehn.TargetEnd,(v,vd)) seen_edges) in_es
                |> Edge.NSet.fold 
                    (edgef (Edge.Ehn.SourceEnd,(v,vd)) seen_edges) out_es)
          end)
        vs (Edge.NSet.empty,a)
    end;

(* ignore the 'seen' edges *)
fun fold_subgraph fv fe_in fe_out vs g a = 
    snd (fold_subgraph' fv fe_in fe_out vs g a);

(* fold a function over the adjacent vertex names. 
   if "x <-e- y" then: "f (Ehn.mk_start e,...)"
   if "x -e-> y" then: "f (Ehn.mk_end e,...)"
   [ The edge-half the half connected to the adj vertex ] *)
fun fold_adj' f graph (ins,outs) a = 
    a |> (Edge.NSet.fold 
         (fn e => let val (d,(from,to)) = get_edge graph e 
          in f (Edge.Ehn.mk_start e,d) from end) ins)
      |> (Edge.NSet.fold 
         (fn e => let val (d,(from,to)) = get_edge graph e 
          in f (Edge.Ehn.mk_end e,d) to end) outs);

(* all vertices adjacent to vertex named n; 
   g = graph; f = function; a = accumulator *)
fun fold_adj f g n a = 
    fold_adj' f g (snd (get_vertex g n)) a;

(* create table of edge name to adj *)
val adj_etab_to_vnames = 
    fold_adj (fn (eh, d) => fn n2 => fn etab => 
                 let val e = Edge.Ehn.get_ename eh
                 in if Edge.NTab.contains etab e then etab
                    else Edge.NTab.update (e,n2) etab
                 end);

(* IMPROVE: use fold function ! *)
(* make a table: (Edge.data Edge.NTab.T) Vertex.NTab.T
holding every edge's data that is connects n and to the key Vertex.NTab, 
the Vertex.NTab has every vertex adjacent to n. (Note: ignores direction) *)
fun adj_vertices_with_edges' g (n,(ins,outs)) = 
    Vertex.NTab.empty
      |> (Edge.NSet.fold 
            (fn e => fn ntab => 
             let val (d,(from,to)) = get_edge g e
             in if Vertex.name_eq (from,n) then ntab (* no self-loops *)
                else if Vertex.NTab.contains ntab from
                then 
                  Vertex.NTab.map_entry 
                    (Edge.NTab.ins (e,d)) (* no dup exp for well-formed graphs *)
                    from ntab
                else Vertex.NTab.ins (from,Edge.NTab.ins (e,d) Edge.NTab.empty) ntab
             end)
            ins)
      |> (Edge.NSet.fold 
            (fn e => fn ntab => 
             let val (d,(from,to)) = get_edge g e
             in if Vertex.name_eq (to,n) then ntab (* no self-loops *)
                else if (Vertex.NTab.contains ntab to) then   
                  Vertex.NTab.map_entry 
                    (Edge.NTab.ins (e,d)) (* no dup exp for well-formed graphs *)
                  to ntab
                else Vertex.NTab.ins (to,Edge.NTab.ins (e,d) Edge.NTab.empty) ntab
             end)
            outs);

fun get_self_loops' g n ins = 
    Edge.NTab.empty
      |> (Edge.NSet.fold 
            (fn e => fn etab => 
             let val ((bd,d),(from,to)) = get_edge g e
             in if Vertex.name_eq (n,from) 
                then Edge.NTab.ins (e,d) etab else etab
             end)
            ins)

fun get_self_loops g n = 
    get_self_loops' g n (fst (snd (get_vertex g n)));

fun adj_vertices_with_edges g n = 
    adj_vertices_with_edges' g (n,snd (get_vertex g n));

(* IMPROVE: use fold function to simplify this *)
fun adj_vertices_with_enames' g (ins,outs) = 
    Vertex.NTab.empty
      |> (Edge.NSet.fold 
            (fn e => fn ntab => 
             let val (_,(from,to)) = get_edge g e
             in if (Vertex.NTab.contains ntab from) then 
                  Vertex.NTab.map_entry 
                    (Edge.NSet.add e) (* no dup exp for well-formed graphs *)
                    from ntab
                else 
                  Vertex.NTab.ins (from,Edge.NSet.add e Edge.NSet.empty) ntab
             end)
            ins)
      |> (Edge.NSet.fold 
            (fn e => fn ntab => 
             let val (d,(from,to)) = get_edge g e
             in if (Vertex.NTab.contains ntab to) then 
                  Vertex.NTab.map_entry 
                    (Edge.NSet.add e) (* no dup exp for well-formed graphs *)
                    to ntab
                else 
                  Vertex.NTab.ins (to,Edge.NSet.add e Edge.NSet.empty) ntab
             end)
            outs);

fun adj_vertices_with_enames g n = 
    adj_vertices_with_enames' g (snd (get_vertex g n));

(* IMPROVE: use fold function to simplify this *)
(* find all adjacent vertices which have no other egdes than the one
   (from the given ins/outs) of this vertex *)
fun adj_singleton_vs' g (ins,outs) = 
    (Vertex.NTab.empty, Vertex.NSet.empty)
      |> (Edge.NSet.fold 
            (fn e => fn (adjinfo as (stab, notsset)) => 
             let val (_,(from,to)) = get_edge g e
             in if Vertex.NSet.contains notsset from then adjinfo
                else if Vertex.NTab.contains stab from then 
                  (Vertex.NTab.delete from stab, 
                   Vertex.NSet.add from notsset)
                else 
                  (Vertex.NTab.ins (from,Edge.Ehn.mk_start e) stab, 
                   notsset)
             end)
            ins)
      |> (Edge.NSet.fold 
            (fn e => fn (adjinfo as (stab, notsset)) => 
             let val (_,(from,to)) = get_edge g e
             in if Vertex.NSet.contains notsset to then adjinfo
                else if Vertex.NTab.contains stab to then 
                  (Vertex.NTab.delete to stab, Vertex.NSet.add to notsset)
                else 
                  (Vertex.NTab.ins (to,Edge.Ehn.mk_end e) stab, notsset)
             end)
            outs);
fun adj_singleton_vs g n = adj_singleton_vs' g (snd (get_vertex g n));


(* all vertices adjacent to vertex *)
val adj_vnames' = fold_adj' (K Vertex.NSet.add);
fun adj_vnames g n = fold_adj (K Vertex.NSet.add) g n Vertex.NSet.empty;

(* adjacent edge classes *)
fun adj_edge_classes g n = let
       val (_,(ins,outs)) = get_vertex g n
       fun add_if_undir e ns = ns |> let val ((d,_),_) = get_edge g e
                                     in if d = UnDirected then Edge.NSet.add e
                                                          else I
                                     end
       val undirs = Edge.NSet.empty |> Edge.NSet.fold add_if_undir ins
                                    |> Edge.NSet.fold add_if_undir outs
    in (Edge.NSet.subtract ins undirs,Edge.NSet.subtract outs undirs,undirs) end;

(* filtered set of adj vertex *)
fun filter_adj_fn f (e : Edge.Ehn.name * (dir_or_undir * Edge.data)) n s = 
    if (f n) then Vertex.NSet.add n s else s;
val filtered_adj_vnames' = fold_adj' o filter_adj_fn;
fun filtered_adj_vnames f g n = 
    fold_adj (filter_adj_fn f) g n Vertex.NSet.empty;;


(* some more convenience functions for directed graphs *)
fun in_enames g = fst o snd o (get_vertex g);
fun out_enames g = snd o snd o (get_vertex g);

(* names of adj vertices with edges going to them *)
fun succ_vnames g vname =
    adj_vnames' g (Edge.NSet.empty, out_enames g vname) Vertex.NSet.empty;

(* names of adj vertices with edges coming in from them *)
fun pred_vnames g n =
    adj_vnames' g (in_enames g n, Edge.NSet.empty) Vertex.NSet.empty;

(* vertices incident into vns *)
(* the vertices connected to the given set, but outside it *) 
fun incident_vertices g vns = 
    Vertex.NSet.fold
      (fn n => 
          Vertex.NSet.fold 
            (fn n2 => 
                if Vertex.NSet.contains vns n2 then I
                else Vertex.NSet.add n2)
            (adj_vnames g n))
      vns Vertex.NSet.empty;

(* edge names between two vertcies *)
fun enames_between g n1 n2 = 
    let 
      val (_,(ins1,outs1)) = get_vertex g n1
      val (_,(ins2,outs2)) = get_vertex g n2
    in
      Edge.NSet.union_merge
        (Edge.NSet.intersect ins1 outs2)
        (Edge.NSet.intersect ins2 outs1)
    end;    

(* 
fun hens_between g n1 n2 = 
    let 
      val (_,(ins1,outs1)) = get_vertex g n1
      val (_,(ins2,outs2)) = get_vertex g n2
    in
      Ehn.NSet.union_merge
        (Edge.NSet.intersect ins1 outs2)
        (Edge.NSet.intersect ins2 outs1)
    end;
*)


(* *)
fun has_edges_between g n1 n2 = 
    let 
      val (_,(ins1,outs1)) = get_vertex g n1
      val (_,(ins2,outs2)) = get_vertex g n2
    in
       (Edge.NSet.nonempty_intersect ins2 outs1 orelse 
        Edge.NSet.nonempty_intersect ins1 outs2)
    end;

(* edge names between two vertcies *)
fun dir_enames_between g n1 n2 = 
    let 
      val (_,(ins1,outs1)) = get_vertex g n1
      val (_,(ins2,outs2)) = get_vertex g n2
    in
       (Edge.NSet.intersect ins2 outs1,
        Edge.NSet.intersect ins1 outs2)
    end;

val enames_between = uncurry Edge.NSet.union_merge ooo dir_enames_between;


fun edges_between g n1 n2 = 
    Edge.NSet.fold 
      (fn e => Edge.NTab.ins (e, fst (get_edge g e)))
      (enames_between g n1 n2)
      Edge.NTab.empty;

fun halfedges_between g n1 n2 = 
    let val (lr,rl) = dir_enames_between g n1 n2 in
      Edge.Ehn.NTab.empty
        |> (Edge.NSet.fold 
              (fn e => Edge.Ehn.NTab.ins 
                         (Edge.Ehn.mk_start e, fst (get_edge g e))) lr)
        |> (Edge.NSet.fold 
              (fn e => Edge.Ehn.NTab.ins 
                         (Edge.Ehn.mk_end e, fst (get_edge g e))) rl)
    end;

fun dir_edges_between g n1 n2 = 
    let val (lr,rl) = dir_enames_between g n1 n2 in
      (Edge.NSet.fold 
        (fn e => Edge.NTab.ins (e, fst (get_edge g e))) lr
        Edge.NTab.empty,
       Edge.NSet.fold 
        (fn e => Edge.NTab.ins (e, fst (get_edge g e))) rl
        Edge.NTab.empty)
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
      (Vertex.NSet.eq (Vertex.NTab.get_nameset v1s)
                      (Vertex.NTab.get_nameset v2s)) andalso 
      (* check same data, and same incident edges structure *)
      (Vertex.NTab.forall 
         (fn (n,(d,(fromes,toes))) => 
             case Vertex.NTab.lookup v2s n of 
               NONE => false
             | SOME (d2,(fromes2,toes2)) => 
               (Vertex.data_eq (d,d2)) andalso 
               (* now check structure *)
               (Edge.NSet.eq fromes fromes2) andalso
               (Edge.NSet.eq toes toes2) )
         v1s) andalso 
      (* now check same edge data *)
      (Edge.NTab.forall (* note that we dont re-check structure *)
         (fn (e,((bd,ed),_)) => 
             (* from above, we know we have same edge names: could use get *)
             case Edge.NTab.lookup e2s e of 
               NONE => false
             (* now check have same edge data *)
             | SOME ((bd2,ed2),_) => Edge.data_eq (ed,ed2))
         e1s)
    end;

(* Traverse the graph depth-first, folding on fld. The output is the
 * new structure and a set of all the chosen root vertices. *)
fun dft_with_roots fld graph base = let
  val out_edges = fn v => Edge.NSet.list_of (#2(#2(get_vertex graph v)));
  fun tree [] base rem = (base, rem) (* leaf/singleton *)
    | tree (e::es) base rem =
	    let val targ = #2(#2(get_edge graph e))
	    in if Vertex.NSet.contains rem targ then
	         let
		         (* recurse on children *)
		         val (base', rem') = tree (out_edges targ) (fld e base)
					                            (rem|>Vertex.NSet.delete targ)
	         in tree es base' rem' (* recurse on siblings *)
	         end
	       else (base, rem)  (* cycle *)
	    end
  fun forest base rem roots =
	    if Vertex.NSet.is_empty rem then (base, roots)
	    else let val fst = the (rem |> Vertex.NSet.get_first)
		           val (base', rem') = tree (out_edges fst) base
					                              (rem|>Vertex.NSet.delete fst)
	         in forest base' rem' (roots|>Vertex.NSet.add fst)
	         end
in forest base (get_vnames graph) Vertex.NSet.empty
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
    val out_elist = Edge.NSet.list_of o (out_enames graph)

    (* generate a dag from the given edge list *)
    fun dag depth (e::es) dg vtab =
	let
	    val (edata, (s, t)) = get_edge graph e
	    val (dg', vtab') =
		(case Vertex.NTab.lookup vtab t
		  (* if target hasn't been seen, add edge and recurse *)
		  of NONE => dag (depth + 1) (out_elist t)
				 (dg |> new_edge e edata s t)
				 (vtab |> Vertex.NTab.update (t, depth + 1))
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
	  | fv (SOME v) = (case Vertex.NTab.lookup vtab v
			    of NONE => SOME v
			     | _ => fv (Vertex.NSet.next_bigger vnames v))
    in fv (Vertex.NSet.get_min vnames)
    end
    
    (* iteratively run dag until we run out of vertices *)
    fun dag_until_done dg vtab =
	case fresh_vert vtab
	 of NONE => dg
	  | (SOME v) => let
		val vtab = (Vertex.NTab.map_all (K ~1) vtab)
			       |> Vertex.NTab.update (v,0)
		val (dg',vtab') = dag 0 (out_elist v) dg vtab
	    in dag_until_done dg' vtab'
	    end
    val init_dg = Edge.NSet.fold delete_edge (get_enames graph) graph
in dag_until_done init_dg Vertex.NTab.empty
end

(* several common uses for dag_traverse *)
local
    fun ignore_edge _ _ _ _ = I
    fun keep_edge e edata s t g = #2(g|>add_named_edge e edata s t)
    (* flip_edge also removes self-loops *)
    fun flip_edge e edata s t g = if Vertex.name_ord (s,t) = EQUAL then g
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
    val outputs = Vertex.NSet.filter
		     (fn v => Edge.NSet.is_empty (out_enames dag v))
		     (get_vnames dag)
    val vnames = Vertex.NSet.list_of
		     (Vertex.NSet.subtract (get_vnames dag) outputs)
    fun test seen v = not (Vertex.NSet.contains seen v)
		      andalso Vertex.NSet.forall
				  (Vertex.NSet.contains seen)
				  (pred_vnames dag v)
    fun mk seen =
	case filter (test seen) vnames
	 of []=>[] | lst => lst :: mk (Vertex.NSet.add_list lst seen)
in mk Vertex.NSet.empty @ [Vertex.NSet.list_of outputs]
end

(* vertex and edge unification morphisms *)
structure VUmorph = UmorphFun(Vertex);
structure EUmorph = UmorphFun(Edge);

fun unif_edge_both_ways e1 e2 (u,u') = 
    (u |> Param.unif_edge e1 e2, u'|> Param.unif_edge e2 e1);

fun unif_vertex_both_ways v1 v2 (u,u') = 
    (u |> Param.unif_vertex v1 v2, u'|> Param.unif_vertex v2 v1);

(* v = vertex iso, 
   e = edge iso, 
   u = unif, 
   t = agenda, 
   g and h = graphs, 
*)
fun get_isomorphisms_unify_next_edge v e u t g h gs hs otherend = let
    val n1 = Option.valOf
            (Edge.NSet.get_local_bot gs)
    val ((dir1,d1),_) = get_edge g n1
  in 
    Seq.maps get_isomorphisms_unify_next
    (Seq.map_filter
      (fn n2 => 
          let val ((dir2,d2),_) = get_edge h n2 in
            if dir1 <> dir2 (* andalso Edge.data_eq(d1,d2) *) 
            then NONE 
            else SOME
              (v |> Vertex.InjEndo.add (otherend n1 g) (otherend n2 h),
               e |> Edge.InjEndo.add n1 n2,
               u |> unif_edge_both_ways d1 d2,
               Seq.append t  (* push unification of the target vertex of
                                this edge onto the unification queue *)
                 (Seq.single (otherend n1 g,
                              otherend n2 h)),
               g |> delete_edge n1,
               h |> delete_edge n2)
               handle Vertex.InjEndo.add_exp(_,_,_) => NONE
           end)
        (Edge.NSet.seq_of hs))
  end
and get_isomorphisms_unify_next (v,e,u,t,g,h) = 
  (case Seq.pull t of
    (* Nothing to unify; start unifying new connectivity component *)
    NONE                => let
       val gbot = the (Vertex.NSet.get_local_bot (get_vnames g))
       val hmin = Vertex.NSet.get_min (get_vnames h)
       fun maketuple n = (v,e,u,Seq.single (gbot,n),g,h);
    in Seq.map maketuple (Vertex.NSet.seq_of (get_vnames h)) end 
    (* Something to unify; return lazy list of all possible next steps *)
  | SOME ((vng,vnh),t') => let
       (* point undirected edges towards us *)
       fun attract e gr = let
           val ((dir,_),_) = get_edge gr e
         in if dir=UnDirected
            then swap_edge_dir e gr
            else gr end
       val (_,(_,gout)) = get_vertex g vng
       val (_,(_,hout)) = get_vertex h vnh
       val g = g |> Edge.NSet.fold attract gout
       val h = h |> Edge.NSet.fold attract hout
       (* some other helper stuff *)
       val (gd,(gin,gout)) = get_vertex g vng
       val (hd,(hin,hout)) = get_vertex h vnh
       fun esrc n x = let val (_,(f,t)) = get_edge x n
                 in f end;
       fun esnk n x = let val (_,(f,t)) = get_edge x n
                 in t end;
    in if true (*Vertex.data_eq (gd,hd)*)
       then case (Edge.NSet.is_empty gin,
                  Edge.NSet.is_empty hin,
                  Edge.NSet.is_empty gout,
                  Edge.NSet.is_empty hout) of
          (* no (more) edges; match up *)
          (true,true,true,true) =>
            Seq.single (v |> Vertex.InjEndo.add vng vnh,
                        e,
                        u |> unif_vertex_both_ways gd hd,
                        t',
                        g |> delete_vertex vng,
                        h |> delete_vertex vnh)
          (* some outgoing edges remaining *)
        | (_,_,false,false) =>
          get_isomorphisms_unify_next_edge v e u t g h gout hout esnk
          (* some incoming edges remaining *)
        | (false,false,_,_) => 
          get_isomorphisms_unify_next_edge v e u t g h gin hin esrc
          (* edge mismatch *)
        | _ => Seq.empty
       else Seq.empty (* Mismatching vertex data *)
     end handle no_such_vname_exp(_,_,_) (* consequence of multiple edges. 
                                            Vertex has already been successfully
                                            unified; skip. *)
             => get_isomorphisms_unify_next (v,e,u,t',g,h)
  ) handle Vertex.InjEndo.add_exp(_,_,_) => Seq.empty

(* v,e are working copies of the isomorphism to be constructed;
   t is the queue of vertex pairs in the source and target graph to be
   unified *)
fun get_isomorphisms_sub (v,e,u,t,g,h) = case (is_empty g,is_empty h) of
    (* both graphs are empty; done *)
    (true,true) => Seq.single (v,e,fst u)
    (* neither graph is empty; get possible next unification steps and
       proceed matching *)
  | (false,false) => Seq.maps get_isomorphisms_sub
                                      (get_isomorphisms_unify_next (v,e,u,t,g,h))
    (* one graph is empty and the other is not; no iso *)
  | _ => Seq.empty 
;
  
(* in:  graphs g, h *)
(* out: lazy list of (Vertex isomorphism*Edge isomorphism) containing all
        isomorphisms between g and h *)
fun get_isomorphisms g h  
  = get_isomorphisms_sub (Vertex.InjEndo.empty,Edge.InjEndo.empty,
                          (Param.empty_unif,Param.empty_unif),Seq.empty,g,h);

(* in:  graphs g, h *)
(* out: true if g~=h, false otherwise *)
val is_isomorphic : T -> T -> bool 
  = is_some o Seq.pull oo get_isomorphisms



fun copy1edge (ehn, dir_and_data) v v2 ((vrn,ern), g) =
    let val (e',g') = (* add edge, orient appropriately *)
          if Edge.Ehn.is_start ehn then add_edge dir_and_data v2 v g 
          else add_edge dir_and_data v v2 g
    in ((vrn,Edge.Rnm.do_compose_rename1 (Edge.Ehn.get_ename ehn) e' ern),g') end

  
  (* copy a set of vertices, making copy having names fresh w.r.t. given v set (vs) 
     and e set (es); copies edges to all vertices outside the selected set. 
     assumes: get_Xnames g <= Xs; for X = {e, v, b}
  *)
  fun copy1_vset vset ((vrn,ern),g) = 
      Vertex.NSet.fold 
        (fn v => fn ((vrn,ern),g) => (* copy each vertex *)
            let val (v',g') = add_vertex (get_vertex_data g v) g 
                val vrn' = Vertex.Rnm.do_compose_rename1 v v' vrn
            in
              fold_adj (* for each connected edge *)
                (fn (ehn, dir_and_data) => fn v2 => fn (r_g as ((vrn,ern), g)) =>
                    (* has the other end of the edge already been added? *)
                    if Vertex.NSet.contains vset v2 then 
                      (case Vertex.Rnm.lookup vrn v2 of 
                           NONE => r_g
                         (* add edges to any other adjacent stuff that has already been copied *)
                         | SOME v2' => copy1edge (ehn, dir_and_data) v' v2' r_g)
                     else (* if outside of vset, copy edge to vertex outside *)
                        copy1edge (ehn, dir_and_data) v' v2 r_g
                ) g v ((vrn',ern),g)
             end
        ) vset ((vrn,ern), g);

  
  fun init_arity (eh,(UnDirected,ed)) = 
      Arity.update_undir (fn x => x + 1) Arity.zero
    | init_arity (eh,(Directed,ed)) = 
      if Edge.Ehn.is_start eh then Arity.update_in (fn x => x + 1) Arity.zero
      else Arity.update_out (fn x => x + 1) Arity.zero;

  fun add_edge_to_arity (eh,(UnDirected,ed)) arity = 
      Arity.update_undir (fn x => x + 1) arity 
    | add_edge_to_arity (eh,(Directed,ed)) arity =
      if Edge.Ehn.is_start eh then Arity.update_in (fn x => x + 1) arity 
      else Arity.update_out (fn x => x + 1) arity; 

  fun get_adj_arities g n = 
      let   
        fun add_one_arity e n2 ntab =
            if Vertex.NTab.contains_name ntab n2 then
              Vertex.NTab.map_entry (add_edge_to_arity e) n2 ntab
            else Vertex.NTab.update (n2,init_arity e) ntab;
      in 
        fold_adj add_one_arity g n Vertex.NTab.empty
      end;
  

end; (* local -- hides opened Graph data types *)
end;



(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  test that GraphToolsFun does what signature says... *) 
functor GraphFun(BasicGraph : BASIC_GRAPH) 
: GRAPH
= struct
  open BasicGraph;
  structure Tools = GraphToolsFun(BasicGraph);
  open Tools;
end;
