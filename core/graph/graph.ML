(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  extending Graphs have interesting/useful functions  *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

(*  GRAPH is BASIC_GRAPH + useful utilities/API *)
signature GRAPH = 
sig

include BASIC_GRAPH

(* graph has not vertices (and thus no edges) *)
val is_empty : T -> bool 

(* getting stuff from graphs (gets raise exceptions if no entry) *)
val lookup_vertex : T -> V.name 
									-> (VData.data * (E.NSet.T * E.NSet.T)) option
val lookup_edge : T -> E.name 
									-> ((dir_or_undir * EData.data) 
                      * (V.name * V.name)) option

(* getting stuff from graphs (exception if not there) *)
val get_edge_source : T -> E.name -> V.name
val get_edge_target : T -> E.name -> V.name

(* given an edge and vertex, get the other end *)
val edge_get_other_vertex : T -> E.name -> V.name -> V.name

val get_in_edges  : T -> V.name -> E.NSet.T
val get_out_edges : T -> V.name -> E.NSet.T

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
val add_named_vertex : V.name -> VData.data -> T -> T  

(* V.names must already exist, else raises: no_such_vname_exp *)
val add_edge : dir_or_undir * EData.data -> V.name -> V.name -> T -> E.name * T
val doadd_edge : dir_or_undir * EData.data -> V.name -> V.name -> T -> T
val add_named_edge 
    : E.name -> dir_or_undir * EData.data -> V.name -> V.name -> T -> T (* can raise *)
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

(* renaming internal names in a graph, used for variables within a vertex. *)
structure CRnmX : COMPOUND_RENAMING
where type obj = T and type renaming = X.Rnm.T
structure VertexCRnm : COMPOUND_RENAMING 
where type obj = T and type renaming = V.Rnm.T
structure EdgeCRnm : COMPOUND_RENAMING 
where type obj = T and type renaming = E.Rnm.T

val rename : (X.renaming * V.renaming * E.renaming) -> T 
    -> (X.renaming * V.renaming * E.renaming) * T 
(* rename the 2nd graph to avoid all names in the first one *)
val rename_apart : T -> T -> (X.renaming * V.renaming * E.renaming) * T 

(* merge two graphs which have distinct egdes by shared vertices:
   vertices with same name are kept only once, edges must be
   distinct. *)
val merge_by_vertices : T -> T -> T

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
val dir_enames_between : T -> V.name -> V.name 
      -> E.NSet.T (* from fst to snd vertex *) 
         * E.NSet.T (* from snd to fst vertex *) 
val dir_edges_between : T -> V.name -> V.name 
      -> (dir_or_undir * EData.data) E.NTab.T (* from fst to snd vertex *) 
         * (dir_or_undir * EData.data) E.NTab.T (* from snd to fst vertex *) 

(* get adjacent vertices to some vertex *)
val adj_vnames : T -> V.name -> V.NSet.T
    
(* get incoming, outgoing, undirected edges adjacent to vertex *)
val adj_edge_classes: T -> V.name -> (E.NSet.T*E.NSet.T*E.NSet.T)

datatype incidence_type = Incoming | Outgoing | SelfLoop;
(* Fold over the incident edges.
 *
 * Each incident edge will feature exactly once, as an
 * incoming edge, an outgoing edge or a self-loop.
 *
 * In addition to the edge information and the incidence
 * type, the vertex on other other end will be given.
 * In the case of self-loops, this will be the same as
 * the vertex originally passed.
 *
 * The folding order is undefined.
 *)
val fold_adj : (incidence_type * V.name * (E.name * (dir_or_undir * EData.data))
                -> 'a -> 'a)
      -> T -> V.name -> 'a -> 'a

(* the vertices connected to the given set, but outside it *) 
val incident_vertices : T -> V.NSet.T -> V.NSet.T

val get_self_loops : T -> V.name -> EData.data E.NTab.T

val in_enames : T -> V.name -> E.NSet.T
val out_enames : T -> V.name -> E.NSet.T
val pred_vnames : T -> V.name -> V.NSet.T
val succ_vnames : T -> V.name -> V.NSet.T

val get_arity : T -> V.name -> Arity.T

(* depth-first traversal *)
val dft : (E.name -> 'a -> 'a) -> T -> 'a -> 'a
val dft_with_roots : (E.name -> 'a -> 'a) -> T -> 'a -> ('a * V.NSet.T)

(* directed acyclic graphs *)
val convert_to_dag : T -> T
val spanning_dag : T -> T
val spanning_tree : T -> T (* another spanning tree, using DAG traversal *)
val get_dag_ranks : T -> V.name list list

(* Duplicates a subgraph (defined by a set of vertices) and
 * maintains its connectivity to the rest of the graph
 *)
val copy_subgraph : V.NSet.T
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
  
  val get_edge_source = (fst o snd) oo get_edge
  val get_edge_target = (snd o snd) oo get_edge
  
  fun edge_get_other_vertex g e v = let
    val (_,(s,t)) = get_edge g e
  in if V.name_eq(s,v) then t else s
  end
  
  val get_in_edges  = (fst o snd) oo get_vertex
  val get_out_edges = (snd o snd) oo get_vertex
  
  val get_vertex_data = fst oo get_vertex;
  val get_edge_data = (snd o fst) oo get_edge;

  val set_vertex_data = update_vertex_data o K;
  val set_edge_data = update_edge_data o K;
  
  
  (* change structure of graph *)
  fun update_vertex_enames f n g = 
      update_vertices (V.NTab.map_entry (fn (nd,ios) => (nd,f ios)) n) g;
  fun update_edge_vnames f en g = 
      update_edges (E.NTab.map_entry (fn (e,fromto) => (e,f fromto)) en) g;
  
  
  (* implicit args: nd g *)
  val add_vertex = add_vertex_named_like V.default_name;
  val doadd_vertex = snd oo add_vertex;
  fun add_named_vertex n nd g =
      let val (n2,g2) = add_vertex_named_like n nd g in
        if V.name_eq (n2, n) then g2 else 
        raise vname_already_exists_exp (n, g)
      end;
  
  (* implicit args: g n *)
  val has_vname = V.NTab.contains_name o get_vertices;
  val has_ename = E.NTab.contains_name o get_edges;
  
  (* implicit args: e n1 n2 g *)
  val add_edge = add_edge_named_like E.default_name;
  val doadd_edge = snd oooo add_edge;
  fun add_named_edge en e n1 n2 g =
      let val (en2,g2) = add_edge_named_like en e n1 n2 g in
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
                   if has_vname g3 vn then g3 else add_named_vertex vn d g3)
               (get_vertices g1))
         |> (E.NTab.fold 
               (fn (en,(d,(vn1,vn2))) => fn g3 => 
                   if has_ename g3 en then 
                     raise ename_already_exists_exp (en, g3)
                   else add_named_edge en d vn1 vn2 g3)
               (get_edges g1));

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
      handle xname_already_exists_exp _ => NONE;

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
  
  (* rename internal data apart *)
  (* IMPROVE: don't do the slow fold over everything; use dependency info in 
     NameDeps to rename only the needed parts, etc. *)
  (* structure CRnmX : COMPOUND_RENAMING
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
  end; *)
   
  structure CRnmX : COMPOUND_RENAMING
  = CompoundRenamingFun(
  struct 
    structure Nm = X
    type obj = T
    val try_rename1 = try_rename1_xname;
    val get_nameset = get_xnames;
  end);
  
  (*  *) 
  fun rename (xrn,vrn,ern) g1 = 
      let 
        val (xrn2,g2) = CRnmX.rename xrn g1
        val (vrn2,g3) = VertexCRnm.rename vrn g2
        val (ern2,g4) = EdgeCRnm.rename ern g3
      in ((xrn2,vrn2,ern2),g4) end;
  
  (* rename g2 apart from g1 *)
  fun rename_apart g1 g2 = 
      let 
        val xrn = X.Rnm.mk_from_avoids 
            (X.NSet.union_merge (get_xnames g1) (get_xnames g2))
        val vrn = V.Rnm.mk_from_avoids 
            (V.NSet.union_merge (get_vnames g1) (get_vnames g2))
        val ern = E.Rnm.mk_from_avoids 
            (E.NSet.union_merge (get_enames g1) (get_enames g2))
      in rename (xrn,vrn,ern) g2 end;   

  datatype incidence_type = Incoming | Outgoing | SelfLoop;

  fun fold_adj f g v a = let
    val (ins,outs) = snd (get_vertex g v)
  in
    a |> (E.NSet.fold
         (fn e => let
            val (d,(from,to)) = get_edge g e 
            val inctyp =
              if V.name_eq (from,to)
              then SelfLoop
              else Incoming
          in
            f (inctyp,from,(e,d))
          end) ins)
      |> (E.NSet.fold
         (fn e => let
            val (d,(from,to)) = get_edge g e 
          in
            if V.name_eq (from,to)
            then (fn x => x)
            else f (Outgoing,to,(e,d))
          end) outs)
  end;
  
  fun get_self_loops' g n ins = 
      E.NTab.empty
        |> (E.NSet.fold 
              (fn e => fn etab => 
               let val ((_,d),(from,_)) = get_edge g e
               in if V.name_eq (n,from) 
                  then E.NTab.ins (e,d) etab else etab
               end)
              ins)
  
  fun get_self_loops g n = 
      get_self_loops' g n (fst (snd (get_vertex g n)));
  
  (* all vertices adjacent to vertex *)
  fun adj_vnames g v = fold_adj (fn (_,v',_) => V.NSet.add v') g v V.NSet.Empty
  
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
  
  (* some more convenience functions for directed graphs *)
  fun in_enames g = fst o snd o (get_vertex g);
  fun out_enames g = snd o snd o (get_vertex g);
  
  (* names of adj vertices with edges going to them *)
  fun succ_vnames g vname =
      E.NSet.fold
           (fn e => let val (_,(_,to)) = get_edge g e 
            in V.NSet.add to end)
           (out_enames g vname)
           V.NSet.empty;
  
  (* names of adj vertices with edges coming in from them *)
  fun pred_vnames g vname =
      E.NSet.fold
           (fn e => let val (_,(from,_)) = get_edge g e 
            in V.NSet.add from end)
           (in_enames g vname)
           V.NSet.empty;
  
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
           (fn (e,((_,ed),_)) =>
               (* from above, we know we have same edge names: could use get *)
               case E.NTab.lookup e2s e of 
                 NONE => false
               (* now check have same edge data *)
               | SOME ((_,ed2),_) => EData.data_eq (ed,ed2))
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
      fun keep_edge e edata s t g = #2(g|>add_edge_named_like e edata s t)
      (* flip_edge also removes self-loops *)
      fun flip_edge e edata s t g = if V.name_ord (s,t) = EQUAL then g
            else #2(g|>add_edge_named_like e edata t s)
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
  fun copy1edge inctyp (e, dir_and_data) v v2 ((vrn,ern), g) =
      let 
        val (new_e_opt,ern') = E.Rnm.rename1 e ern;
        val (_,g') =
            (case new_e_opt of NONE => 
               raise no_such_ename_exp ("renaming doesn't have edge in avoid set",e,g)
             | SOME e' => 
               (e', case inctyp of
                         Incoming => add_named_edge e' dir_and_data v2 v g
                       | _ => add_named_edge e' dir_and_data v v2 g
                    ));
      in ((vrn,ern'),g') end;

  (* copy a set of vertices, making copy having names fresh w.r.t. 
     vrn and ern (edge and vertex renaming). 
     copies edges to all vertices outside the selected set. 
     assumes: (get_Xnames g) <= get_avoids(Xrn); for X = {e, v}.
  *)
  fun copy_subgraph vset ((vrn,ern),g) =
      V.NSet.fold 
        (fn v => fn ((vrn,ern),g) => (* copy each vertex *)
            let
              val (new_v_opt,vrn') = V.Rnm.rename1 v vrn
              val (v',g') = (case new_v_opt of NONE => 
                         raise no_such_vname_exp ("renaming doesn't have vertex in avoid set",v,g)
                       | SOME v' => 
                         (v',add_named_vertex v' (get_vertex_data g v) g))
            in
              fold_adj (* for each connected edge *)
                (fn (inctyp,v2,(e, dir_and_data)) =>
                 fn (r_g as ((vrn,_), g'')) =>
                    (* has the other end of the edge already been added? *)
                    if V.NSet.contains vset v2 then 
                      (case V.Rnm.lookup vrn v2 of 
                           NONE => r_g
                         (* add edges to any other adjacent stuff that has already been copied *)
                         | SOME v2' =>
                             if (has_vname g'' v2')
                             then copy1edge inctyp (e, dir_and_data) v' v2' r_g
                             else r_g)
                     else (* if outside of vset, copy edge to vertex outside *)
                        copy1edge inctyp (e, dir_and_data) v' v2 r_g
                ) g' v ((vrn',ern),g')
             end
        ) vset ((vrn,ern), g);

      
  fun get_arity g v =
      let   
        fun plus1 x = x + 1
        fun plus2 x = x + 2
        fun strip_args (inctyp,_,(_,(dirtyp,_))) = (inctyp,dirtyp)
        fun add_edge_to_arity (SelfLoop,UnDirected) =
            Arity.update_undir plus2
          | add_edge_to_arity (_,UnDirected) =
            Arity.update_undir plus1
          | add_edge_to_arity (Incoming,Directed) =
            Arity.update_in plus1
          | add_edge_to_arity (Outgoing,Directed) =
            Arity.update_out plus1
          | add_edge_to_arity (SelfLoop,Directed) =
            Arity.update_in plus1 o Arity.update_out plus1
      in 
        fold_adj (add_edge_to_arity o strip_args) g v Arity.zero
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
