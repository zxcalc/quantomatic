(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *)
(*  Representation Graphs with  Bang-Boxes *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *)
(* FIXME: make merge_by_vertices respect !-boxes. ? need general notion of
   data merging? *)

(* Quantomatic's implementation of pattern graphs (a.k.a. !-graphs) deviates
 * from the theoretical version in several ways.  The !-vertices are stored
 * separately to other vertex types, in their own namespace (B, rather than V).
 * The edges from !-vertices to non-!-vertices (ie: the contents of the !-box)
 * are stored implicitly as a binary relation.  Similarly, the edges between
 * !-vertices are stored as a partial order.  This is possible because there is
 * at most one edge between a !-vertex and any other vertex, and the !-vertex
 * must be the source of the edge.
 *)

(* Basic Bang Graphs: data and also basic functions *)
signature BASIC_BANG_GRAPH =
sig
include BASIC_OGRAPH;

exception bbox_already_exists_exp of B.name * T;

val get_bboxes : T -> B.NSet.T
(* Does not contain empty !-boxes (or unboxed vertices) *)
val get_bbox_rel : T -> BVBRel.T
(* Does not contain empty !-boxes *)
val get_bbox_table : T -> V.NSet.T B.NTab.T
(* Does not contain unboxed vertices *)
val get_vertex_bbox_table : T -> B.NSet.T V.NTab.T
(* all vertices that are in at least one !-box *)
val get_bboxed : T -> V.NSet.T
(* all vertices that are in at no !-boxes *)
val get_unbboxed : T -> V.NSet.T
val get_empty_bboxes : T -> B.NSet.T

val add_named_bbox : B.name -> T -> B.name * T
(* does not delete the contained vertices *)
(* identity if name does not exist *)
val delete_bbox : B.name -> T -> T

(* throws bbox_already_exists_exp on name clash *)
(* identity if original name does not exist *)
val rename_bbox : B.name -> B.name -> T -> T

(* must make sure the bbox_rel dom is a subset of bboxes *)
val update_bboxes : (B.NSet.T -> B.NSet.T) -> T -> T
val update_bbox_rel : (BVBRel.T -> BVBRel.T) -> T -> T

end;



(* bang-box graphs with all the extra functions defined in terms of the
   basic ones *)
signature BANG_GRAPH =
sig
include OGRAPH;

(* *** COPIED FROM BASIC_BANG_GRAPH ... would be better to have union version
   of include *** *)

exception bbox_already_exists_exp of B.name * T;

val get_bboxes : T -> B.NSet.T
(* Does not contain empty !-boxes (or unboxed vertices) *)
val get_bbox_rel : T -> BVBRel.T
(* Does not contain empty !-boxes *)
val get_bbox_table : T -> V.NSet.T B.NTab.T
(* Does not contain unboxed vertices *)
val get_vertex_bbox_table : T -> B.NSet.T V.NTab.T
(* all vertices that are in at least one !-box *)
val get_bboxed : T -> V.NSet.T
(* all vertices that are in at no !-boxes *)
val get_unbboxed : T -> V.NSet.T
val get_empty_bboxes : T -> B.NSet.T

val add_named_bbox : B.name -> T -> B.name * T
(* does not delete the contained vertices *)
(* identity if name does not exist *)
val delete_bbox : B.name -> T -> T

(* throws bbox_already_exists_exp on name clash *)
(* identity if original name does not exist *)
val rename_bbox : B.name -> B.name -> T -> T

(* we don't copy updaters/setters for internal structures,
 * since using these can lead to an inconsistent state *)

(* *** END OF COPIED *** *)

exception merge_bbox_exp of string
exception no_such_bbox_exp of unit
(* thrown when using old API on a graph with overlapping !-boxes *)
(* FIXME: remove *) exception multiple_bboxes_exp of unit

(* getters for bbox -> vert *)
val get_bbox_list : T -> B.name list
val has_bbox : T -> B.name -> bool
(* no_such_bbox_exp if bbox does not exist *)
val get_bbox_contents : T -> B.name -> V.NSet.T

(* getters for vert -> bbox *)
(* no_such_vname_exp if vertex does not exist *)
val get_bboxes_of : T -> V.name -> B.NSet.T
(* FIXME: remove *) val lookup_bbox_of : T -> V.name -> B.name option
(* FIXME: remove *) val get_bbox_of : T -> V.name -> B.name
(* no_such_vname_exp if vertex does not exist *)
val is_bboxed : T -> V.name -> bool

(* bbox_already_exists_exp *)
val new_named_bbox : B.name -> T -> T
val add_bbox : T -> B.name * T
val doadd_bbox : T -> T

val try_rename1_bbox : B.name -> B.name -> T -> T option

(* no_such_bbox_exp if !-box does not exist *)
val add_to_bbox : B.name -> V.NSet.T -> T -> T
(* no_such_bbox_exp if !-box does not exist *)
(* does not raise if verts are not in !-box *)
val remove_from_bbox : B.name -> V.NSet.T -> T -> T
(* FIXME: remove *) val move_to_bbox : B.name -> V.NSet.T -> T -> T
(* FIXME: remove *) val move_to_unbbox : V.NSet.T -> T -> T

(* utility for folding across a list of graphs, adding the names from
 * each one *)
val add_to_bboxes : T -> B.NSet.T -> B.NSet.T

(* no_such_bbox_exp *)
val kill_bbox : B.name -> T -> T
(* no_such_bbox_exp *)
val drop_bbox : B.name -> T -> T
val merge_bboxes : B.NSet.T -> T -> B.name * T

(* copy a set of vertices, making copy having names fresh w.r.t.
   vrn and ern (edge and vertex renaming).
   copies edges to all vertices outside the selected set.
   assumes: (get_vnames g) <= get_avoids(vrn)
            (get_enames g) <= get_avoids(ern)
            (get_bboxes g) <= get_avoids(brn)
*)
val copy1_bbox : B.name (* bbox to copy *)
      -> ((V.renaming (* renaming for bbox copy/avoids *)
           * E.renaming
           * B.renaming)
          * T) (* graph containing bbox name *)
      -> (B.name (* new box name *)
          * ((V.renaming (* how vnames were copied *)
               * E.renaming (* ename copying *)
               * B.renaming) (* bbname copying *)
             * T)) (* new graph with bbox copied *)

val fresh_copy1_bbox : B.name (* bbox to copy *)
      -> T (* graph containing bbox name *)
      -> (B.name (* new box name *)
          * T) (* new graph with bbox copied *)

(* copy and drop *)
val expand1_bbox : B.name (* bbox to expand *)
      -> ((V.renaming (* renaming for bbox copy/avoids *)
           * E.renaming
           * B.renaming)
          * T) (* graph containing bbox name *)
      -> ((V.renaming (* how vnames were copied *)
               * E.renaming (* ename copying *)
               * B.renaming) (* bbname copying *)
             * T) (* new graph with bbox expanded *)

(* copy and drop *)
val fresh_expand1_bbox : B.name (* bbox to expand *)
      -> T (* graph containing bbox name *)
      -> T (* new graph with bbox expanded *)

(* FIXME: remove *) val get_filtered_adj_arities_and_bang_arities :
    (((H.name * (dir_or_undir * EData.data)) * V.name) -> bool)
    -> T -> V.name
    -> Arity.T V.NTab.T
       * Arity.T V.NTab.T B.NTab.T

(* FIXME: remove *) val get_adj_arities_and_bang_arities :
    T -> V.name
    -> Arity.T V.NTab.T
       * Arity.T V.NTab.T B.NTab.T

(* renaming of bang-box names *)
structure BBoxCRnm : COMPOUND_RENAMING
where type obj = T and type renaming = B.Rnm.T

val rename_apart_incl_bboxes : T -> T ->
  (V.renaming * E.renaming * B.renaming) * T

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *)
(* A  Graph With Boundary *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *)
functor BasicBangGraphFun(
        structure Param : OGRAPH_PARAM
        type data
        val empty_data : data
) : BASIC_BANG_GRAPH
= struct

  (* IDEA: would be handy to have a warning for type overloading; can be tricky to see! e.g. if OVertex = Param.Vertex ... *)

  (* to avoid using clobbered data and empty graph from the opened Basic Graph *)
  type data2 = data;
  val empty_data2 = empty_data;

  (* we add bbox to Graph data, this is hidden *)
  datatype hdata =
           HData of
           { bboxes : B.NSet.T,
             bbox_rel : BVBRel.T,
             data : data
           };
  fun get_bboxes_of_hdata (HData rep) = #bboxes rep;
  fun get_bbox_rel_of_hdata (HData rep) = #bbox_rel rep;
  fun get_data_of_hdata (HData rep) = #data rep;
  fun update_bboxes_of_hdata f (HData rep) =
      HData { data = #data rep,
              bbox_rel = #bbox_rel rep,
              bboxes = f (#bboxes rep)
            };
  fun update_bbox_rel_of_hdata f (HData rep) =
      HData { data = #data rep,
              bbox_rel = f (#bbox_rel rep),
              bboxes = #bboxes rep
            };
  fun update_data_of_hdata f (HData rep) =
      HData { data = f (#data rep),
              bbox_rel = #bbox_rel rep,
              bboxes = #bboxes rep
            };
  val empty_hdata = HData { bboxes = B.NSet.empty,
                     bbox_rel = BVBRel.empty,
                     data = empty_data};

  (* make basic graph with this as internal data *)
  structure G : BASIC_OGRAPH = BasicOGraphFun(
    structure Param = Param
    type data = hdata
    val empty_data = empty_hdata
  );
  (* inherit functions from Graph *)
  open G;

  (* clobber sub-graph data stuff *)
  type data = data2;
  val empty_data = empty_data2;
  val get_data = get_data_of_hdata o G.get_data;
  val update_data = G.update_data o update_data_of_hdata;

  exception bbox_already_exists_exp of B.name * T;

  (* for bbox part of added data *)
  val get_bboxes : T -> B.NSet.T = get_bboxes_of_hdata o G.get_data;
  val get_bbox_rel : T -> BVBRel.T = get_bbox_rel_of_hdata o G.get_data;
  val get_bbox_table = BVBRel.get_domtab o get_bbox_rel
  val get_vertex_bbox_table = BVBRel.get_codtab o get_bbox_rel
  (* all vertices that are in at least one bbox *)
  val get_bboxed = V.NTab.get_nameset o get_vertex_bbox_table;
  fun get_unbboxed g =
    let
      val boxrel = get_bbox_rel g
      val verts = V.NTab.get_nameset (get_vertices g)
    in
      V.NSet.subtract verts (BVBRel.get_codset boxrel)
    end
  fun get_empty_bboxes g =
    let val boxrel = get_bbox_rel g in
      B.NSet.subtract (get_bboxes g) (BVBRel.get_domset boxrel)
    end

  val update_bboxes = G.update_data o update_bboxes_of_hdata;
  val update_bbox_rel = G.update_data o update_bbox_rel_of_hdata;

  fun add_named_bbox bn g =
    let
      val (bn',bs') = B.NSet.add_new bn (get_bboxes g)
    in
      (bn', g |> update_bboxes (fn _ => bs'))
    end
  fun delete_bbox bn g =
        g |> update_bboxes (B.NSet.delete bn)
          |> update_bbox_rel (BVBRel.del_dom bn)

  (* raises bbbox_already_exists_exp if bbox name already exists *)
  fun rename_bbox n1 n2 g =
      g |> update_bboxes (fn bxs =>
                             case B.NSet.try_rename1 n1 n2 bxs
                               of NONE => raise bbox_already_exists_exp (n2,g)
                                | SOME bxs2 => bxs2)
        |> update_bbox_rel (fn bxs =>
                             case BVBRel.try_rename1_dom n1 n2 bxs
                              of NONE => raise bug_exp "bbox in bbox_rel but not bboxes"
                               | SOME bxs2 => bxs2);

  (* modified graph functions *)
  fun delete_vertex n g =
      g |> update_bbox_rel (BVBRel.del_cod n)
        |> G.delete_vertex n;

  fun rename_vname n1 n2 g =
      g |> G.rename_vname n1 n2
        (* the NONE case should never happen: all vertices already occur in
           graph, so G.rename_vname should have raised an exeption. *)
        |> update_bbox_rel (the o BVBRel.try_rename1_cod n1 n2);

  (* prettifying *)
  fun pretty g =
      Pretty.chunks
        [ Pretty.str "BangBoxGraph{",
          Pretty.block
            [Pretty.str "  ", (* indent *)
             Pretty.chunks
               ((if B.NSet.is_empty (get_bboxes g) then []
                 else [Pretty.block
                         [Pretty.str "Bang-Boxes: ",
                          B.NSet.pretty (get_empty_bboxes g)],
                       Pretty.block
                         [Pretty.str "Bang-Box relation: ",
                          BVBRel.pretty (get_bbox_rel g)]])
                @ [G.pretty g])],
          Pretty.str "}"
        ];

  val print = Pretty.writeln o pretty;

end (* Functor BasicBangGraphFun : ... -> BASIC_BANG_GRAPH *)


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *)
(*  functor make a BANG_GRAPH *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *)
(* Given a BASIC_BANG_GRAPH, provide it with various tools *)
functor BangGraphTools(G : BASIC_BANG_GRAPH)
: BANG_GRAPH
= struct

  (* provide basic bang-graph functions and sub-structures  *)
  open G;

  (* provide open graph functions/tools *)
  structure OGraph = OGraphFun(G);
  open OGraph;

  exception no_such_bbox_exp of unit;
  exception merge_bbox_exp of string
  (* FIXME: remove *) exception multiple_bboxes_exp of unit

  (* getters for bbox -> vert *)
  val get_bbox_list = B.NSet.list_of o get_bboxes;
  val has_bbox = B.NSet.contains o get_bboxes;
  fun get_bbox_contents g b =
    if has_bbox g b then
      (case B.NTab.lookup (get_bbox_table g) b of
            NONE    => V.NSet.empty
          | SOME vs => vs)
    else
      raise no_such_bbox_exp ()

  (* getters for vert -> bbox *)
  (* lookup names of boxes that v is in *)
  fun get_bboxes_of g v =
      case V.NTab.lookup (get_vertex_bbox_table g) v of
           NONE => (if not (V.NTab.contains_name (get_vertices g) v) then
                      raise no_such_vname_exp ("get_bboxes_of", v, g)
                    else B.NSet.empty)
         | SOME bs => bs;
  fun lookup_bbox_of g v =
    let
      val bboxes = get_bboxes_of g v
    in
      if B.NSet.is_empty bboxes then NONE
      else case B.NSet.tryget_singleton bboxes of
                NONE => raise multiple_bboxes_exp ()
              | SOME b => SOME b
    end
  fun get_bbox_of g v =
      case lookup_bbox_of g v of
           NONE => raise no_such_bbox_exp ()
         | SOME b => b;
  fun is_bboxed g v =
    if V.NSet.contains (get_vnames g) v then
      V.NSet.contains (get_bboxed g) v
    else
      raise no_such_vname_exp ("is_bboxed", v, g)

  fun new_named_bbox b g =
    let val (b',g') = add_named_bbox b g in
      if B.name_eq (b',b) then g'
      else raise bbox_already_exists_exp (b,g)
    end
  val add_bbox = add_named_bbox B.default_name
  val doadd_bbox = snd o add_bbox

  fun try_rename1_bbox n1 n2 g =
      (SOME (rename_bbox n1 n2 g)) handle bbox_already_exists_exp _ => NONE;

  fun add_to_bbox b vs g =
    if not (has_bbox g b) then
      raise no_such_bbox_exp ()
    else if not (V.NSet.sub_set vs (get_vnames g)) then
      raise no_such_vnames_exp ("add_to_bbox",vs,g)
    else
      g |> update_bbox_rel (BVBRel.add_to_dom b vs);

  fun remove_from_bbox b vs g =
    if not (has_bbox g b) then
      raise no_such_bbox_exp ()
    else
      g |> update_bbox_rel (BVBRel.del_from_dom b vs);

  fun move_to_bbox bn vs g =
    g |> update_bbox_rel (BVBRel.add_to_dom bn vs
                        o V.NSet.fold BVBRel.del_cod vs)
      |> update_bboxes (B.NSet.add bn);
  fun move_to_unbbox vs g =
      g |> update_bbox_rel (V.NSet.fold BVBRel.del_cod vs);

  val add_to_bboxes = B.NSet.union_merge o get_bboxes;

  fun kill_bbox bn g =
      let val bbox_vs = get_bbox_contents g bn in
        g |> V.NSet.fold delete_vertex bbox_vs
          |> delete_bbox bn
      end;

  fun drop_bbox b g =
    if not (has_bbox g b) then
      raise no_such_bbox_exp ()
    else
      delete_bbox b g;

  (* returns merged bbox-name and new merged graph *)
  fun merge_bboxes bnset g =
      (case B.NSet.pull_local_top bnset of
         NONE => raise merge_bbox_exp "given empty bbox name set"
       | SOME (bn,bnset2) =>
         (bn, B.NSet.fold
              (fn bn2 => fn g2 =>
                   g2 |> add_to_bbox bn (get_bbox_contents g2 bn2)
                      |> delete_bbox bn2)
              bnset2 g));

  (* copy bang box, given renaming info, or possibly with dictated renaming. *)
  fun copy1_bbox bn ((vrn,ern,brn),g) =
      let
        val bbox_vs = get_bbox_contents g bn;
        val ((vrn',ern'),g') = copy_subgraph bbox_vs ((vrn, ern),g);
        val (new_bn_opt,brn') = B.Rnm.rename1 bn brn;
        val bn' = (case new_bn_opt
                     of NONE => raise ERROR ("copy1_bbox was passed brn that " ^
                                "does not avoid !-box being renamed")
                      | SOME bn' => bn');
        val g'' = new_named_bbox bn' g'
        fun extend_bbs v gr =
          let
            val SOME v' = V.Rnm.lookup vrn' v
            val vs = V.NSet.single v'
            val bbs' = B.NSet.delete bn (get_bboxes_of g v)
            val bbs = B.NSet.add bn' bbs'
          in
            B.NSet.fold (fn b => add_to_bbox b vs) bbs gr
          end
        val g''' = V.NSet.fold extend_bbs bbox_vs g''
      in
         (bn', ((vrn',ern',brn'), g'''))
      end;

  fun fresh_copy1_bbox bn g =
      let val (bname, (_, graph)) = copy1_bbox bn
                ((V.Rnm.mk_from_avoids (get_vnames g),
                E.Rnm.mk_from_avoids (get_enames g),
                B.Rnm.mk_from_avoids (get_bboxes g)),g)
      in (bname, graph) end;

  fun expand1_bbox bn ((vrn,ern,brn),g) =
    (* brn is currently unused, but will be needed for
     * nested !-boxes *)
      let
        val bbox_vs = get_bbox_contents g bn;
        val ((vrn',ern'),g') = copy_subgraph bbox_vs ((vrn, ern),g);
        fun extend_bbs v gr =
          let
            val SOME v' = V.Rnm.lookup vrn' v
            val vs = V.NSet.single v'
            val bbs = B.NSet.delete bn (get_bboxes_of g v)
          in
            B.NSet.fold (fn b => add_to_bbox b vs) bbs gr
          end
        val g'' = V.NSet.fold extend_bbs bbox_vs g'
      in
         ((vrn',ern',brn), g'')
      end;

  fun fresh_expand1_bbox bn g =
      let val (_, graph) = expand1_bbox bn
                ((V.Rnm.mk_from_avoids (get_vnames g),
                E.Rnm.mk_from_avoids (get_enames g),
                B.Rnm.mk_from_avoids (get_bboxes g)),g)
      in graph end;

  (* FIXME: this code is largely duplicated in bgraph - clean up *)

  (* IMPROVE: move these into graph *)
  fun pair_from_of_edge g e =
      let val ((bd,ed),(from,to)) = get_edge g e in (e,ed,from) end;
  fun pair_to_of_edge g e =
      let val ((bd,ed),(from,to)) = get_edge g e in (e,ed,to) end;

  (* auxillary function; adding a vertex to a map of adj banged,
     or a set of others. *)
  fun add_to_others g n (vtobang, others) =
      (case lookup_bbox_of g n of NONE => (vtobang, V.NSet.add n others)
       | SOME bbn => (if VBMap.dom_contains vtobang n then vtobang
                         else VBMap.add_to_dom n bbn vtobang,
                      others));

  (* get arrities to adjacent vertices, return two tables, one for normal edges
     and one for those in a different bang-boxes. Filter by function: when function is true, skip this adj entry. *)
  fun get_filtered_adj_arities_and_bang_arities f g n =
      let
        val nbbox = lookup_bbox_of g n;

        (* interesting case for auto-optimisation *)
        fun in_a_different_bbox n2 =
            (case nbbox of NONE => lookup_bbox_of g n2
             | SOME bbn =>
                (case lookup_bbox_of g n2 of
                    NONE => NONE
                  | SOME bbn2 =>
                    if (not (B.name_eq (bbn, bbn2)))
                    then SOME bbn2 else NONE));

        (* *)
        fun init_adj_info (eh,(UnDirected,ed)) =
            Arity.add_1_undir Arity.zero
          | init_adj_info (eh,(Directed,ed)) =
            if H.is_start eh then Arity.add_1_in Arity.zero
            else Arity.add_1_out Arity.zero;
        (* *)
        fun add_one_to_adj_info (eh,(UnDirected,ed)) a = Arity.add_1_undir a
          | add_one_to_adj_info (eh,(Directed,ed)) a =
            if H.is_start eh then Arity.add_1_in a
            else Arity.add_1_out a;

        fun add_one_arity e n2 ntab =
            if V.NTab.contains_name ntab n2 then
              V.NTab.map_entry (add_one_to_adj_info e) n2 ntab
            else V.NTab.update (n2,init_adj_info e) ntab;

        fun consider1 e n2 (ntab,bboxntab) =
            if f (e,n2) then (ntab,bboxntab) else
            (case in_a_different_bbox n2 of
               NONE => (add_one_arity e n2 ntab, bboxntab)
             | SOME bbn =>
               (ntab,
                if B.NTab.contains_name bboxntab bbn then
                  B.NTab.map_entry (add_one_arity e n2) bbn bboxntab
                else
                  B.NTab.update (bbn, add_one_arity e n2
                                           V.NTab.empty)
                                   bboxntab))
      in
        fold_adj_halfedges consider1 g n (V.NTab.empty, B.NTab.empty)
      end;

  (* get arrities to adjacent vertices, return two tables, one for normal edges
     and one for those in a different bang-boxes. *)
  val get_adj_arities_and_bang_arities =
      get_filtered_adj_arities_and_bang_arities (K false);

  structure BBoxCRnm = CompoundRenamingFun(
  struct
    structure Nm = B
    type obj = T
    val try_rename1 = try_rename1_bbox;
    val get_nameset = get_bboxes
  end);

  (*  *)
  fun rename_incl_bboxes (vrn,ern,brn) g =
      let
        val (vrn2,g') = VertexCRnm.rename vrn g
        val (ern2,g'') = EdgeCRnm.rename ern g'
        val (brn2,g''') = BBoxCRnm.rename brn g''
      in ((vrn2,ern2,brn2),g''') end;

  (* rename g2 apart from g1 *)
  fun rename_apart_incl_bboxes g1 g2 =
      let
        val vrn = V.Rnm.mk_from_avoids (get_vnames g1)
        val ern = E.Rnm.mk_from_avoids (get_enames g1)
        val brn = B.Rnm.mk_from_avoids (get_bboxes g1)
      in rename_incl_bboxes (vrn,ern,brn) g2 end;

  (* override OGraph functions *)
  (* Merge g1 into g2.
     (Replaces basic merge_by_vertices to include bboxes)
     Assumes edge-names are disjoint. Assumes bbox names disjoint.
     Assumes all edge-points from g1 are in g2 already (and copied bboxes skip edge-points)
     When an edge-point in g1 overlaps an internal vertex in g2, if the
     edge-point is in a bangbox, then the bang-box doesn't extend to the
     internal vertex. *)
  fun merge_by_vertices g1 g2 =
      g2 |> OGraph.merge_by_vertices g1
         |> B.NTab.fold
   (* FIXME: this is probably wrong *)
              (fn (bbn,vset) =>
                move_to_bbox bbn
                  (V.NSet.subtract vset (get_edgepoints g1)))
              (get_bbox_table g1);

  (* Asserts that all the vertices in vs have exactly the
   * !-boxes vs *)
  fun check_bboxes g bs vs =
    V.NSet.fold (fn v => fn bs =>
      if B.NSet.eq bs (get_bboxes_of g v) then bs
      else raise ERROR "Inconsistent !-boxes") vs bs
  fun check_wire_bboxes g (w as (s,t,wvs,_)) =
    if is_wire_vertex g s then
      (if is_wire_vertex g t then
         (check_bboxes g (get_bboxes_of g s) wvs; w)
       else
         let
           val bs = get_bboxes_of g s
           val _ = check_bboxes g bs wvs
           val _ = if B.NSet.sub_set (get_bboxes_of g t) bs
                   then () else raise ERROR "Inconsistent !-boxes"
         in w end)
    else if is_wire_vertex g t then
      let
        val bs = get_bboxes_of g t
        val _ = check_bboxes g bs wvs
        val _ = if B.NSet.sub_set (get_bboxes_of g s) bs
                then () else raise ERROR "Inconsistent !-boxes"
      in w end
    else
      case V.NSet.get_local_bot wvs of
           NONE => w
         | SOME wv =>
              let
                val bs = get_bboxes_of g wv
                val _ = check_bboxes g bs wvs
                val _ = if B.NSet.sub_set (get_bboxes_of g s) bs
                        then () else raise ERROR "Inconsistent !-boxes"
                val _ = if B.NSet.sub_set (get_bboxes_of g t) bs
                        then () else raise ERROR "Inconsistent !-boxes"
              in w end

  fun minimise g = let
    val wire_list = get_wire_list g
    (* add a minimised wire where the given wire is *)
    fun add_min_wire (wire as (s,t,wvs,es)) h = let
      (* this data should be the same for all edges in the wire *)
      val _ = check_wire_bboxes h wire
      val (dir_and_data,_) = get_edge h ((the o E.NSet.get_min) es)
      fun add_wire_with_bboxes s t bs g = let
        val (wv,g') = g |> add_vertex OVData.EVert
        val g'' = g' |> doadd_edge dir_and_data s wv
                     |> doadd_edge dir_and_data wv t
        val wvset = V.NSet.single wv
      in
        g'' |> B.NSet.fold (fn b => add_to_bbox b wvset) bs
      end
    in
      if is_wire_vertex h s orelse is_wire_vertex h t then
        h |> doadd_edge dir_and_data s t
      else
        case V.NSet.get_local_bot wvs of
             NONE => h |> doadd_edge dir_and_data s t
           | SOME wv =>
               let
                 val bs = get_bboxes_of h wv
                 val edge_sufficient = B.NSet.is_empty bs orelse
                      (B.NSet.eq bs (B.NSet.union_merge (get_bboxes_of h s)
                                                        (get_bboxes_of h t)))
               in
                 if edge_sufficient
                 then h |> doadd_edge dir_and_data s t
                 else h |> add_wire_with_bboxes s t bs
               end
    end
  (* new wires are added before remove old ones because we need edge data *)
  in fold delete_wire wire_list (fold add_min_wire wire_list g)
  end

  fun normalise g = let
    val wire_list = get_wire_list g
    (* add a normalised wire where the given wire is *)
    fun add_norm_wire (wire as (s,t,wvs,es)) h = let
      val _ = check_wire_bboxes h wire
      (* this data should be the same for all edges in the wire *)
      val (dir_and_data,_) = get_edge h ((the o E.NSet.get_min) es)
    in
      (* add two extra wire verts on internal wires *)
      if is_node_vertex h s andalso is_node_vertex h t then
        let
          val (b1,h) = h |> add_vertex OVData.EVert
          val (b2,h) = h |> add_vertex OVData.EVert
          val bs = case V.NSet.get_local_bot wvs of
                        NONE => B.NSet.union_merge (get_bboxes_of h s)
                                                   (get_bboxes_of h t)
                      | SOME wv => get_bboxes_of h wv
          val wvset = V.NSet.of_list [b1,b2]
        in
          h |> doadd_edge dir_and_data s b1
            |> doadd_edge dir_and_data b1 b2
            |> doadd_edge dir_and_data b2 t
            |> B.NSet.fold (fn b => add_to_bbox b wvset) bs
        end
      (* add no extra wire-verts on boundary wires, bare wires, or circles *)
      else h |> doadd_edge dir_and_data s t
    end
  (* new wires are added before remove old ones because we need edge data *)
  in fold delete_wire wire_list (fold add_norm_wire wire_list g)
  end

end; (* BangGraphFun *)

(* vim:et:sts=2:sw=2
*)
