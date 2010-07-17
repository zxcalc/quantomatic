(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Ross' Quantum Graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graphs with Boundary *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 

signature BANG_GRAPH_SHARING = 
sig
include OGRAPH_SHARING
structure BBox : NAMES_SHARING
structure VtoBoxMap : NAME_MAP_SHARING
  sharing VtoBoxMap.Cod = BBox;
  sharing VtoBoxMap.Dom = Vertex;
end;


(* bang-box graphs *)
signature BASIC_BANG_GRAPH = 
sig
include BASIC_OGRAPH;

exception merge_bbox_exp of string 
exception overlapping_bbox_exp of Vertex.NSet.T * T

structure BBox : SSTR_NAMES; (* !(Bang)-Box names/tables/sets *)
structure VtoBoxMap : NAME_MAP (* !-box-name to vertices *)

structure SharingBangGraph : BANG_GRAPH_SHARING
sharing SharingOGraph = SharingBangGraph
sharing SharingBangGraph.BBox = BBox.Sharing
sharing SharingBangGraph.VtoBoxMap = VtoBoxMap.Sharing

val get_bboxes : T -> VtoBoxMap.T
val get_bbox : T -> BBox.name -> Vertex.NSet.T
val get_bbox_list : T -> BBox.name list
val get_unboxed : T -> Vertex.NSet.T
val get_bnames : T -> BBox.NSet.T

val lookup_box_of : T -> Vertex.name -> BBox.name option
val is_boxed : T -> Vertex.name -> bool

val new_box : BBox.name -> T -> BBox.name * T
val move_to_box : BBox.name -> Vertex.NSet.T -> T -> T
val move_to_unbox : Vertex.NSet.T -> T -> T

end;



(* bang-box graphs *)
signature BANG_GRAPH = 
sig
include OGRAPH;

val copy1_box : (Vertex.NSet.T * Edge.NSet.T * BBox.NSet.T) (* avoid names *) 
      -> BBox.name -> T (* bbox to copy and graph containing it *) 
      -> ((BBox.name * T) (* new box name and subgraph in bbox *)
          * (Vertex.renaming (* how vnames were copied *)
              * Edge.renaming (* ename copying *)
              * BBox.NSet.T)) (* new bbox nameset *)
         * T (* new graph *)

val copy1_box_given_rnm : 
         BBox.name (* bbox being copied *)
      -> (BBox.name * (Vertex.Rnm.T * Edge.Rnm.T)) (* use these renamings *) 
      -> T (* graph containing bbox *) 
      -> T (* new graph *)

val copy1_box_given_ext_rnm : 
         BBox.name (* bbox being copied *)
      -> BBox.name (* new name copied to *) 
      -> (Vertex.Rnm.T * Edge.Rnm.T) * T (* external vs renaming,graph bbox *) 
      -> (Vertex.Rnm.T * Edge.Rnm.T) * T (* new renaming, new graph *)

val kill_box : BBox.name -> T -> T
val drop_box : BBox.name -> T -> T
val merge_boxes : BBox.NSet.T -> T -> BBox.name * T


(*  
val get_adj_banged_vnames : 
    T -> Vertex.name -> VtoBoxMap.T
*)

(* *)
val get_adj_arities : 
    T -> Vertex.name -> int Vertex.NTab.T * int Vertex.NTab.T BBox.NTab.T

(* adjacent vertex info, w.r.t. tgt and pattern for matching *)
val adj_tgt_info_bangs : 
    T -> Vertex.name 
    -> Vertex.data 
       * (Edge.data Edge.NTab.T (* names/data of edges that are self-loops *)
          * (VtoBoxMap.T (* bang box to contained adj vertices *)
             * Vertex.NSet.T)) (* other adj vertices *)

(* like tgt, but includes separation of boundary vertices *)
val adj_pat_info_bangs :
    T -> Vertex.name 
    -> Vertex.data 
       * ((Edge.Ehn.name * Edge.data)
            Vertex.NTab.T (* leaf vertex to source's half-edge *)
          * Edge.data Edge.NTab.T (* names of edges that are self-loops *)
          * (VtoBoxMap.T  (* bang box to contained adj vertices *)
             * Vertex.NSet.T)) (* other adj vertices *)

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* A  Graph With Boundary *)
functor BangGraphFun(
        structure OVertex : OVERTEX
        structure Edge : EDGE
        type data 
        val empty_data : data
) : BANG_GRAPH
= struct

structure V = OVertex;
structure E = Edge;

(* bang-box names *)
structure BBox : SSTR_NAMES = 
struct 
open SStrName;
val default_name = mk "Ba";
end;

(* structure BBox : SSTR_NAMES = BBox; *)
structure VtoBoxMap = NameMapFun(structure Dom = V and Cod = BBox);

(* to avoid using clobbered data and empty graph from the opened Basic Graph *)
type data2 = data;
val empty_data2 = empty_data;

(* we add bbox to Graph data, this is hidden *)
datatype hdata = 
         HData of 
         { bboxes : VtoBoxMap.T,
           unboxed : V.NSet.T,
           data : data
         };
fun get_unboxed_of_hdata (HData rep) = #unboxed rep;
fun get_bboxes_of_hdata (HData rep) = #bboxes rep;
fun get_data_of_hdata (HData rep) = #data rep;
fun update_bboxes_of_hdata f (HData rep) =
    HData { data = #data rep,
            bboxes = f (#bboxes rep),
            unboxed = #unboxed rep
          };
fun update_data_of_hdata f (HData rep) =
    HData { data = f (#data rep),
            bboxes = #bboxes rep,
            unboxed = #unboxed rep
          };
fun update_unboxed_of_hdata f (HData rep) =
    HData { data = #data rep,
            bboxes = #bboxes rep,
            unboxed = f (#unboxed rep)
          };
val empty_hdata = HData {bboxes = VtoBoxMap.empty,
                   unboxed = V.NSet.empty,
                   data = empty_data};


structure BasicBBGraph =
struct 

  (* make basic graph with this as internal data *)
  structure Graph : OGRAPH = BasicOGraphFun(
    structure OVertex = V 
    and Edge = E
    type data = hdata
    val empty_data = empty_hdata
  );
  (* inherit functions from Graph *)
  open Graph;
  
  (* clobber sub-graph data *)
  type data = data2;
  val empty_data = empty_data2;
  val get_data = get_data_of_hdata o Graph.get_data;
  val update_data = Graph.update_data o update_data_of_hdata;

  (* for bbox part of added data *)
  val get_unboxed = get_unboxed_of_hdata o Graph.get_data;
  val get_bboxes : T -> VtoBoxMap.T = get_bboxes_of_hdata o Graph.get_data;
  val get_bbox_list = VtoBoxMap.get_codlist o get_bboxes;
  val update_unboxed = Graph.update_data o update_unboxed_of_hdata;
  val update_bboxes = Graph.update_data o update_bboxes_of_hdata;
  val set_unboxed = update_unboxed o K;
  val add_to_unboxed = update_unboxed o V.NSet.add;
  val del_from_unboxed = update_unboxed o V.NSet.delete;
  val set_bboxes : VtoBoxMap.T -> T -> T = update_bboxes o K;
  val get_bbox = BBox.NTab.get o VtoBoxMap.get_codtab o get_bboxes;

  (* modified/new basic graph functions *)
  fun add_named_vertex n v g =
      let val (n2,g2) = Graph.add_named_vertex n v g
      in (n2, add_to_unboxed n2 g2) end;

  (* deleting vertex also removes all it's edges. *)
  fun delete_vertex n g = 
      g |> del_from_unboxed n
        |> Graph.delete_vertex n;

  (* IMPROVE: change BBox.NTab.T to a namemap and do direct renaming:
     this will avoid looking for the name in sets in which it does not
     occur
     IMPROVE: use renaming functor 
   *)
  fun rename_vname n1 n2 g = 
      g |> Graph.rename_vname n1 n2
        |> update_bboxes (fn bxs => 
                             case VtoBoxMap.try_rename1_dom n1 n2 bxs 
                              of NONE => bxs
                               | SOME bxs2 => bxs2)
        |> update_unboxed (fn unbxs => 
                              case V.NSet.try_rename1 n1 n2 unbxs 
                               of NONE => unbxs
                                | SOME unbxs2 => unbxs2);

  (* prettifying *)
  fun pretty g = 
      Pretty.chunks 
        [ Pretty.str "BangBoxGraph{",
          Pretty.block 
            [Pretty.str "  ", (* indent *)
             Pretty.chunks 
               ((if VtoBoxMap.is_empty (get_bboxes g) then []
                 else [Pretty.block 
                         [Pretty.str "Bang-Boxes: ", 
                          VtoBoxMap.pretty (get_bboxes g)]])
                @ [Graph.pretty g])],
          Pretty.str "}"
        ];

  val print = Pretty.writeln o pretty;
end; (* BasicBGraph *)

open BasicBBGraph;

(* FIXME: pretty horrible way to overload extended functions: rethink this *)
structure BBGraphTools = GraphToolsFun(BasicBBGraph);
open BBGraphTools;


(* FIXME!!!: should be able to do this by inherrited delete function; 
   this is HORRIBLE: copied from ograph *)




exception overlapping_bbox_exp of V.NSet.T * T

(* lookup name of box that v is in, NONE if unboxed *)
fun lookup_box_of g v = VtoBoxMap.lookup_domf (get_bboxes g) v;
fun is_boxed g v = V.NSet.contains (VtoBoxMap.get_domset (get_bboxes g)) v;

(* all !-box names *)
val get_bnames = VtoBoxMap.get_codset o get_bboxes;

(* make a new empty !-box *)
fun new_box bn g = 
    let
      val bboxes = (get_bboxes g);
      val bn2 = BBox.NSet.new (VtoBoxMap.get_codset bboxes) bn;
      val bboxes2 : VtoBoxMap.T = 
          VtoBoxMap.add_to_cod bn2 V.NSet.empty bboxes;
    in (bn2, g |> set_bboxes bboxes2) end;

(* add to a !-box; makes new box, if name doesn't alreayd exist;
IMPROVE: raise exception if box doesn't already exist? *)
fun move_to_box bn vs g = 
    g |> update_bboxes (VtoBoxMap.add_to_cod bn vs 
                        o Vertex.NSet.fold VtoBoxMap.del_dom vs)
      |> update_unboxed (Vertex.NSet.remove_set vs);

fun move_to_unbox vs g = 
    g |> update_bboxes (Vertex.NSet.fold VtoBoxMap.del_dom vs)
      |> update_unboxed (Vertex.NSet.union_merge vs);

(* FIXME: merge_by_vertices respect !-boxes *)
(* FIXME: subgraph_of_selection respect !-boxes *)
(* FIXME: other stuff respect !-boxes ??? *)

(* copy bang box, making copy having names fresh w.r.t. given v set (vs) 
   and e set (es);
   assumes: G.get_Xnames g <= Xs; for X = {e, v, b}
*)
fun copy1_box (vs,es,bs) bn g = 
    let val bbox_vs = get_bbox g bn
        (* subgraph of selection gives back something without bang-boxes *)
        val (boundary_vs, bbg) = cut_subgraph_of_selection' bbox_vs g;
        val ((vrn,ern),bbg2) = 
            bbg |> rename (Vertex.Rnm.mk boundary_vs vs Vertex.NTab.empty,
                           Edge.Rnm.mk_from_avoidset es) 
        val bbox2_vs = Vertex.Rnm.get_newnames vrn;
        val bboxes = get_bboxes g
        val (bn2, bs2) = BBox.NSet.add_new bn bs;
        val bboxes2 = VtoBoxMap.add_to_cod bn2 bbox2_vs bboxes;
    in
      (((bn2,bbg2),(vrn,ern,bs2)), 
       g |> merge_by_vertices bbg2
         |> set_bboxes bboxes2)
    end;

(* given a new bang-box name and the renamings for its vertices and edges, copy 
   the bangbox, renamed as specified. *)
fun copy1_box_given_rnm bn (bn2,(vrn,ern)) g = 
    let val bbox_vs = get_bbox g bn
        (* subgraph of selection gives back something without bang-boxes *)
        val (boundary_vs, bbg) = cut_subgraph_of_selection' bbox_vs g;
        val bbg2 = bbg |> VertexRnm.do_renaming vrn
                       |> EdgeRnm.do_renaming ern
        val bbg_internal_vs = 
            Vertex.NSet.subtract (get_vnames bbg2) boundary_vs;
    in
      g |> merge_by_vertices bbg2
        |> update_bboxes 
             (VtoBoxMap.add_to_cod bn2 bbg_internal_vs)
    end;


(* given a renaming of external/boundary nodes, which may for example be shared with the lhs of a rewrite rule (although the ignores are not shared - these are reset each call to be the interface to the copied graph component in the !-box. Copies the bang box using the specified vertex renaming for external/boundary nodes within the bbox. also assumes that bn2 must be fresh from bbox names in g. *)
fun copy1_box_given_ext_rnm bn1 bn2 ((vrn,ern),g) = 
    let val bbox_vs = get_bbox g bn1
        (* subgraph of selection gives back something without bang-boxes *)
        val (boundary_vs, bbg) = cut_subgraph_of_selection' bbox_vs g;
        val (vrn2,bbg2) = VertexRnm.rename 
                            (Vertex.Rnm.set_ignored boundary_vs vrn) bbg;
        val (ern2,bbg3) = EdgeRnm.rename ern bbg2;
        val bbg_internal_vs = 
            Vertex.NSet.subtract (get_vnames bbg3) boundary_vs;
    in
      ((vrn2,ern2),
       g |> merge_by_vertices bbg3
         |> update_bboxes 
              (VtoBoxMap.add_to_cod bn2 bbg_internal_vs))
    end;


fun kill_box bn g = 
    let val bbox_vs = get_bbox g bn
    in g |> Vertex.NSet.fold delete_vertex bbox_vs
         |> update_bboxes (VtoBoxMap.del_cod bn)
    end;

fun drop_box bn g = 
    let val bbox_vs = get_bbox g bn 
    in g |> update_bboxes (VtoBoxMap.del_cod bn)
         |> update_unboxed (Vertex.NSet.union_merge bbox_vs)
    end;


(* *)
exception merge_bbox_exp of string 

(* returns bbox-name merged all boxes into and new merged graph *)
fun merge_boxes bnset g = 
    (case BBox.NSet.pull_local_top bnset of 
       NONE => raise merge_bbox_exp "given empty bbox name set"
     | SOME (bn,bnset2) => 
       (bn, BBox.NSet.fold 
            (fn bn2 => fn g2 =>
                 g2 |> move_to_box bn (get_bbox g2 bn2)
                    |> update_bboxes (VtoBoxMap.del_cod bn2))
            bnset2 g));


(* FIXME: this code is largely duplicated in bgraph - clean up *)

(* IMPROVE: move these into graph *)
fun pair_from_of_edge g e = 
    let val ((bd,ed),(from,to)) = get_edge g e in (e,ed,from) end;
fun pair_to_of_edge g e = 
    let val ((bd,ed),(from,to)) = get_edge g e in (e,ed,to) end;

(* auxillary function; adding a vertex to a map of adj banged, 
   or a set of others. *)
fun add_to_others g n (vtobang, others) = 
    (case lookup_box_of g n of NONE => (vtobang, V.NSet.add n others)
     | SOME bbn => (if VtoBoxMap.dom_contains vtobang n then vtobang 
                       else VtoBoxMap.add_to_dom n bbn vtobang, 
                    others));

(* find all adjacent vertices, except self, divided into: 
   boundary leafs, bangbox vertices, and others *)
fun adj_tgt_info_bangs' g n0 (d,(ins,outs)) = 
    let 
      fun consider_edge (e,ed,n) (loopes, others) = 
          if V.name_eq (n,n0) then (E.NTab.update (e, ed) loopes, others)
          else (loopes, add_to_others g n others)
    in 
      (d, (E.NTab.empty, (VtoBoxMap.empty, V.NSet.empty))
            |> (Edge.NSet.fold (consider_edge o (pair_from_of_edge g)) ins)
            |> (Edge.NSet.fold (consider_edge o (pair_to_of_edge g)) outs))
    end;

fun adj_tgt_info_bangs g n =  adj_tgt_info_bangs' g n (get_vertex g n);

(* find all adjacent vertices, except self, divided into: 
   boundary leafs, and others *)
fun adj_pat_info_bangs' g n0 (d,(ins,outs)) = 
    let 
      fun consider_edge eh (leaftab, loopes, others) = 
          let val n = get_ehn_vname g (Edge.Ehn.other eh) 
              val en = Edge.Ehn.get_ename eh in
          if V.name_eq (n,n0) then (* self-loop *)
            (leaftab, E.NTab.update (en,get_edge_data g en) loopes, others)
          else if V.NTab.contains leaftab n then 
            (V.NTab.delete n leaftab, loopes, add_to_others g n others)
          else if is_boundary g n then 
            (V.NTab.ins (n,(eh,get_edge_data g en)) leaftab, loopes, others)
          else (leaftab, loopes, add_to_others g n others)
          end;
    in 
      (d, (V.NTab.empty, E.NTab.empty, (VtoBoxMap.empty, V.NSet.empty))
            |> (Edge.NSet.fold (consider_edge o Edge.Ehn.mk_end) ins)
            |> (Edge.NSet.fold (consider_edge o Edge.Ehn.mk_start) outs))
    end;

fun adj_pat_info_bangs g n = adj_pat_info_bangs' g n (get_vertex g n);

(* get arrities to adjacent vertices, return two tables, one for normal edges 
   and one for those in a different bang-boxes. *)
fun get_adj_arities g n = 
    let 
      val nbbox = lookup_box_of g n;

      (* interesting case for auto-optimisation *)
      fun in_a_different_bbox n2 = 
          (case nbbox of NONE => lookup_box_of g n2
           | SOME bbn => 
              (case lookup_box_of g n2 of 
                  NONE => NONE
                | SOME bbn2 => 
                  if (not (BBox.name_eq (bbn, bbn2))) 
                  then SOME bbn2 else NONE));

      fun add_one_arity n2 ntab =
          (* no self loops *)
          (* if Vertex.name_eq (n,n2) then ntab 
          else *)
            if Vertex.NTab.contains_name ntab n2 then
              Vertex.NTab.map_entry (fn i => i + 1) n2 ntab
            else Vertex.NTab.update (n2,1) ntab;

      fun consider1 e n2 (ntab,bboxntab) = 
          (case in_a_different_bbox n2 of 
             NONE => (add_one_arity n2 ntab, bboxntab)
           | SOME bbn =>  
             (ntab,
              if BBox.NTab.contains_name bboxntab bbn then
                BBox.NTab.map_entry (add_one_arity n2) bbn bboxntab
              else 
                BBox.NTab.update (bbn, Vertex.NTab.update (n2,1) 
                                       Vertex.NTab.empty) 
                                 bboxntab))
    in 
      fold_adj consider1 g n (Vertex.NTab.empty, BBox.NTab.empty)
    end;

(* *)
structure SharingBangGraph 
= struct
  open SharingOGraph;
  structure BBox = BBox.Sharing;
  structure VtoBoxMap = VtoBoxMap.Sharing;
end;

end; (* BangGraphFun *)
