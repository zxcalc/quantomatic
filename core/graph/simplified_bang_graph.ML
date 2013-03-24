functor BangGraph(structure Param: OGRAPH_PARAM) =
struct

structure Param = Param
structure OVData = Param.OVData
structure EData = Param.EData

(* main bang-graph datatype *)
datatype T = BG of {
  vdata  : OVData.data V.NTab.T, 
  edata  : (dir_or_undir * EData.data) E.NTab.T,
  source : EVMap.T,
  target : EVMap.T,
  
  bboxes   : B.NSet.T,
  bbox_rel : BVBRel.T,
  bbox_parent_map : BMap.T,
  
  param : Param.T
}

type Wire = V.name * V.name * V.NSet.T * E.NSet.T (* a chain of edges, representing a wire *)

(* empty bang graph *)
val empty : T = BG {
  vdata  = V.NTab.empty,
  edata  = E.NTab.empty,
  source = EVMap.empty,
  target = EVMap.empty,
  
  bboxes   = B.NSet.empty,
  bbox_rel = BVBRel.empty,
  bbox_parent_map = BMap.empty,

  param = Param.empty
}

(*********************************)
(********** EXCEPTIONS ***********)
(*********************************)

exception bug_exp of string

exception no_such_xname_exp of string * X.name * T
exception xname_already_exists_exp of X.name * T

exception no_such_vname_exp of string * V.name * T
exception no_such_vnames_exp of string * V.NSet.T * T
exception vname_already_exists_exp of V.name * T

exception no_such_ename_exp of string * E.name * T
exception ename_already_exists_exp of E.name * T

exception not_an_endpoint_exp of string * E.name * V.name

exception wire_vertex_exp of string * T * V.name
exception wire_exp of string * T * Wire

(*********************************)
(*********** ACCESSORS ***********)
(*********************************)

(* these may be automatically regenerated, so avoid making manual changes *)

fun update_vdata f (BG r) = BG {vdata=f(#vdata r),edata= #edata r,source= #source r,target= #target r,bboxes= #bboxes r,bbox_rel= #bbox_rel r,bbox_parent_map= #bbox_parent_map r,param= #param r}
fun update_edata f (BG r) = BG {vdata= #vdata r,edata=f(#edata r),source= #source r,target= #target r,bboxes= #bboxes r,bbox_rel= #bbox_rel r,bbox_parent_map= #bbox_parent_map r,param= #param r}
fun update_source f (BG r) = BG {vdata= #vdata r,edata= #edata r,source=f(#source r),target= #target r,bboxes= #bboxes r,bbox_rel= #bbox_rel r,bbox_parent_map= #bbox_parent_map r,param= #param r}
fun update_target f (BG r) = BG {vdata= #vdata r,edata= #edata r,source= #source r,target=f(#target r),bboxes= #bboxes r,bbox_rel= #bbox_rel r,bbox_parent_map= #bbox_parent_map r,param= #param r}
fun update_bboxes f (BG r) = BG {vdata= #vdata r,edata= #edata r,source= #source r,target= #target r,bboxes=f(#bboxes r),bbox_rel= #bbox_rel r,bbox_parent_map= #bbox_parent_map r,param= #param r}
fun update_bbox_rel f (BG r) = BG {vdata= #vdata r,edata= #edata r,source= #source r,target= #target r,bboxes= #bboxes r,bbox_rel=f(#bbox_rel r),bbox_parent_map= #bbox_parent_map r,param= #param r}
fun update_bbox_parent_map f (BG r) = BG {vdata= #vdata r,edata= #edata r,source= #source r,target= #target r,bboxes= #bboxes r,bbox_rel= #bbox_rel r,bbox_parent_map=f(#bbox_parent_map r),param= #param r}
fun update_param f (BG r) = BG {vdata= #vdata r,edata= #edata r,source= #source r,target= #target r,bboxes= #bboxes r,bbox_rel= #bbox_rel r,bbox_parent_map= #bbox_parent_map r,param=f(#param r)}

fun get_vdata           (BG r) = #vdata r
fun get_edata           (BG r) = #edata r
fun get_source          (BG r) = #source r
fun get_target          (BG r) = #target r
fun get_bboxes          (BG r) = #bboxes r
fun get_bbox_rel        (BG r) = #bbox_rel r
fun get_bbox_parent_map (BG r) = #bbox_parent_map r
fun get_param           (BG r) = #param r

val set_vdata              = update_vdata           o K
val set_edata              = update_edata           o K
val set_source             = update_source          o K
val set_target             = update_target          o K
val set_bboxes             = update_bboxes          o K
val set_bbox_rel           = update_bbox_rel        o K
val set_bbox_parent_map    = update_bbox_parent_map o K
val set_param              = update_param           o K


(*********************************)
(******* DERIVED ACCESSORS *******)
(*********************************)

fun get_vertices g = V.NTab.get_nameset (get_vdata g)
fun get_edges g = E.NTab.get_nameset (get_edata g)

(* getting stuff from graphs (exception if not there) *)
fun get_edge_source g = EVMap.domf (get_source g)
fun get_edge_target g = EVMap.domf (get_target g)


(* given an edge and vertex, get the other end *)
fun edge_get_other_vertex g e v1 = let
  val (s,t) = (get_edge_source g e, get_edge_target g e)
in if V.name_eq (v1, s) then t
   else if V.name_eq(v1, t) then s
   else raise not_an_endpoint_exp ("edge_get_other_vertex",e,v1)
end

(* note this ignore direction *)
fun get_in_edges g = EVMap.codf (get_target g)
fun get_out_edges g = EVMap.codf (get_source g)

fun get_adj_edges g v =
  E.NSet.union_merge (EVMap.codf (get_source g) v)
                     (EVMap.codf (get_target g) v)

fun get_vertex_data g = V.NTab.get (get_vdata g)
fun get_edge_dir_and_data g = E.NTab.get (get_edata g)
fun get_edge_dir g e  = #1 (get_edge_dir_and_data g e)
fun get_edge_data g e = #2 (get_edge_dir_and_data g e)

fun set_vertex_data d v = update_vdata (V.NTab.update (v,d))
fun set_edge_data d e = update_edata (E.NTab.map_entry (fn (dir,_) => (dir,d)) e)

fun get_vertices_in_bbox g = BVBRel.domf (get_bbox_rel g)
fun get_bboxes_containing_vertex g = BVBRel.codf (get_bbox_rel g)
fun get_bbox_parent_opt g = BMap.lookup_domf (get_bbox_parent_map g)

fun contains_vertex g = V.NSet.contains (get_vertices g)
fun contains_edge g   = E.NSet.contains (get_edges g)

fun add_named_vertex v d g =
  if (contains_vertex g v) then raise vname_already_exists_exp (v,g)
  else g |> update_vdata (V.NTab.update (v,d))
  
fun add_vertex d g = let
  val fresh_name = V.NSet.new (get_vertices g) V.default_name
in (fresh_name, add_named_vertex fresh_name d g)
end

fun doadd_vertex d g = snd (add_vertex d g)


fun add_named_edge e (dir,d) s t g =
  if (contains_edge g e) then raise ename_already_exists_exp (e,g)
  else
    let
      val missing = V.NSet.filter (fn v => not (contains_vertex g v)) (V.NSet.of_list [s,t])
    in if V.NSet.is_empty missing then
         g |> update_edata (E.NTab.update (e,(dir,d)))
           |> update_source (EVMap.add_to_dom e s)
           |> update_target (EVMap.add_to_dom e t)
       else raise no_such_vnames_exp ("add_named_edge", missing, g)
    end

fun add_edge dd s t g = let
  val fresh_name = E.NSet.new (get_edges g) E.default_name
in (fresh_name, add_named_edge fresh_name dd s t g)
end
  
fun doadd_edge dd s t g = snd (add_edge dd s t g)

fun delete_edge e g =
  g |> update_edata (E.NTab.delete e)
    |> update_source (EVMap.surj_del_dom e)
    |> update_target (EVMap.surj_del_dom e)
    
fun delete_vertex v g =
  g |> update_vdata (V.NTab.delete v)
    |> E.NSet.fold delete_edge (get_adj_edges g v)
    |> update_bbox_rel (BVBRel.del_cod v)


fun swap_edge_dir e g = let
  val (s,t) = (get_edge_source g e, get_edge_target g e)
  val dd = (get_edge_dir_and_data g e)
in g |> delete_edge e |> add_named_edge e dd t s
end


(************************************)
(******* DEPRECATED ACCESSORS *******)
(************************************)


(*********************************)
(******* GLOBAL PROPERTIES *******)
(*********************************)

(* a graph is empty if it has no vertices or bboxes *)
fun is_empty g = V.NTab.is_element_empty (get_vdata g) andalso B.NSet.is_empty (get_bboxes g)

(* TODO: rename to 'eq' taking uncurried arguments *)
fun exact_eq g1 g2 =
let 
  val e1s = get_edges g1
  val e2s = get_edges g2
  val v1s = get_vertices g1
  val v2s = get_vertices g2
  val bb1s = get_bboxes g1
  val bb2s = get_bboxes g2
in
  (* check same vertex names *)
  (V.NSet.eq v1s v2s) andalso
  (* check same edge names *)
  (E.NSet.eq e1s e2s) andalso
  (* check same bbox names *)
  (B.NSet.eq bb1s bb2s) andalso
  (* check same vertex data TODO: same bboxes *)
  (V.NTab.forall (fn (n,d1) => OVData.data_eq (d1, get_vertex_data g2 n)) (get_vdata g1)) andalso 
  (* check same edge data, and source and target maps agree *)
  (E.NTab.forall (fn (n,(dir1,d1)) => 
    let
      val (dir2, d2) = get_edge_dir_and_data g2 n
    in dir1 = dir2 andalso
       EData.data_eq (d1, d2) andalso
       V.name_eq (get_edge_source g1 n, get_edge_source g2 n) andalso
       V.name_eq (get_edge_target g1 n, get_edge_target g2 n)
    end) (get_edata g1)) andalso
  (* check bbox containment and parents agree *)
  (B.NSet.forall (fn bb =>
    V.NSet.eq (get_vertices_in_bbox g1 bb) (get_vertices_in_bbox g2 bb) andalso
    (case (get_bbox_parent_opt g1 bb, get_bbox_parent_opt g2 bb)
       of (SOME p1, SOME p2) => B.name_eq (p1,p2)
        | (NONE,NONE) => true
        | _ => false)
  ) (get_bboxes g1))
end


end
