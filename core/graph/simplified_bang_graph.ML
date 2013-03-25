functor BangGraph(structure Param: OGRAPH_PARAM) : OGRAPH =
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

exception bad_graph_merge_exp of T * T * T

exception no_such_ename_exp of string * E.name * T
exception ename_already_exists_exp of E.name * T

exception no_such_bbname_exp of string * B.name * T
exception bbname_already_exists_exp of B.name * T

exception not_an_endpoint_exp of string * E.name * V.name

exception wire_vertex_exp of string * T * V.name
exception wire_exp of string * T * Wire

(*********************************)
(******** BASIC ACCESSORS ********)
(*********************************)

(* Note that it is possible to break a graph by calling these updaters, so they should
 * all be private. Also note that these functions are automatically generated, so avoid
 * making changes *)

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

(*
val set_vdata              = update_vdata           o K
val set_edata              = update_edata           o K
val set_source             = update_source          o K
val set_target             = update_target          o K
val set_bboxes             = update_bboxes          o K
val set_bbox_rel           = update_bbox_rel        o K
val set_bbox_parent_map    = update_bbox_parent_map o K
val set_param              = update_param           o K
*)


(*********************************)
(******* DERIVED ACCESSORS *******)
(*********************************)

(* any graph updaters below should produce valid !-graphs (or fail with
 * exception/NONE) *)

(* note a graph is empty iff it has no vertices or bboxes *)
fun is_empty g = V.NTab.is_element_empty (get_vdata g) andalso B.NSet.is_empty (get_bboxes g)

fun get_vertices g = V.NTab.get_nameset (get_vdata g)
fun get_edges g = E.NTab.get_nameset (get_edata g)


val num_vertices = V.NSet.cardinality o get_vertices
val num_edges = E.NSet.cardinality o get_edges
val num_bboxes = B.NSet.cardinality o get_bboxes


fun get_edge_source g = EVMap.domf (get_source g)
fun get_edge_target g = EVMap.domf (get_target g)

fun get_vertex_data g = V.NTab.get (get_vdata g)
fun get_edge_dir_and_data g = E.NTab.get (get_edata g)
fun get_edge_dir g e  = #1 (get_edge_dir_and_data g e)
fun get_edge_data g e = #2 (get_edge_dir_and_data g e)
fun is_dir_edge g e = (get_edge_dir g e) = Directed

fun set_vertex_data d v = update_vdata (V.NTab.update (v,d))
fun set_edge_data d e = update_edata (E.NTab.map_entry (fn (dir,_) => (dir,d)) e)


(* given an edge and vertex, get the other end *)
fun edge_get_other_vertex g e v1 = let
  val (s,t) = (get_edge_source g e, get_edge_target g e)
in if V.name_eq (v1, s) then t
   else if V.name_eq(v1, t) then s
   else raise not_an_endpoint_exp ("edge_get_other_vertex",e,v1)
end

(* Convenience functions for adjacent edges *)

(* note these ignore direction *)
fun get_in_edges g = EVMap.codf (get_target g)
fun get_out_edges g = EVMap.codf (get_source g)

fun get_adj_edges g v =
  E.NSet.union_merge (EVMap.codf (get_source g) v)
                     (EVMap.codf (get_target g) v)

fun get_in_dir_edges g = E.NSet.filter (fn e => get_edge_dir g e = Directed) o get_in_edges g
fun get_out_dir_edges g = E.NSet.filter (fn e => get_edge_dir g e = Directed) o get_out_edges g
fun get_adj_undir_edges g = E.NSet.filter (fn e => get_edge_dir g e = Undirected) o get_adj_edges g

fun is_self_loop g e = V.name_eq (get_edge_source g e, get_edge_target g e)
fun get_self_loops g v = E.NSet.filter (is_self_loop g) (get_adj_edges g v)

fun get_dir_self_loops g v =
  E.NSet.filter (fn e => is_self_loop g e andalso is_dir_edge g e)
                (get_adj_edges g v)
fun get_undir_self_loops g v =
  E.NSet.filter (fn e => is_self_loop g e andalso not (is_dir_edge g e))
                (get_adj_edges g v)


(* set the arity. note that (undir) self-loops are counted twice *)
fun get_arity g v = let
  fun card f = E.NSet.cardinality (f g v)
in Arity.zero |> Arity.set_in (card get_in_edges)
              |> Arity.set_out (card get_out_edges)
              |> Arity.set_undir (card get_adj_undir_edges + card get_adj_undir_edges)
end

fun adj_edge_classes g v = (
  get_in_dir_edges g v,
  get_out_dir_edges g v,
  get_adj_undir_edges g v
)

fun get_adj_vertices g v = E.NSet.fold
  (fn e => let val v1 = edge_get_other_vertex g e v
           in if V.name_eq(v,v1) then I else V.NSet.add v1
           end) (get_adj_edges g v) V.NSet.empty

fun get_vertices_in_bbox g = BVBRel.domf (get_bbox_rel g)
fun get_bboxes_containing_vertex g = BVBRel.codf (get_bbox_rel g)
fun get_bbox_parent_opt g = BMap.lookup_domf (get_bbox_parent_map g)

fun contains_vertex g = V.NSet.contains (get_vertices g)
fun contains_edge g   = E.NSet.contains (get_edges g)
fun contains_bbox g   = B.NSet.contains (get_bboxes g)

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

fun add_named_vertex v d g =
  if (contains_vertex g v) then raise vname_already_exists_exp (v,g)
  else g |> update_vdata (V.NTab.update (v,d))
  
fun add_vertex d g = let
  val fresh_name = V.NSet.new (get_vertices g) V.default_name
in (fresh_name, add_named_vertex fresh_name d g)
end

fun doadd_vertex d g = snd (add_vertex d g)
    
fun delete_vertex v g =
  g |> update_vdata (V.NTab.delete v)
    |> E.NSet.fold delete_edge (get_adj_edges g v)
    |> update_bbox_rel (BVBRel.del_cod v)

fun add_named_bbox bb g =
  if (contains_bbox g bb) then raise bbname_already_exists_exp (bb,g)
  else g |> update_bboxes (B.NSet.add bb)

fun add_bbox g = let
  val fresh_name = B.NSet.new (get_bboxes g) B.default_name
in (fresh_name, add_named_bbox fresh_name g)
end
  
fun doadd_bbox g = snd (add_bbox g)

fun delete_bbox bb g =
  g |> update_bboxes (B.NSet.delete bb)
    |> update_bbox_rel (BVBRel.del_dom bb)
    |> update_bbox_parent_map (BMap.surj_del_dom bb)
    |> update_bbox_parent_map (BMap.del_cod bb)

fun add_vertices_to_bbox vset bb g =
  if (contains_bbox g bb) then g |> update_bbox_rel (BVBRel.add_to_dom bb vset)
  else raise no_such_bbname_exp ("add_vertices_to_bbox", bb, g)
  
fun add_vertex_to_bbox v = add_vertices_to_bbox (V.NSet.single v)

fun remove_vertices_from_bbox vset bb g =
  if (contains_bbox g bb) then g |> update_bbox_rel (BVBRel.del_from_dom bb vset)
  else raise no_such_bbname_exp ("remove_vertices_from_bbox", bb, g)
  
fun remove_vertex_from_bbox v = remove_vertices_from_bbox (V.NSet.single v)

fun set_bbox_parent_opt bb bbp_opt g =
  if (contains_bbox g bb) then (case bbp_opt
      of SOME bbp =>
        if (contains_bbox g bbp) then g |> update_bbox_parent_map (BMap.add_to_dom bb bbp)
        else raise no_such_bbname_exp ("set_bbox_parent_opt", bbp, g)
       | NONE => g |> update_bbox_parent_map (BMap.surj_del_dom bb))
  else raise no_such_bbname_exp ("set_bbox_parent_opt", bb, g)


fun swap_edge_dir e g = let
  val (s,t) = (get_edge_source g e, get_edge_target g e)
  val dd = (get_edge_dir_and_data g e)
in g |> delete_edge e |> add_named_edge e dd t s
end


(*************************************)
(*** OPERATIONS ON MULTIPLE GRAPHS ***)
(*************************************)

fun is_subgraph g1 g2 =
let 
  val e1s = get_edges g1
  val e2s = get_edges g2
  val v1s = get_vertices g1
  val v2s = get_vertices g2
  val bb1s = get_bboxes g1
  val bb2s = get_bboxes g2
in
  (* check vertex names *)
  (V.NSet.sub_set v1s v2s) andalso
  (* check edge names *)
  (E.NSet.sub_set e1s e2s) andalso
  (* check bbox names *)
  (B.NSet.sub_set bb1s bb2s) andalso
  (* check same vertex data *)
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
    V.NSet.sub_set (get_vertices_in_bbox g1 bb) (get_vertices_in_bbox g2 bb) andalso
    (case (get_bbox_parent_opt g1 bb, get_bbox_parent_opt g2 bb)
       of (SOME p1, SOME p2) => B.name_eq (p1,p2)
        | (NONE,NONE) => true
        | _ => false)
  ) (get_bboxes g1))
end

(* should be equivalent to (is_subgraph g1 g2 andalso is_subgraph g2 g1) *)
fun exact_eq g1 g2 =
  (num_vertices g1 = num_vertices g2) andalso
  (num_edges g1 = num_edges g2) andalso
  (num_bboxes g1 = num_bboxes g2) andalso
  is_subgraph g1 g2
  

fun is_intersection sub g1 g2 = let
  val vi = V.NSet.intersect (get_vertices g1) (get_vertices g2)
  val ei = E.NSet.intersect (get_edges g1) (get_edges g2)
  val bbi = B.NSet.intersect (get_bboxes g1) (get_bboxes g2)
in (V.NSet.eq (get_vertices sub) vi) andalso
   (E.NSet.eq (get_edges sub) ei) andalso
   (B.NSet.eq (get_bboxes sub) bbi) andalso
   (is_subgraph sub g1) andalso
   (is_subgraph sub g2)
end


fun merge_by_subgraph sub g1 g2 =
  (* Note that without checking disjointness, one of the add_... functions below will likely
     throw an exception. However, this extra work will produce better errors. *)
  if not (is_intersection sub g1 g2) then raise bad_graph_merge_exp (sub,g1,g2)
  else sub (* start with common graph *)
  (* add new verts from g1 and g2 *)
  |> V.NSet.fold (fn v =>
      add_named_vertex v (get_vertex_data g1 v))
     (V.NSet.subtract (get_vertices g1) (get_vertices sub))
  |> V.NSet.fold (fn v =>
      add_named_vertex v (get_vertex_data g2 v))
     (V.NSet.subtract (get_vertices g2) (get_vertices sub))
  (* add new edges from g1 and g2 *)
  |> E.NSet.fold (fn e =>
      add_named_edge e
        (get_edge_dir_and_data g1 e)
        (get_edge_source g1 e)
        (get_edge_target g1 e))
     (E.NSet.subtract (get_edges g1) (get_edges sub))
  |> E.NSet.fold (fn e =>
      add_named_edge e
        (get_edge_dir_and_data g2 e)
        (get_edge_source g2 e)
        (get_edge_target g2 e))
     (E.NSet.subtract (get_edges g2) (get_edges sub))
  (* add new bboxes from g1 and g2 *)
  |> B.NSet.fold (fn bb => add_named_bbox bb)
     (B.NSet.subtract (get_bboxes g1) (get_bboxes sub))
  |> B.NSet.fold (fn bb => add_named_bbox bb)
     (B.NSet.subtract (get_bboxes g2) (get_bboxes sub))
  (* bboxes contained in the intersection may contain more vertices in g1 and g2 than just
   * those in sub, so update containment (and parents) for all bboxes *)
  |> B.NSet.fold (fn bb =>
      add_vertices_to_bbox (get_vertices_in_bbox g1 bb) bb o
      set_bbox_parent_opt bb (get_bbox_parent_opt g1 bb))
     (get_bboxes g1)
  |> B.NSet.fold (fn bb =>
      add_vertices_to_bbox (get_vertices_in_bbox g2 bb) bb o
      set_bbox_parent_opt bb (get_bbox_parent_opt g2 bb))
     (get_bboxes g2)

(* currently, this will fail on bad_graph_merge_exp if any vertices are contained
 * in bboxes. This can be handled by adding common bboxes containing common verts
 * to the subgraph. *)
fun merge_by_vertices g1 g2 = let
  val common_verts = V.NSet.intersect (get_vertices g1) (get_vertices g2)
  val common_graph = V.NSet.fold
                       (fn v => add_named_vertex v (get_vertex_data g1 v))
                       common_verts empty
in merge_by_subgraph common_graph g1 g2
end

(*fun merge_by_vertices g1 g2 =
  if not (E.NSet.is_empty (E.NSet.intersect (get_edges g1) (get_edges g2)))
  then raise bad_graph_merge_exp ("edge names not disjoint",g1,g2)
  else *)


(************************************)
(************* RENAMING *************)
(************************************)

(* TODO : improve these functions for graph-at-once renaming *)

exception rename_do_nothing_exp of unit;

(* implicit args: n1 n2 g; raises X.Rnm. *)
(* renames internal variable name n1 to n2 (assuming n2 is fresh in this graph)
   Updates the name in the param data as well as in each vertex and edge data in
   which the name occurs. *)
fun rename_xname n1 n2 g =
let 
  val param = get_param g
  val vnames_to_update = Param.dep_vnames_of_xname param n1 
  val enames_to_update = Param.dep_enames_of_xname param n1
in 
  g |> update_param (Param.rename_xname n1 n2)
    |> update_vdata (V.NSet.fold
        (V.NTab.map_entry (Param.subst_xname_in_vdata n1 n2)) 
        vnames_to_update)
    |> update_edata (E.NSet.fold
        (E.NTab.map_entry (fn (dir,d) => (dir,Param.subst_xname_in_edata n1 n2 d))) 
        enames_to_update)
end handle X.Rnm.invalid_renaming_exp _ => 
    raise xname_already_exists_exp (n2,g)


fun try_rename1_xname n1 n2 g = SOME (rename_xname n1 n2 g)
  handle xname_already_exists_exp _ => NONE

(* V.name = suggested name *)
(* rename all occurances of old name (1st arg) with new name (2nd arg) 
   n2 must not exist already, n1 must exist *)
(*fun rename_vname n1 n2 g = 
    let 
      val vertices = (get_vertices g)
      (* gets vertex for n1, makes ure it exists *)
      val (nd,(ins,outs)) = case V.NTab.lookup vertices n1 
                             of NONE => raise rename_do_nothing_exp ()
                              | SOME x => x;

      (* update the vertex entry for n1, we make sure that n2 doesn't already exist *)
      val vertices2 = 
          case (vertices |> V.NTab.delete n1
                         (* raises exception if n2 already exsists *)
                         |> V.NTab.try_ins (n2,(nd,(ins,outs))))
           of NONE => raise vname_already_exists_exp (n2,g)
            | SOME vertices2 => vertices2;

      (* update any edges to use the new name n1 instead of n1, using ins and outs lists *)
      val edges2 = 
          (get_edges g)
            |> E.NSet.fold (E.NTab.map_entry (fn (e,(from,_)) => 
                                                       (e,(from,n2)))) ins
            |> E.NSet.fold (E.NTab.map_entry (fn (e,(_,tos)) => 
                                                       (e,(n2,tos)))) outs;

      (* give back new graph *)
    in g |> set_vertices vertices2
         |> set_edges edges2
         |> update_param (Param.rename_vname n1 n2)
    end handle rename_do_nothing_exp () => g;
    (* raise no_such_vname_exp ("rename_vname", n1, g) *)

fun rename_ename en1 en2 g = 
    let 
      val edges = (get_edges g)
      (* gets edge for n1, makes ure it exists *)
      val (e,(from_n,to_n)) = case E.NTab.lookup edges en1
                           of NONE => raise rename_do_nothing_exp ()
                            | SOME x => x;

      (* update the edge entry for n1, we make sure that n2 doesn't 
         already exist *)
      val edges2 = 
          case (edges |> E.NTab.delete en1
                      |> E.NTab.try_ins (en2,(e,(from_n,to_n))))
           of NONE => raise ename_already_exists_exp (en2,g)
            | SOME edges2 => edges2;

      (* update any edges to use the new name n1 instead of n1, using 
         ins and outs lists *)
      val vertices2 = 
          (get_vertices g)
            |> V.NTab.map_entry (fn (n,(ins,outs)) => 
                                   let val outs2 = outs |> E.NSet.delete en1
                                                        |> E.NSet.ins_fresh en2
                                   in (n,(ins,outs2)) end) from_n
            |> V.NTab.map_entry (fn (n,(ins,outs)) =>
                                   let val ins2 = ins |> E.NSet.delete en1
                                                      |> E.NSet.ins_fresh en2
                                   in (n,(ins2,outs)) end) to_n;

    (* return new graph *)
    in g |> set_vertices vertices2
         |> set_edges edges2
         |> update_param (Param.rename_ename en1 en2)
    end handle rename_do_nothing_exp () => g;
    (* no_such_ename_exp ("rename_vname", en1, g) *)
*)



(************************************)
(******* DEPRECATED FUNCTIONS *******)
(************************************)

end
