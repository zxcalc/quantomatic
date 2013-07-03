functor BangGraph(structure Data: GRAPH_DATA) : BANG_GRAPH =
struct

structure Data = Data
open Data
datatype vdata = NVert of Data.nvdata | WVert

(* main bang-graph datatype *)
datatype T = BG of {
  vdata  : vdata VTab.T,
  edata  : (dir_or_undir * Data.edata) ETab.T,
  source : EVFn.T,
  target : EVFn.T,
  
  bboxes   : B.NSet.T,
  bbox_rel : BVRel.T,
  bbox_child_rel : BBRel.T
}

structure Sharing =
struct
  type T = T
  type nvdata = nvdata
  type edata  = edata
  type psubst = psubst
  type subst  = subst
end

type wire = V.name * V.name * V.NSet.T * E.NSet.T (* a chain of edges, representing a wire *)

(* empty bang graph *)
val empty : T = BG {
  vdata  = VTab.empty,
  edata  = ETab.empty,
  source = EVFn.empty,
  target = EVFn.empty,
  
  bboxes   = B.NSet.empty,
  bbox_rel = BVRel.empty,
  bbox_child_rel = BBRel.empty
}

(*********************************)
(********** EXCEPTIONS ***********)
(*********************************)

exception no_such_vertex_exp of string * V.name * T
(*exception no_such_vnames_exp of string * V.NSet.T * T*)
exception duplicate_vertex_exp of V.name * T

exception bad_graph_merge_exp of T * T

exception no_such_edge_exp of string * E.name * T
exception duplicate_edge_exp of E.name * T

exception no_such_bbox_exp of string * B.name * T
exception duplicate_bbox_exp of B.name * T

exception not_an_endpoint_exp of string * E.name * V.name * T

exception wire_vertex_exp of string * T * V.name
exception wire_exp of string * T * wire

exception plugging_exp of T * V.name * V.name

(* !-graph exceptions *)
exception bbox_not_open_exp of (V.name * V.name) * B.name * T
exception bbox_bad_parent_exp of B.name * B.name * T
exception merge_bbox_exp of string

(*********************************)
(******** BASIC ACCESSORS ********)
(*********************************)

(* Note that it is possible to break a graph by calling these updaters, so they should
 * all be private. Also note that these functions are automatically generated, so avoid
 * making changes *)

fun update_vdata f (BG r) = BG {vdata=f(#vdata r),edata= #edata r,source= #source r,target= #target r,bboxes= #bboxes r,bbox_rel= #bbox_rel r,bbox_child_rel= #bbox_child_rel r}
fun update_edata f (BG r) = BG {vdata= #vdata r,edata=f(#edata r),source= #source r,target= #target r,bboxes= #bboxes r,bbox_rel= #bbox_rel r,bbox_child_rel= #bbox_child_rel r}
fun update_source f (BG r) = BG {vdata= #vdata r,edata= #edata r,source=f(#source r),target= #target r,bboxes= #bboxes r,bbox_rel= #bbox_rel r,bbox_child_rel= #bbox_child_rel r}
fun update_target f (BG r) = BG {vdata= #vdata r,edata= #edata r,source= #source r,target=f(#target r),bboxes= #bboxes r,bbox_rel= #bbox_rel r,bbox_child_rel= #bbox_child_rel r}
fun update_bboxes f (BG r) = BG {vdata= #vdata r,edata= #edata r,source= #source r,target= #target r,bboxes=f(#bboxes r),bbox_rel= #bbox_rel r,bbox_child_rel= #bbox_child_rel r}
fun update_bbox_rel f (BG r) = BG {vdata= #vdata r,edata= #edata r,source= #source r,target= #target r,bboxes= #bboxes r,bbox_rel=f(#bbox_rel r),bbox_child_rel= #bbox_child_rel r}
fun update_bbox_child_rel f (BG r) = BG {vdata= #vdata r,edata= #edata r,source= #source r,target= #target r,bboxes= #bboxes r,bbox_rel= #bbox_rel r,bbox_child_rel=f(#bbox_child_rel r)}

fun get_vdata          (BG r) = #vdata r
fun get_edata          (BG r) = #edata r
fun get_source         (BG r) = #source r
fun get_target         (BG r) = #target r
fun get_bboxes         (BG r) = #bboxes r
fun get_bbox_rel       (BG r) = #bbox_rel r
fun get_bbox_child_rel (BG r) = #bbox_child_rel r

val set_vdata             = update_vdata          o K
val set_edata             = update_edata          o K
val set_source            = update_source         o K
val set_target            = update_target         o K
val set_bboxes            = update_bboxes         o K
val set_bbox_rel          = update_bbox_rel       o K
val set_bbox_child_rel    = update_bbox_child_rel o K

(*********************************)
(******* DERIVED ACCESSORS *******)
(*********************************)

(* any graph updaters below should produce valid !-graphs (or fail with
 * exception/NONE) *)

(* note a graph is empty iff it has no vertices or bboxes *)
fun is_empty g = VTab.is_empty (get_vdata g) andalso B.NSet.is_empty (get_bboxes g)

fun get_vertices g = VTab.get_dom_set (get_vdata g)
fun get_edges g = ETab.get_dom_set (get_edata g)
(* get_bboxes is an accessor fn *)

fun has_vertex g = V.NSet.contains (get_vertices g)
fun has_edge g   = E.NSet.contains (get_edges g)
fun has_bbox g   = B.NSet.contains (get_bboxes g)

fun is_wire_vertex g v =
  case VTab.get_opt (get_vdata g) v
    of SOME WVert => true
     | SOME _ => false
     | NONE => raise no_such_vertex_exp ("is_wire_vertex",v,g)
fun is_node_vertex g v = not (is_wire_vertex g v)
fun get_wire_vertices g = V.NSet.filter (is_wire_vertex g) (get_vertices g)
fun get_node_vertices g = V.NSet.filter (is_node_vertex g) (get_vertices g)

val num_vertices = V.NSet.cardinality o get_vertices
val num_edges = E.NSet.cardinality o get_edges
val num_bboxes = B.NSet.cardinality o get_bboxes


fun get_edge_source g e = EVFn.get (get_source g) e
  handle Map.no_such_elem_exp () =>
    raise no_such_edge_exp ("get_edge_source", e, g)

fun get_edge_target g e = EVFn.get (get_target g) e
  handle Map.no_such_elem_exp () =>
    raise no_such_edge_exp ("get_edge_target", e, g)

fun get_vertex_data g v = VTab.get (get_vdata g) v
  handle Map.no_such_elem_exp () =>
    raise no_such_vertex_exp ("get_vertex_data", v, g)

fun get_edge_dir_and_data g e = ETab.get (get_edata g) e
  handle Map.no_such_elem_exp () =>
    raise no_such_edge_exp ("get_edge_dir_and_data", e, g)

fun get_edge_info g e = (get_edge_dir_and_data g e, (get_edge_source g e, get_edge_target g e))

fun get_edge_dir g e  = #1 (get_edge_dir_and_data g e)
fun get_edge_data g e = #2 (get_edge_dir_and_data g e)
fun is_dir_edge g e = (get_edge_dir g e) = Directed

fun update_vertex_data f v g =
  if has_vertex g v then g |> update_vdata (VTab.map_entry f v)
  else raise no_such_vertex_exp ("update_vertex_data", v, g)

fun update_edge_data f e g =
  if has_edge g e then
    g |> update_edata (ETab.map_entry (fn (dir,d) => (dir, f d)) e)
  else
    raise no_such_edge_exp ("update_edge_data", e, g)

fun update_edge_dir f e g =
  if has_edge g e then
    g |> update_edata (ETab.map_entry (fn (dir,d) => (f dir, d)) e)
  else
    raise no_such_edge_exp ("update_edge_dir", e, g)

fun set_vertex_data d = update_vertex_data (K d)
fun set_edge_data d = update_edge_data (K d)
fun set_edge_dir dir = update_edge_dir (K dir)

fun get_vertices_in_bbox g bb = 
  if has_bbox g bb then BVRel.img (get_bbox_rel g) bb
  else raise no_such_bbox_exp ("get_vertices_in_bbox", bb, g)

fun get_bboxes_containing_vertex g v =
  if has_vertex g v then BVRel.inv_img (get_bbox_rel g) v
  else raise no_such_vertex_exp ("get_bboxes_containing_vertex", v, g)

fun get_bbox_parents g bb =
  if has_bbox g bb then  BBRel.inv_img (get_bbox_child_rel g) bb
  else raise no_such_bbox_exp ("get_bbox_parents", bb, g)

fun get_bbox_children g bb =
  if has_bbox g bb then  BBRel.img (get_bbox_child_rel g) bb
  else raise no_such_bbox_exp ("get_bbox_children", bb, g)

fun bbox_has_parents g bb =
  if has_bbox g bb then  BBRel.cod_contains (get_bbox_child_rel g) bb
  else raise no_such_bbox_exp ("bbox_has_parents", bb, g)

fun bbox_has_children g bb =
  if has_bbox g bb then  BBRel.dom_contains (get_bbox_child_rel g) bb
  else raise no_such_bbox_exp ("bbox_has_children", bb, g)

fun get_bboxed g = BVRel.get_cod_set (get_bbox_rel g)
fun get_unbboxed g = V.NSet.subtract (get_vertices g) (get_bboxed g)
fun get_empty_bboxes g = B.NSet.subtract (get_bboxes g) (BVRel.get_dom_set (get_bbox_rel g))

fun is_bboxed g v = if has_vertex g v
  then V.NSet.contains (BVRel.get_cod_set (get_bbox_rel g)) v
  else raise no_such_vertex_exp ("is_bboxed", v, g)


(**************************************)
(***** EXTRA DATA/SUBST FUNCTIONS *****)
(**************************************)

fun get_nvdata_tab g = VTab.fold (fn (k,v) => case v of NVert nv => VTab.add (k, nv) | _ => I) (get_vdata g) VTab.empty
val get_vdata_tab    = get_vdata
fun get_edata_tab  g = ETab.fold (fn (k,v) => ETab.add (k, snd v)) (get_edata g) ETab.empty
val default_vdata = WVert
fun default_vdata_of_typestring "wire-vertex" = WVert
  | default_vdata_of_typestring s = NVert (default_nvdata_of_typestring s)
fun typestring_of_vdata WVert = "wire-vertex"
  | typestring_of_vdata (NVert d) = typestring_of_nvdata d

fun subst_in_vdata sub (NVert nv) =
      let val (sub', nv') = subst_in_nvdata sub nv
      in (sub', NVert nv') end
  | subst_in_vdata sub (WVert) = (sub, WVert)

fun apply_data_subst sub graph =
  (sub, graph)
    |> V.NSet.fold (fn v => fn (sub, g) => (
         let val (sub', vd) = subst_in_vdata sub (get_vertex_data g v)
         in (sub', g |> set_vertex_data vd v) end
       )) (get_vertices graph)
    |> E.NSet.fold (fn e => fn (sub, g) => (
         let val (sub', ed) = subst_in_edata sub (get_edge_data g e)
         in (sub', g |> set_edge_data ed e) end
       )) (get_edges graph)

fun vdata_eq (NVert nv1, NVert nv2) = Data.nvdata_eq (nv1,nv2)
  | vdata_eq (WVert, WVert) = true
  | vdata_eq _ = false

fun match_vdata (NVert nv1, NVert nv2) psubst = Data.match_nvdata (nv1,nv2) psubst
  | match_vdata (WVert, WVert) psubst = SOME psubst
  | match_vdata _ _ = NONE

fun pretty_vdata (NVert n) = Pretty.block[Pretty.str "N{", Data.pretty_nvdata n, Pretty.str "}"]
  | pretty_vdata (WVert)   = Pretty.str "W{}"


fun init_psubst pattern target =
  init_psubst_from_data (get_nvdata_tab pattern, get_edata_tab pattern)
                        (get_nvdata_tab target,  get_edata_tab target)



(**************************************)
(******** TRAVERSAL FUNCTIONS *********)
(**************************************)

(* given an edge and vertex, get the other end *)
fun edge_get_other_vertex g e v1 = let
  val (s,t) = (get_edge_source g e, get_edge_target g e)
in if V.name_eq (v1, s) then t
   else if V.name_eq(v1, t) then s
   else raise not_an_endpoint_exp ("edge_get_other_vertex",e,v1,g)
end

(* Convenience functions for adjacent edges *)

(* note these ignore direction *)
fun get_in_edges g v =
  if has_vertex g v then EVFn.inv_img (get_target g) v
  else raise no_such_vertex_exp ("get_in_edges", v, g)


fun get_out_edges g v =
  if has_vertex g v then EVFn.inv_img (get_source g) v
  else raise no_such_vertex_exp ("get_out_edges", v, g)

fun get_adj_edges' g v =
  E.NSet.union_merge (EVFn.inv_img (get_source g) v)
                     (EVFn.inv_img (get_target g) v)
fun get_adj_edges g v =
  if has_vertex g v
  then get_adj_edges' g v
  else raise no_such_vertex_exp ("get_adj_edges", v, g)

fun get_successor_vertices g v =
  E.NSet.fold (fn e => (
    if is_dir_edge g e
    then let val v1 = get_edge_target g e
         in if V.name_eq (v,v1) then I else V.NSet.add v1
         end
    else I
  )) (get_out_edges g v) V.NSet.empty

(* vertices connected to a directed in-edge of the given one *)
fun get_predecessor_vertices g v =
  E.NSet.fold (fn e => (
    if is_dir_edge g e
    then let val v1 = get_edge_source g e
         in if V.name_eq (v,v1) then I else V.NSet.add v1
         end
    else I
  )) (get_in_edges g v) V.NSet.empty

(* vertices connected to an undirected edge of the given one *)
fun get_sibling_vertices g v =
  V.NSet.empty
    |> E.NSet.fold (fn e => (
         if not (is_dir_edge g e)
         then let val v1 = get_edge_target g e
              in if V.name_eq (v,v1) then I else V.NSet.add v1
              end
         else I
       )) (get_out_edges g v)
    |> E.NSet.fold (fn e => (
         if not (is_dir_edge g e)
         then let val v1 = get_edge_source g e
              in if V.name_eq (v,v1) then I else V.NSet.add v1
              end
         else I
       )) (get_in_edges g v)

fun get_adj_vertices g v =
  V.NSet.empty |> E.NSet.fold (fn e =>
                   (let val v1 = get_edge_source g e
                    in if V.name_eq (v,v1) then I else V.NSet.add v1 end)) (get_in_edges g v)
               |> E.NSet.fold (fn e =>
                   (let val v1 = get_edge_target g e
                    in if V.name_eq (v,v1) then I else V.NSet.add v1 end)) (get_out_edges g v)

fun get_adj_vertices_to_set g vset =
  V.NSet.subtract (V.NSet.fold (V.NSet.union_merge o get_adj_vertices g) vset V.NSet.empty) vset

fun get_in_dir_edges g = E.NSet.filter (fn e => get_edge_dir g e = Directed) o get_in_edges g
fun get_out_dir_edges g = E.NSet.filter (fn e => get_edge_dir g e = Directed) o get_out_edges g
fun get_adj_undir_edges g = E.NSet.filter (fn e => get_edge_dir g e = Undirected) o get_adj_edges g

fun is_self_loop g e = V.name_eq (get_edge_source g e, get_edge_target g e)
fun get_self_loops g v = E.NSet.filter (is_self_loop g) (get_adj_edges g v)

fun get_dir_self_loops g v =
  E.NSet.filter (fn e => is_self_loop g e andalso is_dir_edge g e)
                (get_adj_edges' g v)
fun get_undir_self_loops g v =
  E.NSet.filter (fn e => is_self_loop g e andalso not (is_dir_edge g e))
                (get_adj_edges' g v)


(* set the arity. note that (undir) self-loops are counted twice *)
fun get_arity g v = let
  fun card f = E.NSet.cardinality (f g v)
in Arity.zero |> Arity.set_in (card get_in_dir_edges)
              |> Arity.set_out (card get_out_dir_edges)
              |> Arity.set_undir (card get_adj_undir_edges + card get_undir_self_loops)
end

fun adj_edge_classes g v = (
  get_in_dir_edges g v,
  get_out_dir_edges g v,
  get_adj_undir_edges g v
)

fun get_only_edge g v = E.NSet.tryget_singleton (get_adj_edges' g v)

(* need these here for add_named_edge *)
fun wv_get_other_edge g wv e = E.NSet.tryget_singleton (E.NSet.delete e (get_adj_edges g wv))
fun get_wire_containing_edge g e = let
  (* proceed in the direction of v away from prev_e until we hit e, a boundary, or a
     node-vertex. return endpoint and edges encountered *)
  fun traverse v prev_e =
    if is_node_vertex g v then (v, V.NSet.empty, E.NSet.empty)
    else case wv_get_other_edge g v prev_e
           of SOME new_e =>
             if E.name_eq (e, new_e)  (* come back around, is a circle *)
             then (get_edge_source g e, V.NSet.empty, E.NSet.empty) (* so we get the same node for both endpoints *)
             else (* v is a wire vertex that is not an endpoint *)
               let val (v', wvs, es) = traverse (edge_get_other_vertex g new_e v) new_e
               in (v', wvs |> V.NSet.add v, es |> E.NSet.add new_e)
               end
            | NONE => (v, V.NSet.empty, E.NSet.empty)
  val (s_end,s_wvs,s_es) = traverse (get_edge_source g e) e
  val (t_end,t_wvs,t_es) = traverse (get_edge_target g e) e
  val es = s_es |> E.NSet.union_merge t_es |> E.NSet.add e
(* for circles, we take the minimum name to be s=t and the rest to be internal wire-vertices *)
in if V.name_eq (s_end,t_end) andalso is_wire_vertex g s_end then
     let
       val wvs = s_wvs |> V.NSet.union_merge t_wvs |> V.NSet.add s_end
       val min_wv = (the o V.NSet.get_min) wvs
     in (min_wv, min_wv, wvs |> V.NSet.delete min_wv, es)
     end
   else (s_end, t_end, s_wvs |> V.NSet.union_merge t_wvs, es)
end


(*****************************************)
(****** ADD/REMOVE/MODIFY FUNCTIONS ******)
(*****************************************)

fun add_vertex_to_bbox bb v g =
  if (has_bbox g bb) then g |> update_bbox_rel (BVRel.add (bb, v))
  else raise no_such_bbox_exp ("add_vertex_to_bbox", bb, g)

fun add_vertices_to_bbox bb = V.NSet.fold (add_vertex_to_bbox bb)


fun add_named_edge e (dir,d) s t g =
  if (has_edge g e) then raise duplicate_edge_exp (e,g)
  else
    let
      val () = if has_vertex g s then ()
               else raise no_such_vertex_exp ("add_named_edge", s, g)
      val () = if has_vertex g t then ()
               else raise no_such_vertex_exp ("add_named_edge", t, g)
      (* crazy long function to check that we don't add an incompatible
       * edge to a wire vertex *)
      fun can_link get_bad_edge_end v =
        if not (is_wire_vertex g v) then ()
        else let val v_es = get_adj_edges' g v in
          if E.NSet.is_empty v_es then ()
          else
            case E.NSet.tryget_singleton v_es
              of NONE => raise wire_vertex_exp ("too many edges",g,v)
               | SOME e =>
                  let val (dod,ed) = get_edge_dir_and_data g e in
                    if dod <> dir
                    then raise wire_vertex_exp ("directedness mismatch",g,v)
                    else if not (edata_eq (ed,d))
                    then raise wire_vertex_exp ("data mismatch",g,v)
                    else if dir = Directed andalso
                            V.name_eq (v,get_bad_edge_end g e)
                    then raise wire_vertex_exp ("directed edges clash",g,v)
                    else if dir = Undirected andalso
                            V.name_eq (get_edge_source g e,get_edge_target g e)
                    then raise wire_vertex_exp ("too many edges",g,v)
                    else ()
                  end
        end
      val _ = can_link get_edge_source s
      val _ = can_link get_edge_target t
      fun extend_bbs g = let
        val (_,_,wvs,_) = get_wire_containing_edge g e
        val s_bbs = get_bboxes_containing_vertex g s
        val t_bbs = get_bboxes_containing_vertex g t
        val wire_bbs = B.NSet.union_merge s_bbs t_bbs
      in
        B.NSet.fold (fn b => add_vertices_to_bbox b wvs) wire_bbs g
      end
    in g |> update_edata (ETab.add (e,(dir,d)))
         |> update_source (EVFn.add (e, s))
         |> update_target (EVFn.add (e, t))
         |> extend_bbs
    end

fun add_edge dd s t g = let
  val fresh_name = E.NSet.new (get_edges g) E.default_name
in (fresh_name, add_named_edge fresh_name dd s t g)
end
  
fun add_edge_anon dd s t g = snd (add_edge dd s t g)

fun delete_edge e g =
  g |> update_edata (ETab.delete e)
    |> update_source (EVFn.delete e)
    |> update_target (EVFn.delete e)

fun add_named_vertex v d g =
  if (has_vertex g v) then raise duplicate_vertex_exp (v,g)
  else g |> update_vdata (VTab.add (v,d))
  
fun add_vertex d g = let
  val fresh_name = V.NSet.new (get_vertices g) V.default_name
in (fresh_name, add_named_vertex fresh_name d g)
end

fun add_vertex_anon d g = snd (add_vertex d g)
    
fun delete_vertex v g =
  g |> update_vdata (VTab.delete v)
    |> E.NSet.fold delete_edge (get_adj_edges' g v)
    |> update_bbox_rel (BVRel.inv_delete v)

fun add_named_bbox bb g =
  if (has_bbox g bb) then raise duplicate_bbox_exp (bb,g)
  else g |> update_bboxes (B.NSet.add bb)

fun add_bbox g = let
  val fresh_name = B.NSet.new (get_bboxes g) B.default_name
in (fresh_name, add_named_bbox fresh_name g)
end
  
fun add_bbox_anon g = snd (add_bbox g)

fun delete_bbox bb g =
  g |> update_bboxes (B.NSet.delete bb)
    |> update_bbox_rel (BVRel.delete bb)
    |> update_bbox_child_rel (BBRel.delete bb)
    |> update_bbox_child_rel (BBRel.inv_delete bb)

fun remove_vertex_from_bbox bb v g =
  if (has_bbox g bb) then g |> update_bbox_rel (BVRel.unmap (bb, v))
  else raise no_such_bbox_exp ("remove_vertex_from_bbox", bb, g)
  
fun remove_vertices_from_bbox bb = V.NSet.fold (remove_vertex_from_bbox bb)

fun add_child_to_bbox' bb bb_child g =
  if not (has_bbox g bb_child)
  then raise no_such_bbox_exp ("add_child_to_bbox'", bb_child, g)
  else if B.name_eq (bb,bb_child)
  then g (* no-op *)
  else if not (V.NSet.sub_set (get_vertices_in_bbox g bb_child)
                              (get_vertices_in_bbox g bb))
  then raise bbox_bad_parent_exp (bb,bb_child,g)
  else
    let
      fun add_child rel =
        if BBRel.is_mapped rel (bb_child,bb)
        then raise bbox_bad_parent_exp (bb,bb_child,g)
        else
          let
            val grandchildren = B.NSet.add bb_child (BBRel.img rel bb_child)
            val grandparents = B.NSet.add bb (BBRel.inv_img rel bb)
          in
            B.NSet.fold (fn bp => B.NSet.fold (fn bc => BBRel.add (bp,bc))
                                              grandchildren)
                        grandparents
                        rel
          end
    in
      g |> update_bbox_child_rel add_child
    end

fun add_child_to_bbox bb bb_child g =
  if not (has_bbox g bb) then raise no_such_bbox_exp ("add_child_to_bbox", bb, g)
  else add_child_to_bbox' bb bb_child g

fun add_children_to_bbox bb bb_children g =
  if not (has_bbox g bb) then raise no_such_bbox_exp ("add_children_to_bbox", bb, g)
  else B.NSet.fold (add_child_to_bbox' bb) bb_children g

(* TODO: these functions need to enforce transitive closure! *)

fun remove_child_from_bbox bb bb_child g =
  if not (has_bbox g bb) then raise no_such_bbox_exp ("remove_child_from_bbox", bb, g)
  else if not (has_bbox g bb_child) then raise no_such_bbox_exp ("remove_child_from_bbox", bb_child, g)
  else
    let
      fun remove_child rel =
        if B.NSet.nonempty_intersect (BBRel.img rel bb)
                                     (BBRel.inv_img rel bb_child)
        then rel
        else BBRel.unmap (bb,bb_child) rel
    in
      g |> update_bbox_child_rel remove_child
    end

fun clear_bbox_children bb g =
  if not (has_bbox g bb) then raise no_such_bbox_exp ("clear_bbox_children", bb, g)
  else g |> update_bbox_child_rel (BBRel.delete bb)

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
  (VTab.forall (fn (n,d1) => vdata_eq (d1, get_vertex_data g2 n)) (get_vdata g1)) andalso 
  (* check same edge data, and source and target maps agree *)
  (ETab.forall (fn (n,(dir1,d1)) => 
    let
      val (dir2, d2) = get_edge_dir_and_data g2 n
    in dir1 = dir2 andalso
       Data.edata_eq (d1, d2) andalso
       V.name_eq (get_edge_source g1 n, get_edge_source g2 n) andalso
       V.name_eq (get_edge_target g1 n, get_edge_target g2 n)
    end) (get_edata g1)) andalso
  (* check bbox containment and children agree *)
  (B.NSet.forall (fn bb =>
    V.NSet.sub_set (get_vertices_in_bbox g1 bb) (get_vertices_in_bbox g2 bb) andalso
    B.NSet.eq (get_bbox_children g1 bb) (get_bbox_children g2 bb)
  ) (get_bboxes g1))
end

(* should be equivalent to (is_subgraph g1 g2 andalso is_subgraph g2 g1) *)
fun exact_eq g1 g2 =
  (num_vertices g1 = num_vertices g2) andalso
  (num_edges g1 = num_edges g2) andalso
  (num_bboxes g1 = num_bboxes g2) andalso
  is_subgraph g1 g2 andalso
  (B.NSet.forall (fn bb =>
    V.NSet.eq (get_vertices_in_bbox g1 bb) (get_vertices_in_bbox g2 bb)
  ) (get_bboxes g1))
  

(* note this assumes that sg < g. In particular, the edges in sg are not (explicitly) deleted. *)
fun delete_subgraph sg g =
  g |> V.NSet.fold delete_vertex (get_vertices sg)
    |> B.NSet.fold delete_bbox (get_bboxes sg)


(************************************)
(************* RENAMING *************)
(************************************)

(*exception rename_do_nothing_exp of unit*)



(* rename vertices using the given substitution *)
fun rename_vertices sub g = let
  (* this will fail with name_clash_exp if the renaming is bad *)
  val rnm = VSub.extend_fixed (get_vertices g) sub
  val rnmi = VSub.inverse_of rnm
in
  g |> update_vdata    (fn m => VTab.compose (m,rnmi))
    |> update_source   (fn m => EVFn.compose (rnm,m))
    |> update_target   (fn m => EVFn.compose (rnm,m))
    |> update_bbox_rel (fn m => BVRel.compose (rnm,m))
end

fun rename_vertex n1 n2 g = rename_vertices (VSub.empty |> VSub.add (n1,n2)) g
  handle VSub.name_clash_exp _ => raise duplicate_vertex_exp (n2,g)
fun rename_vertex_opt n1 n2 g = SOME (rename_vertex n1 n2 g)
  handle duplicate_vertex_exp _ => NONE

(* rename edges using the given substitution *)
fun rename_edges sub g = let
  (* this will fail with name_clash_exp if the renaming is bad *)
  val rnm = ESub.extend_fixed (get_edges g) sub
  val rnmi = ESub.inverse_of rnm
in
  g |> update_edata  (fn m => ETab.compose (m,rnmi))
    |> update_source (fn m => EVFn.compose (m,rnmi))
    |> update_target (fn m => EVFn.compose (m,rnmi))
end

fun rename_edge n1 n2 g = rename_edges (ESub.empty |> ESub.add (n1,n2)) g
  handle ESub.name_clash_exp _ => raise duplicate_edge_exp (n2,g)
fun rename_edge_opt n1 n2 g = SOME (rename_edge n1 n2 g)
  handle duplicate_edge_exp _ => NONE

fun rename_bboxes sub g = let
  (* this will fail with name_clash_exp if the renaming is bad *)
  val rnm = BSub.extend_fixed (get_bboxes g) sub
  val rnmi = BSub.inverse_of rnm
in
  g |> update_bboxes (BSub.img_of_set rnm)
    |> update_bbox_rel (fn m => BVRel.compose (m, rnmi))
    |> update_bbox_child_rel (fn m => BBRel.compose3 (rnm, m, rnmi))
end

fun rename_bbox n1 n2 g = rename_bboxes (BSub.empty |> BSub.add (n1,n2)) g
  handle BSub.name_clash_exp _ => raise duplicate_bbox_exp (n2,g)
fun rename_bbox_opt n1 n2 g = SOME (rename_bbox n1 n2 g)
  handle duplicate_bbox_exp _ => NONE

fun rename_bang_graph (vsub,esub,bsub) g = let
  val vrnm  = VSub.extend_fixed (get_vertices g) vsub
  val vrnmi = VSub.inverse_of vrnm
  val ernm  = ESub.extend_fixed (get_edges g) esub
  val ernmi = ESub.inverse_of ernm
  val brnm  = BSub.extend_fixed (get_bboxes g) bsub
  val brnmi = BSub.inverse_of brnm
in
  (
    (vrnm,ernm,brnm),
    g |> update_vdata    (fn m => VTab.compose (m,vrnmi))
      |> update_edata    (fn m => ETab.compose (m,ernmi))
      |> update_source   (fn m => EVFn.compose3 (vrnm,m,ernmi))
      |> update_target   (fn m => EVFn.compose3 (vrnm,m,ernmi))
      |> update_bboxes   (BSub.img_of_set brnm)
      |> update_bbox_rel (fn m => BVRel.compose3 (vrnm, m, brnmi))
      |> update_bbox_child_rel (fn m => BBRel.compose3 (brnm, m, brnmi))
  )
end

val rename_bang_graph_anon = snd oo rename_bang_graph
fun rename_ograph (vsub,esub) g = let
  val ((vsub',esub',_), g') = rename_bang_graph (vsub,esub,BSub.empty) g
in ((vsub',esub'), g')
end
val rename_ograph_anon = snd oo rename_ograph


fun edges_between g v1 v2 =
  if V.name_eq (v1,v2)
  then get_self_loops g v1
  else E.NSet.intersect (get_adj_edges g v1) (get_adj_edges g v2)
fun has_edges_between g v1 v2 = not (E.NSet.is_empty (edges_between g v1 v2))

fun dir_edges_between g v1 v2 =
  (
    E.NSet.intersect (get_out_dir_edges g v1) (get_in_edges g v2),
    E.NSet.intersect (get_out_dir_edges g v2) (get_in_edges g v1)
  )


(* avoids re-checking is_wire_vertex *)
fun is_input' g v = E.NSet.is_empty (get_in_edges g v) andalso
                    (case E.NSet.get_local_bot (get_out_edges g v)
                       of SOME e => is_dir_edge g e
                        | NONE => false)
fun is_output' g v = E.NSet.is_empty (get_out_edges g v) andalso
                     (case E.NSet.get_local_bot (get_in_edges g v)
                        of SOME e => is_dir_edge g e
                         | NONE => false)
fun is_boundary' g v = let
  val in_edges = get_in_edges g v
  val out_edges = get_out_edges g v
in
  (E.NSet.is_empty in_edges andalso E.NSet.is_empty out_edges)
  orelse
  (E.NSet.is_empty in_edges andalso E.NSet.is_singleton out_edges)
  orelse
  (E.NSet.is_singleton in_edges andalso E.NSet.is_empty out_edges)
end

fun is_input g v    = is_wire_vertex g v andalso is_input' g v
fun is_output g v   = is_wire_vertex g v andalso is_output' g v
fun is_boundary g v = is_wire_vertex g v andalso is_boundary' g v

fun get_boundary g = V.NSet.filter (is_boundary' g) (get_wire_vertices g)
fun get_inputs g   = V.NSet.filter (is_input' g)    (get_wire_vertices g)
fun get_outputs g  = V.NSet.filter (is_output' g)   (get_wire_vertices g)


fun merge g1 g2 = let
  (* some pre-checking *)
  (* we (may) have an implied span, where the centre of the span is
   * the subgraph of common names (which may not be a graph at all) *)

  (* First, we check that the inclusions reflect !-box containment.
   * This is actually slightly stronger than necessary, but will almost
   * always be satisfied in practice.
   *)
  val shared_bbs = B.NSet.intersect (get_bboxes g1) (get_bboxes g2)
  val _ = if B.NSet.forall (fn b =>
              B.NSet.eq (BBRel.inv_img (get_bbox_child_rel g1) b)
                        (BBRel.inv_img (get_bbox_child_rel g2) b)
              ) shared_bbs
          then ()
          else raise bad_graph_merge_exp (g1,g2)
  val shared_vs = V.NSet.intersect (get_vertices g1) (get_vertices g2)
  val _ = if V.NSet.forall (fn b =>
              B.NSet.eq (BVRel.inv_img (get_bbox_rel g1) b)
                        (BVRel.inv_img (get_bbox_rel g2) b)
              ) shared_vs
          then ()
          else raise bad_graph_merge_exp (g1,g2)

  (* Now we check that the inclusions are boundary-coherent; this means
   * that anything that is an input of the intersection is an input of
   * either g1 or g2, and similarly for outputs.
   * This is complicated by:
   * (a) the fact we store wire-vertex types on edges
   * (b) undirected edges
   *)
  val _ = if V.NSet.forall (fn v =>
        not (is_wire_vertex g1 v) orelse
        let
          val adj1 = get_adj_edges' g1 v
          val adj2 = get_adj_edges' g2 v
          val adj = E.NSet.intersect adj1 adj2
          fun input_somewhere v = is_input' g1 v orelse is_input' g2 v
          fun output_somewhere v = is_output' g1 v orelse is_output' g2 v
          fun undir_bnd_everywhere v = E.NSet.is_singleton adj1
                                       andalso
                                       E.NSet.is_singleton adj2
          fun undir_bnd_somewhere v = E.NSet.is_singleton adj1
                                      orelse
                                      E.NSet.is_singleton adj2
          val types_agree =
            case (E.NSet.get_local_bot adj1,E.NSet.get_local_bot adj2)
              of (SOME e1,SOME e2) =>
                    let
                      val (dir1,data1) = get_edge_dir_and_data g1 e1
                      val (dir2,data2) = get_edge_dir_and_data g2 e2
                    in
                      dir1 = dir2 andalso edata_eq (data1,data2)
                    end
               | _ => true
        in
          types_agree andalso
          if E.NSet.is_empty adj
          then (input_somewhere v andalso output_somewhere v)
               orelse undir_bnd_everywhere v
          else
            case E.NSet.tryget_singleton adj
              of NONE => true (* not a boundary at all *)
               | SOME e =>
                   let
                     val s = get_edge_source g1 e
                     val t = get_edge_target g1 e
                   in
                     if V.name_eq (s,t)
                     then true (* circle *)
                     else if get_edge_dir g1 e = Undirected
                     then undir_bnd_somewhere v
                     else if V.name_eq (s,v)
                     then output_somewhere v
                     else input_somewhere v
                   end
        end
      ) shared_vs
      then () else raise bad_graph_merge_exp (g1,g2)

  (* Of course, we also need to check that the intersection is truly a
   * subgraph of both g1 and g2 (ie: that they agree on types, edge sources
   * and edge targets); we check that during the construction below *)

  (* start with g1 and add things from g2 to it *)
  val vdata =
        VTab.fold (fn (v,d) => fn tab =>
          case VTab.get_opt tab v
            of SOME d' => if vdata_eq (d,d') then tab
                          else raise bad_graph_merge_exp (g1,g2)
             | NONE    => VTab.add (v,d) tab
          ) (get_vdata g2) (get_vdata g1)
  val edata =
        ETab.fold (fn (e,(d as (dir,dd))) => fn tab =>
          case ETab.get_opt tab e
            of SOME (d' as (dir',dd')) =>
                  if dir = dir' andalso edata_eq (dd,dd') then tab
                  else raise bad_graph_merge_exp (g1,g2)
             | NONE    => ETab.add (e,d) tab
          ) (get_edata g2) (get_edata g1)
  val source =
        EVFn.fold (fn (e,v) => fn f =>
          case EVFn.get_opt f e
            of SOME v' => if V.name_eq (v,v')
                          then f
                          else if fst (ETab.get edata e) = Undirected
                                  andalso (* might just be reversed *)
                                  EVFn.is_mapped (get_target g1) (e,v)
                                  andalso
                                  EVFn.is_mapped (get_target g2) (e,v')
                          then f
                          else raise bad_graph_merge_exp (g1,g2)
             | NONE    => EVFn.add (e,v) f
          ) (get_source g2) (get_source g1)
  val target =
        EVFn.fold (fn (e,v) => fn f =>
          case EVFn.get_opt f e
            of SOME v' => if V.name_eq (v,v')
                          then f
                          else if fst (ETab.get edata e) = Undirected
                                  andalso (* might just be reversed *)
                                  EVFn.is_mapped (get_source g1) (e,v)
                                  andalso
                                  EVFn.is_mapped (get_source g2) (e,v')
                          then f
                          else raise bad_graph_merge_exp (g1,g2)
             | NONE    => EVFn.add (e,v) f
          ) (get_target g2) (get_target g1)
  val bboxes = B.NSet.union_merge (get_bboxes g2) (get_bboxes g1)
  val bbox_rel = BVRel.fold BVRel.add (get_bbox_rel g2) (get_bbox_rel g1)
  val bbox_child_rel = BBRel.fold BBRel.add (get_bbox_child_rel g2)
                                            (get_bbox_child_rel g1)
  (* check wvs *)
in
  BG { vdata = vdata, edata = edata,
       source = source, target = target,
       bboxes = bboxes, bbox_rel = bbox_rel,
       bbox_child_rel = bbox_child_rel }
end

fun merge_opt g1 g2 = SOME (merge g1 g2)
  handle bad_graph_merge_exp _ => NONE


(********************************)
(******** WIRE FUNCTIONS ********)
(********************************)

(* the edge-set is unique to a particular wire *)
fun wire_ord ( (_,_,_,es) , (_,_,_,es') ) = E.NSet.ord (es,es')
fun wire_eq w1 w2 = wire_ord (w1, w2) = EQUAL
fun pretty_wire (s,t,w,e) = Pretty.list "wire(" ")" [V.pretty_name s, V.pretty_name t, V.NSet.pretty w, E.NSet.pretty e]
fun delete_wire (_,_,wvs,es) h = V.NSet.fold delete_vertex wvs (E.NSet.fold delete_edge es h)

fun get_wires_for_edges g es = let
  fun gw es' = if E.NSet.is_empty es' then []
              else let val wire = get_wire_containing_edge g ((the o
              E.NSet.get_min) es')
                   in wire :: gw (es' |> E.NSet.remove_set (#4 wire))
                   end
in gw es
end

fun get_wire_list g = get_wires_for_edges g (get_edges g)

fun assert_wire_coherent g (w as (_,_,_,es)) = let
  val sample_edge = the (E.NSet.get_local_bot es)
  val (dir_or_undir,data) = get_edge_dir_and_data g sample_edge
in
  (* FIXME: check all in same direction if directed *)
  E.NSet.fold (fn e => fn w =>
    let
      val (dou',data') = get_edge_dir_and_data g e
    in
      if (dir_or_undir <> dou')
      then raise wire_exp ("Directedness of edges differs",g,w)
      else if not (Data.edata_eq (data,data'))
      then raise wire_exp ("Data of edges differs",g,w)
      else w 
    end) es w
end

fun wire_needs_wvs g (s,t,wvs,_) =
  case V.NSet.get_local_bot wvs
    of NONE => false
     | SOME wv =>
        if is_wire_vertex g s then false
        else if is_wire_vertex g t then false
        else
          let
            val bbs = get_bboxes_containing_vertex g wv
            fun bbox_only_here b = let
              val vs = get_vertices_in_bbox g b
            in
              V.NSet.eq vs wvs
            end
          in
            B.NSet.exists bbox_only_here bbs
          end

fun is_wire_minimal g (wire as (_,_,wvs,_)) =
  V.NSet.is_empty wvs orelse
  (V.NSet.is_singleton wvs andalso wire_needs_wvs g wire)

fun minimise_wire' (wire as (s,t,wvs,es)) g = let
  val dir_and_data = get_edge_dir_and_data g ((the o E.NSet.get_local_bot) es)
in
  if wire_needs_wvs g wire
  then
    let
      val any_wv = the (V.NSet.get_local_bot wvs)
      val bbs    = get_bboxes_containing_vertex g any_wv
      val g      = g |> delete_wire wire
      val (wv,g) = g |> add_vertex WVert
      val g      = g |> B.NSet.fold (fn b => add_vertex_to_bbox b wv) bbs
      val (e1,g) = g |> add_edge dir_and_data s wv
      val (e2,g) = g |> add_edge dir_and_data wv t
    in ((s,t,V.NSet.single wv,E.NSet.of_list [e1,e1]), g) end
  else
    let val (e,g') = g |> delete_wire wire
                       |> add_edge dir_and_data s t
    in ((s,t,V.NSet.empty,E.NSet.single e), g') end
end
fun minimise_wire (wire as (_,_,wvs,_)) g = let
  val _ = assert_wire_coherent g wire
in
  if is_wire_minimal g wire then (wire,g)
  else minimise_wire' wire g
end
val minimise_wire_anon = snd oo minimise_wire

fun is_wire_normalised g (s,t,wvs,_) =
  if is_node_vertex g s andalso is_node_vertex g t then
    V.NSet.cardinality wvs = 2
  else
    V.NSet.is_empty wvs

fun normalise_wire' (wire as (s,t,wvs,es)) g =
  if is_wire_vertex g s orelse is_wire_vertex g t then
    (* add no extra wire-verts on boundary wires, bare wires, or circles *)
    minimise_wire' wire g
  else
    (* add two extra wire verts on internal wires *)
    let
      (* bboxes to add the newly-created wv's to *)
      val bboxes = B.NSet.empty
        |> B.NSet.union_merge (get_bboxes_containing_vertex g s)
        |> B.NSet.union_merge (get_bboxes_containing_vertex g t)
        |> (case V.NSet.get_local_bot wvs
              of SOME wv => B.NSet.union_merge (get_bboxes_containing_vertex g wv)
               | NONE    => I)
      val dir_and_data = get_edge_dir_and_data g ((the o E.NSet.get_local_bot) es)
      val g      = g |> delete_wire wire
      val (wv1,g) = g |> add_vertex WVert
      val (wv2,g) = g |> add_vertex WVert
      val g      = g |> B.NSet.fold (fn b => add_vertex_to_bbox b wv1 o 
                                             add_vertex_to_bbox b wv2) bboxes
      val (e1,g) = g |> add_edge dir_and_data s wv1
      val (e2,g) = g |> add_edge dir_and_data wv1 wv2
      val (e3,g) = g |> add_edge dir_and_data wv2 t
    in
      ((s,t,V.NSet.of_list [wv1,wv2],E.NSet.of_list [e1,e2,e3]), g)
    end
fun normalise_wire wire g = let
  val _ = assert_wire_coherent g wire
in
  if is_wire_normalised g wire
  then (wire,g)
  else normalise_wire' wire g
end
val normalise_wire_anon = snd oo normalise_wire

fun minimise g = g |> fold minimise_wire_anon (get_wire_list g)
fun normalise g = g |> fold normalise_wire_anon (get_wire_list g)

fun split_edge e g = let
  val (dd, (s,t)) = get_edge_info g e
  val bbs = if is_wire_vertex g s
            then get_bboxes_containing_vertex g s
            else if is_wire_vertex g t
            then get_bboxes_containing_vertex g t
            else B.NSet.union_merge (get_bboxes_containing_vertex g s)
                                    (get_bboxes_containing_vertex g t)
  val g' = delete_edge e g
  val (vn,g') = add_vertex WVert g'
  val vset = V.NSet.single vn
  val g' = (B.NSet.fold (fn b => add_vertices_to_bbox b vset) bbs g')
  val (e1,g') = add_edge dd s vn g'
  val (e2,g') = add_edge dd vn t g'
in
  ((e1,vn,e2),g')
end

(********************************)
(****** PLUGGING FUNCTIONS ******)
(********************************)

fun get_plugging g v1 v2 =
  case (get_only_edge g v1,get_only_edge g v2)
    of (SOME e1,SOME e2) =>
          (case (get_edge_dir_and_data g e1,get_edge_dir_and_data g e2)
             of ((Undirected,d1),(Undirected,d2)) =>
                 if Data.edata_eq (d1,d2)
                 then SOME ((Undirected,d1),(v1,v2))
                 else NONE
             | ((Directed,d1),(Directed,d2)) =>
                 if not (Data.edata_eq (d1,d2))
                 then NONE
                 else if is_input g v1 andalso is_output g v2
                 then SOME ((Directed,d1),(v2,v1))
                 else if is_input g v2 andalso is_output g v1
                 then SOME ((Directed,d1),(v1,v2))
                 else NONE
             | _ => NONE)
     | _ => NONE

val are_pluggable = is_some ooo get_plugging
fun plug v1 v2 g =
  case get_plugging g v1 v2 of
       NONE => raise plugging_exp (g,v1,v2)
     | SOME (ed,(v1',v2')) => add_edge ed v1' v2' g;
val plug_anon = snd ooo plug
fun plug_and_minimise g v1 v2 = let
  val (e,g') = plug g v1 v2
in
  minimise_wire (get_wire_containing_edge g' e) g'
end
val plug_and_minimise_anon = snd ooo plug_and_minimise
fun plug_and_normalise g v1 v2 = let
  val (e,g') = plug g v1 v2
in
  normalise_wire (get_wire_containing_edge g' e) g'
end
val plug_and_normalise_anon = snd ooo plug_and_normalise


fun find_open_subgraph g vset = let
  fun absorb new accum =
    if V.NSet.is_empty new then accum
    else
      let
        val new' =
          V.NSet.fold (fn v => (
            if is_wire_vertex g v andalso not (V.NSet.contains accum v)
            then V.NSet.add v
            else I
          )) (get_adj_vertices_to_set g new) V.NSet.empty
      in absorb new' (V.NSet.union_merge new' accum)
      end
in absorb vset vset
end

fun get_open_subgraph g vset = let
  val retained = find_open_subgraph g vset
  val removed = V.NSet.subtract (get_vertices g) retained
in
  V.NSet.fold delete_vertex removed g
end


(************************************)
(********** BBOX FUNCTIONS **********)
(************************************)

fun add_to_bbox b vs g = let
  val () = if B.NSet.contains (get_bboxes g) b then ()
           else raise no_such_bbox_exp ("add_to_bbox",b,g)
  val () = V.NSet.fold (fn v => K (
             if has_vertex g v then ()
             else raise no_such_vertex_exp ("add_to_bbox",v,g)
           )) vs ()
  val to_add = find_open_subgraph g vs
  fun raise_if_not_has_added_verts bb = let
    val contents = get_vertices_in_bbox g bb
  in
    if V.NSet.sub_set to_add contents
    then true
    else raise bbox_bad_parent_exp (bb,b,g)
  end
  val parents = get_bbox_parents g b
  val _ = B.NSet.forall raise_if_not_has_added_verts parents
in (to_add, g |> add_vertices_to_bbox b to_add)
end

val add_to_bbox_anon = snd ooo add_to_bbox

fun remove_from_bbox b vs g =
  if not (B.NSet.contains (get_bboxes g) b) then
    raise no_such_bbox_exp ("remove_from_bbox",b,g)
  else
    let
      val old_bvs = get_vertices_in_bbox g b
      val new_bvs = find_open_subgraph g (V.NSet.subtract old_bvs vs)
      val remove_bvs = V.NSet.subtract old_bvs new_bvs
      fun raise_if_has_removed_verts bb = let
        val contents = get_vertices_in_bbox g bb
      in
        if V.NSet.nonempty_intersect remove_bvs contents
        then raise bbox_bad_parent_exp (b,bb,g)
        else false
      end
      val children = get_bbox_children g b
      val _ = B.NSet.exists raise_if_has_removed_verts children
    in
      (remove_bvs, g |> remove_vertices_from_bbox b remove_bvs)
    end
val remove_from_bbox_anon = snd ooo remove_from_bbox

fun remove_from_all_bboxes vset g = let
  (* no need to check parents or args *)
  fun remove_from_bbox b vs g =
    let
      val old_bvs = get_vertices_in_bbox g b
      val new_bvs = find_open_subgraph g (V.NSet.subtract old_bvs vs)
      val remove_bvs = V.NSet.subtract old_bvs new_bvs
    in
      g |> remove_vertices_from_bbox b remove_bvs
    end
in
  g |> B.NSet.fold (fn b => remove_from_bbox b vset) (get_bboxes g)
end


fun kill_bbox bb g =
  if not (has_bbox g bb) then raise no_such_bbox_exp ("kill_bbox",bb,g)
  else
    let
      (* depending on naming, we may end up recursing to !-boxes later in the
       * list, and then try to remove them again; hence no checks on recurse *)
      fun kill_bbox' bb g =
        if not (has_bbox g bb) then g
        else
          let
            val g' = B.NSet.fold kill_bbox' (get_bbox_children g bb) g
            val bbox_vs = get_vertices_in_bbox g' bb
          in
            g' |> V.NSet.fold delete_vertex bbox_vs
               |> delete_bbox bb
          end
    in
      kill_bbox' bb g
    end

fun drop_bbox bn g =
  if not (has_bbox g bn) then raise no_such_bbox_exp ("drop_bbox",bn,g)
  else g |> delete_bbox bn

fun merge_bboxes bnset g =
  (case B.NSet.pull_local_top bnset of
     NONE => raise merge_bbox_exp "given empty bbox name set"
   | SOME (bn,bnset2) =>
       let
         val parents = get_bbox_parents g bn
         fun merge_with_bn bn2 g2 = let
           val b2verts = get_vertices_in_bbox g2 bn2
           val bverts = get_vertices_in_bbox g2 bn
           val b2parents = get_bbox_parents g2 bn2
         in
           if not (B.NSet.eq parents b2parents)
           then raise merge_bbox_exp ("parents of "^(B.dest bn)^
                                      " and "^(B.dest bn2)^" are different")
           else if V.NSet.nonempty_intersect bverts b2verts
           then raise merge_bbox_exp "!-boxes are not disjoint"
           else
             g2 |> add_to_bbox_anon bn b2verts
                |> delete_bbox bn2
         end
       in
         (bn, B.NSet.fold merge_with_bn bnset2 g)
       end)

fun expand_bbox bb ((vsub,esub,bsub),g) = let
  val _ = if has_bbox g bb then () else raise no_such_bbox_exp ("expand_bbox", bb, g) 
  val old_children = get_bbox_children g bb
  val old_vertices = get_vertices_in_bbox g bb
  val old_edges = V.NSet.fold (E.NSet.union_merge o get_adj_edges' g) old_vertices E.NSet.empty
  val vsub' = VSub.extend_fresh old_vertices vsub
  val esub' = ESub.extend_fresh old_edges esub
  val bsub' = BSub.extend_fresh old_children bsub

  fun cp_vert v g = let
    val v' = VSub.get vsub' v
  in
    g |> add_named_vertex v' (get_vertex_data g v)
      |> B.NSet.fold
           (fn b => (
             if B.name_eq (bb,b) then I
             else add_vertex_to_bbox (if B.NSet.contains old_children b
                                      then BSub.get bsub' b else b) v'
           ))
           (get_bboxes_containing_vertex g v)
  end

  fun cp_edge e g = let
    val e' = ESub.get esub' e
    val (old_s,old_t) = (get_edge_source g e, get_edge_target g e)
    val s = case VSub.get_opt vsub' old_s of SOME s' => s' | NONE => old_s
    val t = case VSub.get_opt vsub' old_t of SOME t' => t' | NONE => old_t
  in
    g |> add_named_edge e' (get_edge_dir_and_data g e) s t
  end

in ((vsub',esub',bsub'),
    g |> B.NSet.fold (add_named_bbox o BSub.get bsub') old_children
      |> B.NSet.fold (fn b => add_children_to_bbox (BSub.get bsub' b)
                               (BSub.img_of_set bsub' (get_bbox_children g b))) old_children
      |> V.NSet.fold cp_vert old_vertices
      |> E.NSet.fold cp_edge old_edges
   )
end

fun copy_bbox bb ((vsub,esub,bsub),g) = let
  val bsub' = BSub.extend_fresh (B.NSet.single bb) bsub
  val ((vsub',esub',bsub'),g') = expand_bbox bb ((vsub,esub,bsub'),g)
  val new_bb = BSub.get bsub' bb
  val new_children = BSub.img_of_set bsub' (get_bbox_children g bb)
  val new_vertices = VSub.img_of_set vsub' (get_vertices_in_bbox g bb)
in ((vsub',esub',bsub'),
    g' |> add_named_bbox new_bb
       |> add_children_to_bbox new_bb new_children
       |> add_vertices_to_bbox new_bb new_vertices)
end

fun fresh_copy_bbox bb g = let
  val vrn = VSub.mk_from_avoids (get_vertices g)
  val ern = ESub.mk_from_avoids (get_edges g)
  val brn = BSub.mk_from_avoids (get_bboxes g)
  val ((_,_,brn'),g') = copy_bbox bb ((vrn,ern,brn),g)
in copy_bbox bb ((vrn,ern,brn),g)
end

val fresh_copy_bbox_anon = snd oo fresh_copy_bbox

fun fresh_expand_bbox bb g = let
  val vrn = VSub.mk_from_avoids (get_vertices g)
  val ern = ESub.mk_from_avoids (get_edges g)
  val brn = BSub.mk_from_avoids (get_bboxes g)
in expand_bbox bb ((vrn,ern,brn),g)
end

val fresh_expand_bbox_anon = snd oo fresh_expand_bbox

fun expand_bbox_op bb g = let
  val vrn = VSub.mk_from_avoids (get_vertices g)
  val ern = ESub.mk_from_avoids (get_edges g)
  val brn = BSub.mk_from_avoids (get_bboxes g)
  val ((vrn',_,bbox_map),g') = expand_bbox bb ((vrn,ern,brn),g)
  val boundary_map =
    V.NSet.fold 
       (fn v => if is_boundary g v 
                then VSub.add (v,VSub.get vrn' v)
                else I)
       (get_vertices_in_bbox g bb)
       VSub.empty
in (BBExpand { bbox=bb, boundary_map=boundary_map, bbox_map=bbox_map }, g')
end

fun kill_bbox_op bb g = ( BBKill bb, kill_bbox bb g )

fun replay_bbox_op (BBExpand {bbox,boundary_map,bbox_map}) g =
  let
    (* freshen the interior of g w.r.t. boundary_map *)
    val g' = g |> rename_vertices (VSub.empty 
                    |> VSub.extend_fixed (get_boundary g)
                    |> VSub.avoid_set_in_cod (VSub.get_cod_set boundary_map)
                    |> VSub.extend_fresh (get_vertices g))
    val vrn = boundary_map |> VSub.avoid_set_in_cod (get_vertices g')
    val ern = ESub.empty   |> ESub.avoid_set_in_cod (get_edges g')
    val brn = bbox_map
  in snd (expand_bbox bbox ((vrn,ern,brn), g'))
  end
  | replay_bbox_op (BBKill bbox) g = kill_bbox bbox g

fun replay_bbox_ops bb_ops = fold_rev replay_bbox_op bb_ops

fun pretty_edge_desc g e =
  Pretty.block[E.pretty_name e,
               Pretty.str "[",
               V.pretty_name (get_edge_source g e),
               Pretty.str (case get_edge_dir g e of Directed => " -> " | Undirected => "--"),
               V.pretty_name (get_edge_target g e),
               Pretty.str "]"
              ]


fun pretty g =
  Pretty.chunks
    [ Pretty.str "BangGraph{",
      Pretty.block [Pretty.str "  ", (* indent *)
         Pretty.chunks ([
          Pretty.block [Pretty.str "Node-vertices: ", V.NSet.pretty (get_node_vertices g)],
          Pretty.block [Pretty.str "Wire-vertices: ", V.NSet.pretty (get_wire_vertices g)],
          Pretty.block [Pretty.str "Vertex data: ", VTab.pretty pretty_vdata (get_vdata g)],
          Pretty.block [Pretty.str "Edges: ", Map.pretty "" (pretty_edge_desc g) (pretty_edata o snd) (get_edata g)],
          Pretty.block [Pretty.str "Bang-boxes: ", B.NSet.pretty (get_bboxes g)],
          Pretty.block [Pretty.str "BB contains: ", BVRel.pretty (get_bbox_rel g)],
          Pretty.block [Pretty.str "BB children: ", BBRel.pretty (get_bbox_child_rel g)]]
         )],
      Pretty.str "}"
    ]

val print = Pretty.writeln o pretty

end


