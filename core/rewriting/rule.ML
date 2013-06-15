(**
 * Rules for rewriting graphs
 *
 * We store the LHS and the RHS, making sure they are consistent
 * (share a boundary, share !-boxes, etc).  We also ensure that
 * these are the *only* shared vertex or edge names between the
 * graphs (FIXME: why?).
 *)

signature OGRAPH_RULE_SHARING =
sig
  structure Graph : OGRAPH_SHARING
  type T
end
(*
signature OGRAPH_RULE =
sig
  type T
  structure Graph : OGRAPH

  structure Sharing : OGRAPH_RULE_SHARING
  sharing Sharing.Graph = Graph.SharingOGraph
  sharing type Sharing.T = T

  exception bad_rule_exp of string * Graph.T * Graph.T;
  val mk : (Graph.T * Graph.T) -> T;
  (* returns the renamings applied to the RHS *)
  val mk_with_renamings : (Graph.T * Graph.T) -> T * (VSub.T * ESub.T);
  
  val try_rename1_ename : E.name -> E.name -> T -> T option
  val try_rename1_vname : V.name -> V.name -> T -> T option

  val rename_ograph_rule : (V.Rnm.T * E.Rnm.T) -> T -> (V.Rnm.T * E.Rnm.T) * T

  (* generate a boundary graph, including bboxes? *)
  val get_boundary_vertices : T -> V.NSet.T

  val get_lhs : T -> Graph.T
  val get_rhs : T -> Graph.T
  
  val get_vnames : T -> V.NSet.T
  val get_enames : T -> E.NSet.T

  (* swaps lhs and rhs *)
  val symmetric : T -> T
  
  (* for pretty printing *)
  val pretty : T -> Pretty.T
  val print : T -> unit
end*)

signature BANG_GRAPH_RULE =
sig
  type T
  structure Graph : BANG_GRAPH

  structure Sharing : OGRAPH_RULE_SHARING
  sharing Sharing.Graph = Graph.Sharing
  sharing type Sharing.T = T

  exception bad_rule_exp of string * Graph.T * Graph.T;
  val mk : (Graph.T * Graph.T) -> T;
  (* returns the renamings applied to the RHS *)
  val mk_with_renamings : (Graph.T * Graph.T) -> T * (VSub.T * ESub.T);
  
  (*val try_rename1_ename : E.name -> E.name -> T -> T option
  val try_rename1_vname : V.name -> V.name -> T -> T option*)

  (* generate a boundary graph, including bboxes? *)
  val get_boundary_vertices : T -> V.NSet.T

  val get_lhs : T -> Graph.T
  val get_rhs : T -> Graph.T
  
  val get_vnames : T -> V.NSet.T
  val get_enames : T -> E.NSet.T

  (* swaps lhs and rhs *)
  val symmetric : T -> T
  
  (* for pretty printing *)
  val pretty : T -> Pretty.T
  val print : T -> unit

  (*val rename_bang_graph_rule : (VSub.T * ESub.T * BSub.T) -> T ->
                               (VSub.T * ESub.T * BSub.T) * T

  val try_rename1_bbox : B.name -> B.name -> T -> T option*)
  val get_bnames : T -> B.NSet.T
end


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *)
functor BangGraphRule(Graph : BANG_GRAPH) : BANG_GRAPH_RULE =
struct

structure Graph = Graph

datatype T = Rule of { lhs : Graph.T,
                       rhs : Graph.T,
                       boundary_vertices : V.NSet.T };

exception bad_rule_exp of string * Graph.T * Graph.T;

type boundary = {
  input_tab : Graph.edata VTab.T,
  output_tab : Graph.edata VTab.T,
  undir_bound_tab : Graph.edata VTab.T,
  isolated : V.NSet.T
}

fun update_input_tab f (r : boundary) = {input_tab=f(#input_tab r),output_tab= #output_tab r,undir_bound_tab= #undir_bound_tab r,isolated= #isolated r}
fun update_output_tab f (r : boundary) = {input_tab= #input_tab r,output_tab=f(#output_tab r),undir_bound_tab= #undir_bound_tab r,isolated= #isolated r}
fun update_undir_bound_tab f (r : boundary) = {input_tab= #input_tab r,output_tab= #output_tab r,undir_bound_tab=f(#undir_bound_tab r),isolated= #isolated r}
fun update_isolated f (r : boundary) = {input_tab= #input_tab r,output_tab= #output_tab r,undir_bound_tab= #undir_bound_tab r,isolated=f(#isolated r)}

fun get_input_tab       (r : boundary) = #input_tab r
fun get_output_tab      (r : boundary) = #output_tab r
fun get_undir_bound_tab (r : boundary) = #undir_bound_tab r
fun get_isolated        (r : boundary) = #isolated r

val get_inputs = VTab.get_dom_set o get_input_tab
val get_outputs = VTab.get_dom_set o get_output_tab
val get_undir_bounds = VTab.get_dom_set o get_undir_bound_tab

fun get_full_boundary (bnd : boundary) =
  get_inputs bnd |> V.NSet.union_merge (get_outputs bnd)
                 |> V.NSet.union_merge (get_undir_bounds bnd)
                 |> V.NSet.union_merge (get_isolated bnd)

val empty_boundary : boundary = {
  input_tab = VTab.empty, output_tab = VTab.empty,
  undir_bound_tab = VTab.empty, isolated = V.NSet.empty
}

fun find_graph_boundary g =
let
  fun build_boundary v (bnd : boundary) =
    if Graph.is_node_vertex g v then bnd
    else
      case (E.NSet.tryget_singleton (Graph.get_in_edges g v),
            E.NSet.tryget_singleton (Graph.get_out_edges g v))
        of (SOME _, SOME _) => bnd
         | (SOME e, NONE) =>
             (case Graph.get_edge_dir_and_data g e
                of (Directed,ed) => bnd |> update_output_tab (VTab.add (v,ed))
                 | (Undirected,ed) => bnd |> update_undir_bound_tab (VTab.add (v,ed)))
         | (NONE, SOME e) =>
             (case Graph.get_edge_dir_and_data g e
                of (Directed,ed) => bnd |> update_input_tab (VTab.add (v,ed))
                 | (Undirected,ed) => bnd |> update_undir_bound_tab (VTab.add (v,ed)))
         | (NONE,NONE) => bnd |> update_isolated (V.NSet.add v)
in
  V.NSet.fold build_boundary (Graph.get_vertices g) empty_boundary
end

(* Internal function that makes sure that:
 *   - boundary names are the same
 *   - !-boxes names are the same
 *   - !-boxes in LHS bang the same boundary vertices as in RHS
 * and returns the shared boundary names
 *)
fun get_rule_bounds lhs rhs =
    let
      val lhs_bnd = find_graph_boundary lhs
      val rhs_bnd = find_graph_boundary rhs
      val boundary_vs = get_full_boundary lhs_bnd

      fun has_same_bboxes v sofar = sofar andalso
           B.NSet.eq (Graph.get_bboxes_containing_vertex lhs v)
                     (Graph.get_bboxes_containing_vertex rhs v)
      
      fun has_same_bbox_children bbox sofar = sofar andalso
        B.NSet.eq (Graph.get_bbox_children lhs bbox)
                  (Graph.get_bbox_children rhs bbox)
    in
      if V.NSet.is_empty (get_isolated lhs_bnd) andalso
         V.NSet.is_empty (get_isolated rhs_bnd) andalso
         VTab.tab_eq Graph.edata_eq (get_input_tab lhs_bnd, get_input_tab rhs_bnd) andalso
         VTab.tab_eq Graph.edata_eq (get_output_tab lhs_bnd, get_output_tab rhs_bnd) andalso
         VTab.tab_eq Graph.edata_eq (get_undir_bound_tab lhs_bnd, get_undir_bound_tab rhs_bnd) andalso
         B.NSet.eq (Graph.get_bboxes lhs) (Graph.get_bboxes rhs) andalso
         B.NSet.fold has_same_bbox_children (Graph.get_bboxes lhs) true andalso
         V.NSet.fold has_same_bboxes boundary_vs true
      then boundary_vs
      else
        raise bad_rule_exp ("get_rule_bounds: Left and right hand side boundaries are different", lhs, rhs)
   end;

fun mk_with_renamings (lhs,rhs) =
    let
      val bnd = get_rule_bounds lhs rhs
      val lhs_int = V.NSet.subtract (Graph.get_vertices lhs) bnd
      val rhs_int = V.NSet.subtract (Graph.get_vertices rhs) bnd
      (* rename rhs; sharing bvertex names with lhs,
         all else must be distinct, but do minimal amount of renaming *)
      val vrn = VSub.empty |> VSub.extend_to bnd
                           |> VSub.avoid_set_in_cod lhs_int
                           |> VSub.extend_fresh rhs_int
      val ern = ESub.empty |> ESub.avoid_set_in_cod (Graph.get_edges lhs)
                           |> ESub.extend_fresh (Graph.get_edges rhs)
      val brn = BSub.empty |> BSub.extend_to (Graph.get_bboxes lhs)
      
      val ((vrn,ern,_), rhs') = Graph.rename_bang_graph (vrn,ern,brn) rhs
    in (Rule { lhs = lhs, rhs = rhs', boundary_vertices = bnd },
        (vrn,ern))
    end;
val mk = fst o mk_with_renamings

fun get_lhs (Rule rep) = #lhs rep;
fun get_rhs (Rule rep) = #rhs rep;
fun get_boundary_vertices (Rule rep) = #boundary_vertices rep;

fun update_lhs f (Rule rep) =
    Rule { lhs = f (#lhs rep), rhs = #rhs rep, boundary_vertices = #boundary_vertices rep };
fun update_rhs f (Rule rep) =
    Rule { lhs = #lhs rep, rhs = f (#rhs rep), boundary_vertices = #boundary_vertices rep };
fun update_boundary_vertices f (Rule rep) =
    Rule { lhs = #lhs rep, rhs = #rhs rep, boundary_vertices = f (#boundary_vertices rep) };

(* swap lhs and rhs of a rule *)
fun symmetric r = mk (get_lhs r, get_rhs r)


fun get_vertices rule =
    V.NSet.union_merge
      (Graph.get_vertices (get_lhs rule))
      (Graph.get_vertices (get_rhs rule))
fun get_edges rule =
    E.NSet.union_merge
      (Graph.get_edges (get_lhs rule))
      (Graph.get_edges (get_rhs rule))
fun get_bboxes rule =
    B.NSet.union_merge
      (Graph.get_bboxes (get_lhs rule))
      (Graph.get_bboxes (get_rhs rule))


fun try_rename1_vname n1 n2 rule =
    (SOME
      (rule |> (update_lhs (untryify (Graph.try_rename1_vname n1 n2)))
            |> (update_rhs (untryify (Graph.try_rename1_vname n1 n2)))
            |> (update_boundary_vertices (untryify (V.NSet.try_rename1 n1 n2)))))
    handle tryify_failed () => NONE
(* FIXME: make sure disjoint names property is preserved *)
fun try_rename1_ename n1 n2 rule =
    (SOME
      (rule |> (update_lhs (untryify (Graph.try_rename1_ename n1 n2)))
            |> (update_rhs (untryify (Graph.try_rename1_ename n1 n2)))))
    handle tryify_failed () => NONE
fun try_rename1_bbox n1 n2 rule =
    (SOME
      (rule |> (update_lhs (untryify (Graph.try_rename1_bbox n1 n2)))
            |> (update_rhs (untryify (Graph.try_rename1_bbox n1 n2)))))
    handle tryify_failed () => NONE


(* rename *)
fun rename (xrn,vrn,ern) rule =
    let
      val (xrn,rule) = XCRnm.rename xrn rule
      val (vrn,rule) = VertexCRnm.rename vrn rule
      val (ern,rule) = EdgeCRnm.rename ern rule
    in
      ((xrn,vrn,ern), rule)
    end
fun rename4 (xrn,vrn,ern,brn) rule =
    let
      val ((xrn,vrn,ern),rule) = rename (xrn,vrn,ern) rule
      val (brn,rule) = BBoxCRnm.rename brn rule
    in
      ((xrn,vrn,ern,brn), rule)
    end

fun pretty (Rule rep) =
    Pretty.chunks
    [Pretty.block [Pretty.str "Rule: "],
     Pretty.block [Pretty.str "LHS: ", Graph.pretty (#lhs rep)],
     Pretty.block [Pretty.str "RHS: ", Graph.pretty (#rhs rep)]]

val print = Pretty.writeln o pretty


structure Sharing = struct
  structure Graph = Graph.Sharing
  type T = T
end

end
