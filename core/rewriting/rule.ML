(**
 * Rules for rewriting graphs
 *
 * We store the LHS and the RHS, making sure they are consistent
 * (share a boundary, share !-boxes, etc).  We also ensure that
 * these are the *only* shared vertex or edge names between the
 * graphs (FIXME: why?).
 *)

signature RULE_SHARING =
sig
  structure Graph : OGRAPH_SHARING
  type T
end

signature RULE
= sig
  structure Graph : OGRAPH
  
  type T (* rule *)

  structure Sharing : RULE_SHARING
  sharing Sharing.Graph = Graph.SharingOGraph
  sharing type Sharing.T = T

  exception bad_rule_exp of string * Graph.T * Graph.T;
  val mk : (Graph.T * Graph.T) -> T;
  (* returns the renamings applied to the RHS *)
  val mk_with_renamings : (Graph.T * Graph.T) -> T * (V.Rnm.T * E.Rnm.T);
  
  val try_rename1_ename : E.name -> E.name -> T -> T option
  val try_rename1_vname : V.name -> V.name -> T -> T option
  val try_rename1_xname : X.name -> X.name -> T -> T option
  
  structure XCRnm : COMPOUND_RENAMING
  where type obj = T and type renaming = X.Rnm.T
  structure VertexCRnm : COMPOUND_RENAMING
  where type obj = T and type renaming = V.Rnm.T
  structure EdgeCRnm : COMPOUND_RENAMING
  where type obj = T and type renaming = E.Rnm.T

  val rename : (X.Rnm.T * V.Rnm.T * E.Rnm.T) -> T
            -> (X.Rnm.T * V.Rnm.T * E.Rnm.T) * T

  (* generate a boundary graph, including bboxes? *)
  val get_boundary_vertices : T -> V.NSet.T

  val get_lhs : T -> Graph.T
  val get_rhs : T -> Graph.T
  
  val get_xnames : T -> X.NSet.T
  val get_vnames : T -> V.NSet.T
  val get_enames : T -> E.NSet.T

  (* swaps lhs and rhs *)
  val symmetric : T -> T
  
  (* for pretty printing *)
  val pretty : T -> Pretty.T
  val print : T -> unit
end;

signature BANG_GRAPH_RULE
= sig
  include RULE;

  structure BangGraph : BANG_GRAPH
  sharing BangGraph.SharingOGraph = Graph.SharingOGraph

  structure BBoxCRnm : COMPOUND_RENAMING
  where type obj = T and type renaming = B.Rnm.T

  (* FIXME: better name? *)
  val rename4 : (X.Rnm.T * V.Rnm.T * E.Rnm.T * B.Rnm.T)
             -> T
             -> (X.Rnm.T * V.Rnm.T * E.Rnm.T * B.Rnm.T) * T

  val try_rename1_bbox : B.name -> B.name -> T -> T option
  val get_bnames : T -> B.NSet.T
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *)
functor BangGraphRule(Graph : BANG_GRAPH) : BANG_GRAPH_RULE
= struct

structure Graph = Graph;
structure BangGraph = Graph;

datatype T = Rule of { lhs : Graph.T,
                       rhs : Graph.T,
                       boundary_vertices : V.NSet.T
                     };

exception bad_rule_exp of string * Graph.T * Graph.T;

  structure G = Graph;

  type boundary = {
    inputs : G.EData.data V.NTab.T,
    outputs : G.EData.data V.NTab.T,
    undir_bounds : G.EData.data V.NTab.T,
    isolated : V.NSet.T
  };

  fun find_graph_boundary g =
  let
    fun build_boundary (_,((G.OVData.NVert _),_)) recd = recd
      | build_boundary (vn,(G.OVData.WVert,(ins,outs)))
                       (recd as { inputs = inp,
                                  outputs = outp,
                                  undir_bounds = undir,
                                  isolated = isol}) =
          if E.NSet.is_empty ins
          then
            case E.NSet.tryget_singleton outs
              of SOME e =>
                  (case G.get_edge g e
                     of ((G.Directed,ed),_) =>
                          { inputs = V.NTab.ins (vn,ed) inp,
                            outputs = outp,
                            undir_bounds = undir,
                            isolated = isol }
                      | ((G.UnDirected,ed),_) =>
                          { inputs = inp,
                            outputs = outp,
                            undir_bounds = V.NTab.ins (vn,ed) undir,
                            isolated = isol })
               | NONE =>
                   if E.NSet.is_empty outs then
                    { inputs = inp, outputs = outp,
                      undir_bounds = undir, isolated = V.NSet.add vn isol }
                   else
                     recd
          else
            if E.NSet.is_empty outs
            then
              case E.NSet.tryget_singleton ins
                of SOME e =>
                  (case G.get_edge g e
                     of ((G.Directed,ed),_) =>
                          { inputs = inp,
                            outputs = V.NTab.ins (vn,ed) outp,
                            undir_bounds = undir,
                            isolated = isol }
                      | ((G.UnDirected,ed),_) =>
                          { inputs = inp,
                            outputs = outp,
                            undir_bounds = V.NTab.ins (vn,ed) undir,
                            isolated = isol })
                 | NONE => recd
            else recd;
  in
    V.NTab.fold build_boundary (G.get_vertices g)
      {
        inputs = V.NTab.empty,
        outputs = V.NTab.empty,
        undir_bounds = V.NTab.empty,
        isolated = V.NSet.empty
      }
  end;

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

      (* ideally, a more efficient version of this would be in NAME_TAB *)
      fun vtab_eq data_eq tab1 tab2 =
        let
          fun cmp_tab2 (nm,d) sofar =
            sofar andalso data_eq (d,(V.NTab.get tab2 nm))
        in
          V.NSet.eq (V.NTab.get_nameset tab1) (V.NTab.get_nameset tab2)
          andalso
          V.NTab.fold cmp_tab2 tab1 true
        end;

      fun test_all test_fn =
        (test_fn (V.NTab.get_nameset (#inputs lhs_bnd))
                 (V.NTab.get_nameset (#inputs rhs_bnd))) andalso
        (test_fn (V.NTab.get_nameset (#outputs lhs_bnd))
                 (V.NTab.get_nameset (#outputs rhs_bnd))) andalso
        (test_fn (V.NTab.get_nameset (#undir_bounds lhs_bnd))
                 (V.NTab.get_nameset (#undir_bounds rhs_bnd))) andalso
        (test_fn (#isolated lhs_bnd) (#isolated rhs_bnd));
      fun has_same_bangbox vn sofar = sofar andalso
           B.NSet.eq (Graph.get_bboxes_of lhs vn) (Graph.get_bboxes_of rhs vn)
      fun compare_bboxes lvs _ =
        (V.NSet.fold (has_same_bangbox) lvs true)
      fun check_bb_parent bbox sofar = sofar andalso
        case (Graph.lookup_bbox_parent lhs bbox,Graph.lookup_bbox_parent rhs bbox)
          of (NONE,NONE) => true
           | (SOME bl,SOME br) => B.name_eq (bl,br)
           | _ => false
    in
      if (V.NSet.is_empty (#isolated lhs_bnd)) andalso
         (V.NSet.is_empty (#isolated rhs_bnd)) andalso
         (vtab_eq G.EData.data_eq (#inputs lhs_bnd) (#inputs rhs_bnd)) andalso
         (vtab_eq G.EData.data_eq (#outputs lhs_bnd) (#outputs rhs_bnd)) andalso
         (vtab_eq G.EData.data_eq (#undir_bounds lhs_bnd) (#undir_bounds rhs_bnd)) andalso
         (B.NSet.eq (Graph.get_bboxes lhs) (Graph.get_bboxes rhs)) andalso
         (B.NSet.fold check_bb_parent (Graph.get_bboxes lhs) true) andalso
         test_all compare_bboxes
      then
        (V.NTab.get_nameset (#inputs lhs_bnd))
          |> V.NSet.union_merge (V.NTab.get_nameset (#outputs lhs_bnd))
          |> V.NSet.union_merge (V.NTab.get_nameset (#undir_bounds lhs_bnd))
      else
        raise bad_rule_exp ("mk: Left and right hand side boundaries are different", lhs, rhs)
   end;

fun mk_with_renamings (lhs,rhs) =
    let
      val lhsbndry : V.NSet.T = get_rule_bounds lhs rhs
      (* rename rhs; sharing bvertex names with lhs,
         all else must be distinct, but do minimal amount of renaming *)
      val ignorevnames = (* ignore boundary and also fresh rhs vnames *)
          V.NSet.union_merge
            lhsbndry
            (V.NSet.subtract (Graph.get_vnames rhs)
                                  (Graph.get_vnames lhs));
      (* ignore enames that are already fresh w.r.t. lhs *)
      val ignoreenames = E.NSet.subtract (Graph.get_enames rhs)
                          (Graph.get_enames lhs);
      val vrn = V.mk_renaming ignorevnames
                  (V.NSet.union_merge ignorevnames
                    (Graph.get_vnames lhs))
                  V.NTab.empty
      val ern = E.mk_renaming ignoreenames
                  (E.NSet.union_merge ignoreenames (Graph.get_enames lhs))
                  E.NTab.empty
      val xrn = X.Rnm.empty;
      val ((_,vrn,ern), rhs') = Graph.rename (xrn,vrn,ern) rhs
    in (Rule { lhs = lhs, rhs = rhs', boundary_vertices = lhsbndry },
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
fun symmetric r = mk (get_lhs r, get_rhs r);

fun get_xnames rule =
    X.NSet.union_merge
      (Graph.get_xnames (get_lhs rule))
      (Graph.get_xnames (get_rhs rule));
fun get_vnames rule =
    V.NSet.union_merge
      (Graph.get_vnames (get_lhs rule))
      (Graph.get_vnames (get_rhs rule));
fun get_enames rule =
    E.NSet.union_merge
      (Graph.get_enames (get_lhs rule))
      (Graph.get_enames (get_rhs rule));
fun get_bnames rule =
    B.NSet.union_merge
      (Graph.get_bboxes (get_lhs rule))
      (Graph.get_bboxes (get_rhs rule));

(* *)
fun try_rename1_xname n1 n2 rule =
    (SOME
      (rule |> (update_lhs (untryify (Graph.try_rename1_xname n1 n2)))
            |> (update_rhs (untryify (Graph.try_rename1_xname n1 n2)))))
    handle tryify_failed () => NONE;
(* FIXME: make sure disjoint names property is preserved *)
fun try_rename1_vname n1 n2 rule =
    (SOME
      (rule |> (update_lhs (untryify (Graph.try_rename1_vname n1 n2)))
            |> (update_rhs (untryify (Graph.try_rename1_vname n1 n2)))
            |> (update_boundary_vertices (untryify (V.NSet.try_rename1 n1 n2)))))
    handle tryify_failed () => NONE;
(* FIXME: make sure disjoint names property is preserved *)
fun try_rename1_ename n1 n2 rule =
    (SOME
      (rule |> (update_lhs (untryify (Graph.try_rename1_ename n1 n2)))
            |> (update_rhs (untryify (Graph.try_rename1_ename n1 n2)))))
    handle tryify_failed () => NONE;
fun try_rename1_bbox n1 n2 rule =
    (SOME
      (rule |> (update_lhs (untryify (Graph.try_rename1_bbox n1 n2)))
            |> (update_rhs (untryify (Graph.try_rename1_bbox n1 n2)))))
    handle tryify_failed () => NONE;

(* *)
structure XCRnm = CompoundRenamingFun(
  struct
    structure Nm = X;
    type obj = T
    val try_rename1 = try_rename1_xname;
    val get_nameset = get_xnames
  end);
structure VertexCRnm = CompoundRenamingFun(
  struct
    structure Nm = V;
    type obj = T
    val try_rename1 = try_rename1_vname;
    val get_nameset = get_vnames
  end);
structure EdgeCRnm = CompoundRenamingFun(
  struct
    structure Nm = E;
    type obj = T
    val try_rename1 = try_rename1_ename;
    val get_nameset = get_enames
  end);
structure BBoxCRnm = CompoundRenamingFun(
  struct
    structure Nm = B;
    type obj = T
    val try_rename1 = try_rename1_bbox;
    val get_nameset = get_bnames
  end);

(* rename *)
fun rename (xrn,vrn,ern) rule =
    let
      val (xrn,rule) = XCRnm.rename xrn rule
      val (vrn,rule) = VertexCRnm.rename vrn rule
      val (ern,rule) = EdgeCRnm.rename ern rule
    in
      ((xrn,vrn,ern), rule)
    end;
fun rename4 (xrn,vrn,ern,brn) rule =
    let
      val ((xrn,vrn,ern),rule) = rename (xrn,vrn,ern) rule
      val (brn,rule) = BBoxCRnm.rename brn rule
    in
      ((xrn,vrn,ern,brn), rule)
    end;

fun pretty (Rule rep) =
    Pretty.chunks
    [Pretty.block [Pretty.str "Rule: "],
     Pretty.block [Pretty.str "LHS: ", Graph.pretty (#lhs rep)],
     Pretty.block [Pretty.str "RHS: ", Graph.pretty (#rhs rep)]];

val print = Pretty.writeln o pretty;


structure Sharing = struct
  structure Graph = Graph.SharingOGraph;
  type T = T;
end;

end;
