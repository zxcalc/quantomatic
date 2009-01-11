(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Ross' Quantum Graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graphs with Boundary *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature BANG_GRAPH = 
sig
include BGRAPH;
structure BBox : SSTR_NAMES (* !(Bang)-Box *)
structure VtoBoxMap : NAME_MAP (* !-box-name to vertices *)
sharing VtoBoxMap.Cod = BBox;
sharing VtoBoxMap.Dom = Vertex;

val get_bboxes : T -> VtoBoxMap.T
val get_bbox : T -> BBox.name -> Vertex.NSet.T
val get_unboxed : T -> Vertex.NSet.T

val lookup_box_of : T -> Vertex.name -> BBox.name option
val is_boxed : T -> Vertex.name -> bool

val new_box : BBox.name -> T -> BBox.name * T
val move_to_box : BBox.name -> Vertex.NSet.T -> T -> T
val move_to_unbox : Vertex.NSet.T -> T -> T
val copy1_box : BBox.name -> T -> (BBox.name (* new box name *)
                                   * (Vertex.renaming (* how vnames were copied *)
                                      * Edge.renaming)) (* ename copying *)
                                  * T (* new graph *)
val kill_box : BBox.name -> T -> T
val drop_box : BBox.name -> T -> T
val merge_boxes : BBox.name -> BBox.name -> T -> T

end;

(* bang box names *)
structure BBox : SSTR_NAMES = SStrName;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* A  Graph With Boundary *)
functor BangGraphFun(
        structure BVertex : BVERTEX
        structure Edge : EDGE
        type data 
        val empty_data : data
) : BANG_GRAPH
= struct

structure V = BVertex;
structure E = Edge;

structure BBox : SSTR_NAMES = BBox;
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
  structure Graph : BGRAPH = BGraphFun(
    structure BVertex = V 
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
        |> update_bboxes (the o VtoBoxMap.try_rename1_dom n1 n2)
        |> update_unboxed (the o V.NSet.try_rename1 n1 n2);

  (* prettifying *)
  fun pretty g = 
      Pretty.chunks 
        [ Pretty.str "BangBoxGraph{",
          Pretty.block 
            [Pretty.str "  ", (* indent *)
             Pretty.chunks 
               [Pretty.block 
                  [Pretty.str "Bang-Boxes: ", VtoBoxMap.pretty (get_bboxes g)],
                Graph.pretty g]],
          Pretty.str "}"
        ];

  val print = Pretty.writeln o pretty;
end; (* BasicBGraph *)

structure BBGraphTools = GraphToolsFun(BasicBBGraph);
open BasicBBGraph;
open BBGraphTools;

exception overlapping_bbox_exp of V.NSet.T * T

(* lookup name of box that v is in, NONE if unboxed *)
fun lookup_box_of g v = VtoBoxMap.lookup_domf (get_bboxes g) v;
fun is_boxed g v = V.NSet.contains (VtoBoxMap.get_domset (get_bboxes g)) v;

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

(* FIXME: merge_by vertices respect !-boxes *)
(* FIXME: subgraph_of_selection respect !-boxes *)
(* FIXME: other stuff respect !-boxes ??? *)

fun copy1_box bn g = 
    let val bbox_vs = get_bbox g bn
        (* subgraph of selection gives back something without bang-boxes *)
        val (boundary_vs, g2) = subgraph_of_selection' bbox_vs g;
        val avoid_vs = Vertex.NSet.subtract (get_vnames g) boundary_vs;
        val ((vrn,ern),g3) = 
            rename (Vertex.Rnm.mk_from_avoidset avoid_vs,
                    Edge.Rnm.mk_from_avoidset (get_enames g)) 
                   g2
        val bbox2_vs = Vertex.Rnm.get_newnames vrn;
        val bboxes = get_bboxes g
        val bn2 = BBox.NSet.new (VtoBoxMap.get_codset bboxes) bn
        val bboxes2 = VtoBoxMap.add_to_cod bn2 bbox2_vs bboxes;
    in
      ((bn2, (vrn,ern)), 
       g |> merge_by_vertices g3
         |> set_bboxes bboxes2)
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

fun merge_boxes bn1 bn2 g = 
    let 
      val b1vs = get_bbox g bn1
    in g |> move_to_box bn1 b1vs end; 

end; (* BangGraphFun *)
