(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Ross' Quantum Graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graphs with Boundary *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature BANG_GRAPH = 
sig
include BGRAPH;
structure BBox : NAMES (* !(Bang)-Box *)
structure BBoxRel : BINREL (* !-box-name to vertices *)
sharing BBoxRel.Dom = BBox;
sharing BBoxRel.Cod = Vertex;

val get_bboxes : T -> BBoxRel.T
val get_bbox : T -> BBox.name -> Vertex.NSet.T
val get_unboxed : T -> Vertex.NSet.T

val new_box : BBox.name -> T -> BBox.name * T
val add_to_box : BBox.name -> Vertex.NSet.T -> T -> T
val copy1_box : BBox.name -> T -> (BBox.name (* new box name *)
                                   * (Vertex.renaming (* how vnames were copied *)
                                      * Edge.renaming)) (* ename copying *)
                                  * T (* new graph *)
val kill_box : BBox.name -> T -> T
val drop_box : BBox.name -> T -> T
end;

structure BVertex : BVERTEX = BVertexFun(structure IVertex = StrVertex);
structure Edge : EDGE = StrEdge;
type data = unit;
val empty_data = ();

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

val foo = V.NSet.empty : V.NSet.T;

structure BBox : NAMES = StrName;
structure BBoxRel = BinRelFun(structure Dom = BBox and Cod = V);

val foo : BBox.name -> V.NSet.T -> BBoxRel.T -> BBoxRel.T
  = BBoxRel.add_to_dom;

(* to avoid using clobbered data and empty graph from the opened Basic Graph *)
type data2 = data;
val empty_data2 = empty_data;

(* we add bbox to Graph data, this is hidden *)
datatype hdata = 
         HData of 
         { bboxes : BBoxRel.T,
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
val empty_hdata = HData {bboxes = BBoxRel.empty,
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
  val get_bboxes : T -> BBoxRel.T = get_bboxes_of_hdata o Graph.get_data;
  val update_unboxed = Graph.update_data o update_unboxed_of_hdata;
  val update_bboxes = Graph.update_data o update_bboxes_of_hdata;
  val set_unboxed = update_unboxed o K;
  val add_to_unboxed = update_unboxed o V.NSet.add;
  val del_from_unboxed = update_unboxed o V.NSet.delete;
  val set_bboxes : BBoxRel.T -> T -> T = update_bboxes o K;
  val get_bbox = BBox.NTab.get o BBoxRel.get_domtab o get_bboxes;

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
        |> update_bboxes (the o BBoxRel.try_rename1_cod n1 n2)
        |> update_unboxed (the o V.NSet.try_rename1 n1 n2);

  (* prettifying *)
  fun pretty g = 
      Pretty.chunks 
        [ Pretty.str "BangBoxGraph{",
          Pretty.block 
            [Pretty.str "  ", (* indent *)
             Pretty.chunks 
               [Pretty.block 
                  [Pretty.str "Bang-Boxes: ", BBoxRel.pretty (get_bboxes g)],
                Graph.pretty g]],
          Pretty.str "}"
        ];

  val print = Pretty.writeln o pretty;
end; (* BasicBGraph *)

structure BBGraphTools = GraphToolsFun(BasicBBGraph);
open BasicBBGraph;
open BBGraphTools;

exception overlapping_bbox_exp of V.NSet.T * T

(* make a new empty !-box *)
fun new_box bn g = 
    let
      val bboxes = (get_bboxes g);
      val bn2 = BBox.NSet.new (BBoxRel.get_domset bboxes) bn;
      val bboxes2 : BBoxRel.T = 
          BBoxRel.add_to_dom bn2 V.NSet.empty bboxes;
    in (bn2, g |> set_bboxes bboxes2) end;

(* add to a !-box; makes new box, if name doesn't alreayd exist;
IMPROVE: raise exception if doesn't already exist? *)
fun add_to_box bn vs g = 
    let val unboxed = (get_unboxed g)
    in
      if V.NSet.sub_set vs unboxed then 
        let val bboxes2 = BBoxRel.add_to_dom bn vs (get_bboxes g)
        in g |> set_bboxes bboxes2
             |> set_unboxed (Vertex.NSet.subtract unboxed vs)
        end
      else raise overlapping_bbox_exp (vs,g)
    end;

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
        val bn2 = BBox.NSet.new (BBoxRel.get_domset bboxes) bn
        val bboxes2 = BBoxRel.add_to_dom bn2 bbox2_vs bboxes;
    in
      ((bn2, (vrn,ern)), 
       g |> merge_by_vertices g3
         |> set_bboxes bboxes2)
    end;

fun kill_box bn g = 
    let val bbox_vs = get_bbox g bn
    in g |> Vertex.NSet.fold delete_vertex bbox_vs
         |> update_bboxes (BBoxRel.del_dom bn)
    end;

fun drop_box bn g = 
    let val bbox_vs = get_bbox g bn 
    in g |> update_bboxes (BBoxRel.del_dom bn)
         |> update_unboxed (Vertex.NSet.union_merge bbox_vs)
    end;


end; (* BangGraphFun *)
