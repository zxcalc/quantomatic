(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Graphs with named vertices and edges. *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

signature BASIC_GRAPH_SHARING = 
sig
structure Edge : EDGE_AND_DATA_SHARING;
structure Vertex : NAME_AND_DATA_SHARING;
structure Param : GRAPH_PARAM_SHARING;
sharing Param.Vertex = Vertex
sharing Param.Edge = Edge
type dir_or_undir;
type T;
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* Basic (Minimal) interface for Graphs *)
signature BASIC_GRAPH = 
sig

structure Vertex : VERTEX
structure Edge : EDGE
structure Param : GRAPH_PARAM

(* each edge is either directed, or bi-directed: this is the family of graphs
   with these edges in both directions. *)
datatype dir_or_undir = Directed | UnDirected;

type T; (* a graph *)

type name_deps (* dependencies between vertex, edge and data names *)

structure SharingBasicGraph : BASIC_GRAPH_SHARING
sharing SharingBasicGraph.Edge = Edge.EdgeSharingWithData;
sharing SharingBasicGraph.Vertex = Vertex.SharingWithData;
sharing SharingBasicGraph.Param = Param.Sharing;
sharing type SharingBasicGraph.T = T;
sharing type SharingBasicGraph.dir_or_undir = dir_or_undir;

exception bug_exp of string;

exception no_such_vname_exp of string * Vertex.name * T;
exception vname_already_exists_exp of Vertex.name * T;

exception no_such_ename_exp of string * Edge.name * T;
exception ename_already_exists_exp of Edge.name * T;

(* Making Graphs *)
val empty : T
(* adding a vertex gives back unique name for it and the new graph *)
(* Vertex.name = suggested name *)
val add_named_vertex : Vertex.name -> Vertex.data -> T -> Vertex.name * T 

(* Vertex.names must already exist, else raises: no_such_vname_exp *)
val add_named_edge : Edge.name -> (dir_or_undir * Edge.data) 
    -> Vertex.name -> Vertex.name -> T -> Edge.name * T 

(* delete raises an exception if there is no such edge/vertex *)
val delete_edge : Edge.name -> T -> T
val delete_vertex : Vertex.name -> T -> T

(* replace all occurances of old name (1st arg) with new name (2nd arg) *)
(*  2nd name must not already exist and 1st must already exist, 
    else an exception is raised *)
val rename_vname : Vertex.name -> Vertex.name -> T -> T 
val rename_ename : Edge.name -> Edge.name -> T -> T 

(* update a data of a vertex or edge *)
val update_vertex_data : (Vertex.data -> Vertex.data) -> Vertex.name -> T -> T
val update_edge_data : (Edge.data -> Edge.data) -> Edge.name -> T -> T

(* getting graph internal representation... *)
val get_vertices : T 
                -> (Vertex.data 
                    * (Edge.NSet.T (* edges coming into this vertex *)
                       * Edge.NSet.T (* edges leaving this vertex *)
                       ))
                   Vertex.NTab.T;
val get_edges : T 
                -> ((dir_or_undir * Edge.data) 
                    * (Vertex.name (* from this vertex *)
                       * Vertex.name)) (* to this vertex *)
                   Edge.NTab.T
val get_Param : T -> name_deps

(* updating graph internal representation... *)
val update_vertices : ((Vertex.data * (Edge.NSet.T * Edge.NSet.T)) 
                         Vertex.NTab.T 
                         -> (Vertex.data * (Edge.NSet.T * Edge.NSet.T)) 
                              Vertex.NTab.T) -> T -> T
val update_edges : (((dir_or_undir * Edge.data) 
                      * (Vertex.name * Vertex.name)) Edge.NTab.T
                    -> ((dir_or_undir * Edge.data) 
                        * (Vertex.name * Vertex.name)) Edge.NTab.T)
                   -> T -> T
val update_Param : (name_deps -> name_deps) -> T -> T


(* pretty printing *)
val pretty : T -> Pretty.T
val print : T -> unit


(* extra data for graphs, for extending the datatype cos ML can't do
it properly from the outside; this is basically a form of record
extensibility, allowing hidden types, but will doing the lazy
extension of data -- allows OOP style extension of signature/structure
by refining data. *)
type data
val get_data : T -> data;
val update_data : (data -> data) -> T -> T

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Basic Graphs with strings at edges and vertices *)
functor BasicGraphFun(
  structure Param : GRAPH_PARAM
  type data (* graph additional extensible/hidden data *)
  val empty_data : data
) 
: BASIC_GRAPH
= struct

structure Vertex = Param.Vertex;
structure Edge = Param.Edge;
structure Param = Param;

exception bug_exp of string;

(* extra data, allows extra data to be added later in decendent
signatures/structures; this provides a style of OOP for normal
structured ML. *)
type data = data;
type name_deps = Param.T;

(* is the edge representing a singtle graph with the directed edge, or is it
 representing the family of graphs with this edge in both directions. *)
datatype dir_or_undir = Directed | UnDirected;

(* a graph *)
datatype T = Graph of 
         {
          vertices : (Vertex.data 
                   * (Edge.NSet.T (* edges coming into this vertex *)
                      * Edge.NSet.T)) (* edges leaving this vertex *)
                    Vertex.NTab.T,
          edges : ((dir_or_undir * Edge.data)
                   * (Vertex.name (* from this vertex *)
                      * Vertex.name)) (* to this vertex *)
                    Edge.NTab.T,  
          (* dependencies between names in data and other graph things *)
          name_deps : Param.T,
          data : data (* global data about a graph *)
         };

(* *)
structure SharingBasicGraph = struct
  structure Vertex = Vertex.SharingWithData;
  structure Edge = Edge.EdgeSharingWithData;
  structure Param = Param.Sharing;
  type T = T;
  type dir_or_undir = dir_or_undir;
end;

(* internal data stuff *)
fun get_data (Graph rep) = #data rep;
val empty_data = empty_data;
fun update_data f (Graph rep) = 
    Graph { vertices = #vertices rep, edges = #edges rep,
            name_deps = #name_deps rep, data = f (#data rep)};

(* basic graph stuff... *)
val empty = Graph { vertices = Vertex.NTab.empty, 
                    edges = Edge.NTab.empty,
                    name_deps = Param.empty,
                    data = empty_data};
fun update_vertices f (Graph rep) = 
    Graph { vertices = f (#vertices rep), edges = #edges rep, 
            name_deps = #name_deps rep, data = #data rep };
fun update_edges f (Graph rep) = 
    Graph { vertices = #vertices rep, edges = f (#edges rep),
            name_deps = #name_deps rep, data = #data rep};
fun update_Param f (Graph rep) = 
    Graph { vertices = #vertices rep, edges = #edges rep,
            name_deps = f(#name_deps rep), data = #data rep};
val set_vertices = update_vertices o K;
val set_edges = update_edges o K;
val set_Param = update_Param o K;

fun get_vertices (Graph rep) = #vertices rep;
fun get_edges (Graph rep) = #edges rep;
fun get_Param (Graph rep) = #name_deps rep;

val get_vertex_list = Vertex.NTab.list_of o get_vertices;
val get_edge_list = Edge.NTab.list_of o get_edges;
val get_vertex_nset = Vertex.NTab.get_nameset o get_vertices;
val get_edge_nset = Edge.NTab.get_nameset o get_edges;

(* implicit: g n *)
val lookup_vertex = Vertex.NTab.lookup o get_vertices
val lookup_edge = Edge.NTab.lookup o get_edges

exception no_such_vname_exp of string * Vertex.name * T;
exception vname_already_exists_exp of Vertex.name * T;
exception no_such_ename_exp of string * Edge.name * T;
exception ename_already_exists_exp of Edge.name * T;

fun get_vertex g n = 
    case lookup_vertex g n 
     of NONE => raise no_such_vname_exp ("get_vertex", n, g)
      | SOME x => x;
fun get_edge g en = 
    case lookup_edge g en 
     of NONE => raise no_such_ename_exp ("get_edge", en, g)
      | SOME x => x;

(* implicit arg: g *)
fun update_vertex_data f n = 
    update_vertices (Vertex.NTab.map_entry (fn (nd,ios) => (f nd,ios)) n);
fun update_vertex_ins f n = 
    update_vertices (Vertex.NTab.map_entry 
                       (fn (nd,(ins,outs)) => (nd,(f ins, outs))) n);
fun update_vertex_outs f n = 
    update_vertices (Vertex.NTab.map_entry 
                       (fn (nd,(ins,outs)) => (nd,(ins, f outs))) n);
fun update_edge f en = 
    update_edges (Edge.NTab.map_entry f en);
fun update_edge_data f = 
    update_edge (fn ((bd,e),fromto) => ((bd,f e),fromto));
fun update_edge_dir_or_undir f = 
    update_edge (fn ((bd,e),fromto) => ((f bd,e),fromto));

(* adding a vertex gives back unique name for it and the new graph *)
fun add_named_vertex n nd g =
    let val (n2,vertices2) = 
            Vertex.NTab.add (n,(nd,(Edge.NSet.empty,Edge.NSet.empty))) 
                            (get_vertices g)
    in (n2, set_vertices vertices2 g) end;

(* implicit arg: g *)
fun new_named_vertex n nd =
    update_vertices 
      (Vertex.NTab.ins (n,(nd,(Edge.NSet.empty,Edge.NSet.empty))));

(* implicit args: g n *)
val has_vname = Vertex.NTab.contains_name o get_vertices;
val has_ename = Edge.NTab.contains_name o get_edges;

fun add_named_edge en e n1 n2 g = 
    let 
      (* make sure the vertices exist *) 
      val _ = if has_vname g n1 then
                if has_vname g n2 then () 
                else raise no_such_vname_exp ("add_named_edge", n2, g)
              else raise no_such_vname_exp ("add_named_edge", n1, g);
      (* add the new egde *)
      val (en2,edges2) = Edge.NTab.add (en,(e,(n1,n2))) (get_edges g)

      (* update the vertices: add edge name as appropriate to ins and outs *)
      val vertices2 = 
          (get_vertices g)
            |> Vertex.NTab.map_entry 
						(fn (n,(ins,outs)) => (n,(ins,Edge.NSet.ins_fresh en2 outs))) n1
            |> Vertex.NTab.map_entry 
						(fn (n,(ins,outs)) => (n,(Edge.NSet.ins_fresh en2 ins,outs))) n2
      (* give back new edge name and new graph *)
    in (en2, g |> set_vertices vertices2
               |> set_edges edges2) end;

(* implicit args: en g *)
(* *)
fun delete_edge en g = 
    let 
      val edges = get_edges g
      val (ed,(fromvn,tovn)) =
          case Edge.NTab.lookup edges en of 
            NONE => raise no_such_ename_exp ("delete_edge", en, g)
          | SOME x => x;

      val edges2 = edges |> Edge.NTab.delete en

      (* delete all edges going into or out of this vertex. *)
      val vertices2 = 
          (get_vertices g)
            |> Vertex.NTab.map_entry (fn (d,(ins,outs)) => 
                                         (d,(ins,Edge.NSet.delete en outs))) fromvn
            |> Vertex.NTab.map_entry (fn (d,(ins,outs)) => 
                                         (d,(Edge.NSet.delete en ins,outs))) tovn;

      (* give back new edge name and new graph *)
    in g |> set_vertices vertices2
         |> set_edges edges2 end;



(* deleting vertex also removes all it's edges. *)
fun delete_vertex n g = 
    let 
      val vertices = get_vertices g
      val (nd,(ins,outs)) =
          case Vertex.NTab.lookup vertices n of 
            NONE => raise no_such_vname_exp ("delete_vertex", n, g)
          | SOME x => x;

      (* delete all edges going into or out of this vertex. *)
      val g2 = 
          g |> Edge.NSet.fold delete_edge ins
            |> Edge.NSet.fold delete_edge (Edge.NSet.subtract outs ins)

    in g2 |> update_vertices (Vertex.NTab.delete n) end;


(* Vertex.name = suggested name *)
(* rename all occurances of old name (1st arg) with new name (2nd arg) 
   n2 must not exist already, n1 must exist *)
fun rename_vname n1 n2 g = 
    let 
      val vertices = (get_vertices g)
      (* gets vertex for n1, makes ure it exists *)
      val (nd,(ins,outs)) = case Vertex.NTab.lookup vertices n1 
                             of NONE => raise no_such_vname_exp ("rename_vname", n1, g)
                              | SOME x => x;

      (* update the vertex entry for n1, we make sure that n2 doesn't already exist *)
      val vertices2 = 
          case (vertices |> Vertex.NTab.delete n1
                         (* raises exception if n2 already exsists *)
                         |> Vertex.NTab.try_ins (n2,(nd,(ins,outs))))
           of NONE => raise vname_already_exists_exp (n2,g)
            | SOME vertices2 => vertices2;

      (* update any edges to use the new name n1 instead of n1, using ins and outs lists *)
      val edges2 = 
          (get_edges g)
            |> Edge.NSet.fold (Edge.NTab.map_entry (fn (e,(from,_)) => 
                                                       (e,(from,n2)))) ins
            |> Edge.NSet.fold (Edge.NTab.map_entry (fn (e,(_,tos)) => 
                                                       (e,(n2,tos)))) outs;

      (* give back new edge name and new graph *)
    in g |> set_vertices vertices2
         |> set_edges edges2
         |> update_Param (Param.rename_vname n1 n2)
    end;


fun rename_ename en1 en2 g = 
    let 
      val edges = (get_edges g)
      (* gets edge for n1, makes ure it exists *)
      val (e,(from_n,to_n)) = case Edge.NTab.lookup edges en1
                           of NONE => raise no_such_ename_exp ("rename_vname", en1, g)
                            | SOME x => x;

      (* update the edge entry for n1, we make sure that n2 doesn't 
         already exist *)
      val edges2 = 
          case (edges |> Edge.NTab.delete en1
                      |> Edge.NTab.try_ins (en2,(e,(from_n,to_n))))
           of NONE => raise ename_already_exists_exp (en2,g)
            | SOME edges2 => edges2;

      (* update any edges to use the new name n1 instead of n1, using 
         ins and outs lists *)
      val vertices2 = 
          (get_vertices g)
            |> Vertex.NTab.map_entry (fn (n,(ins,outs)) => 
                                   let val outs2 = outs |> Edge.NSet.delete en1
                                                        |> Edge.NSet.ins_fresh en2
                                   in (n,(ins,outs2)) end) from_n
            |> Vertex.NTab.map_entry (fn (n,(ins,outs)) =>
                                   let val ins2 = ins |> Edge.NSet.delete en1
                                                      |> Edge.NSet.ins_fresh en2
                                   in (n,(ins2,outs)) end) to_n;

    (* give back new edge name and new graph *)
    in g |> set_vertices vertices2
         |> set_edges edges2
         |> update_Param (Param.rename_ename en1 en2)
    end;

fun pretty_edge_dir_or_undir Directed = Pretty.str "dir"
  | pretty_edge_dir_or_undir UnDirected = Pretty.str "bidir";

(* pretty printing *)
fun pretty g = 
    Pretty.chunks 
      [Pretty.str "Graph{",
       Pretty.block 
         [Pretty.str "  ", (* indent *)
          Pretty.chunks 
          [Pretty.block 
             [Pretty.str "Vertices: ",
              Pretty.chunks 
                (map 
                   (fn (n, (nd,(ins,outs))) => 
                       Pretty.block 
                         [Vertex.pretty_name n,
                          Pretty.str "; ",
                          Pretty.list "i{" "}" (map Edge.pretty_name (Edge.NSet.list_of ins)),
                          Pretty.list "; o{" "}" (map Edge.pretty_name (Edge.NSet.list_of outs)),
                          Pretty.str "; ",
                          Vertex.pretty_data nd
                   ])
                   (Vertex.NTab.list_of (get_vertices g)))],
           Pretty.block
             [Pretty.str "Edges: ",
              Pretty.chunks 
                (map 
                   (fn (en, ((bd,e), (n1, n2))) => 
                       Pretty.block 
                         [Edge.pretty_name en,
                          Pretty.str ": ",
                          Vertex.pretty_name n1,
                          Pretty.str " - ",
                          Vertex.pretty_name n2,
                          Pretty.str "; (",
                          pretty_edge_dir_or_undir bd,
                          Pretty.str ") ",
                          Edge.pretty_data e
                   ])
                   (Edge.NTab.list_of (get_edges g)))]]],
       Pretty.str "}"
      ];
     
val print = Pretty.writeln o pretty;

end;


