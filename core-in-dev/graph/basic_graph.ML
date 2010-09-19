(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Graphs with named vertices and edges. *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

signature BASIC_GRAPH_SHARING = 
sig
structure Edge : EDGE_AND_DATA_SHARING;
structure Vertex : NAME_AND_DATA_SHARING;
type dir_or_undir;
type T;
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* Basic (Minimal) interface for Graphs *)
signature BASIC_GRAPH = 
sig

structure Vertex : VERTEX
structure Edge : EDGE

(* each edge is either directed, or bi-directed: this is the family of graphs
   with these edges in both directions. *)
datatype dir_or_undir = Directed | UnDirected;

type T; (* a graph *)

structure SharingBasicGraph : BASIC_GRAPH_SHARING
sharing SharingBasicGraph.Edge = Edge.EdgeSharingWithData;
sharing SharingBasicGraph.Vertex = Vertex.SharingWithData;
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

