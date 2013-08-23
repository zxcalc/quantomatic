(* each edge is either directed or undirected. Undirected edges are stored the same as
 * directed edges, but may be matched in either direction. *)
datatype dir_or_undir = Directed | Undirected
fun dir_eq Directed Directed = true
    | dir_eq Undirected Undirected = true
    | dir_eq _ _ = false

signature OGRAPH_SHARING =
sig
  type T
  type nvdata
  type edata
  type psubst
  type subst
end

signature OGRAPH =
sig

type T (* an open graph *)

type nvdata (* node-vertex data *)
type edata
type psubst (* a partial substitution on data *)
type subst  (* a complete (solved) substition on data *)
datatype vdata = NVert of nvdata | WVert (* vertex data *)


structure Sharing : OGRAPH_SHARING
sharing type Sharing.T = T
sharing type Sharing.nvdata = nvdata
sharing type Sharing.edata = edata
sharing type Sharing.psubst = psubst
sharing type Sharing.subst = subst

type wire = V.name * V.name * V.NSet.T * E.NSet.T (* a chain of edges, representing a wire *)

(* Thin wrappers to Graph.Data substructure *)
val init_psubst : T -> T -> psubst
val match_vdata : vdata * vdata -> psubst -> psubst option
val match_edata : edata * edata -> psubst -> psubst option
val vdata_eq : vdata * vdata -> bool
val edata_eq : edata * edata -> bool
val default_nv_vdata : vdata
val default_wv_vdata : vdata
val default_edata : edata
(* COMPAT: needed for old controller *)
val default_nv_vdata_of_typestring : string -> vdata
(* COMPAT: needed for old controller *)
val default_edata_of_typestring : string -> edata

val pretty_vdata : vdata -> Pretty.T
val pretty_edata : edata -> Pretty.T
val solve_psubst : psubst -> subst Seq.seq

(* Apply substitutions to data. *)
val subst_in_vdata   : subst -> vdata -> subst * vdata
val subst_in_edata   : subst -> edata -> subst * edata
val apply_data_subst : subst -> T -> subst * T


val pretty : T -> Pretty.T
val print : T -> unit
val empty : T

(*********************************)
(********** EXCEPTIONS ***********)
(*********************************)

exception no_such_vertex_exp of string * V.name * T
exception duplicate_vertex_exp of V.name * T

exception no_such_edge_exp of string * E.name * T
exception duplicate_edge_exp of E.name * T

exception bad_graph_merge_exp of T * T

exception not_an_endpoint_exp of string * E.name * V.name * T

exception wire_vertex_exp of string * T * V.name
exception wire_exp of string * T * wire

exception plugging_exp of T * V.name * V.name

(*******************************)
(*********** GRAPHS ************)
(*******************************)

(* graph is empty *)
val is_empty : T -> bool

(* containment checks *)
val has_vertex : T -> V.name -> bool
val has_edge : T -> E.name -> bool

(* getting stuff from graphs (exception if not there) *)
val get_edge_source : T -> E.name -> V.name
val get_edge_target : T -> E.name -> V.name

val get_edge_info : T -> E.name -> (dir_or_undir * edata) * (V.name * V.name)

(* given an edge and vertex, get the other end *)
val edge_get_other_vertex : T -> E.name -> V.name -> V.name

(* ignorant to direction *)
val get_in_edges  : T -> V.name -> E.NSet.T
val get_out_edges : T -> V.name -> E.NSet.T
val get_adj_edges : T -> V.name -> E.NSet.T

(* filters by direction *)
val get_in_dir_edges : T -> V.name -> E.NSet.T
val get_out_dir_edges : T -> V.name -> E.NSet.T
val get_adj_undir_edges : T -> V.name -> E.NSet.T

(* get all adjacent vertices (not including the given vertex or vertices) *)
val get_adj_vertices : T -> V.name -> V.NSet.T
val get_adj_vertices_to_set : T -> V.NSet.T -> V.NSet.T

(* vertices connected to a directed out-edge of the given one *)
val get_successor_vertices : T -> V.name -> V.NSet.T

(* vertices connected to a directed in-edge of the given one *)
val get_predecessor_vertices : T -> V.name -> V.NSet.T

(* vertices connected to an undirected edge of the given one *)
val get_sibling_vertices : T -> V.name -> V.NSet.T

(* get incoming, outgoing, undirected edges adjacent to vertex *)
val adj_edge_classes: T -> V.name -> (E.NSet.T*E.NSet.T*E.NSet.T)


val get_vdata_tab : T -> vdata VTab.T
val get_edata_tab : T -> edata ETab.T

val get_vertex_data : T -> V.name -> vdata
val get_edge_data : T -> E.name -> edata
val get_edge_dir : T -> E.name -> dir_or_undir
val get_edge_dir_and_data : T -> E.name -> dir_or_undir * edata

(* setting data *)
val set_vertex_data : vdata -> V.name -> T -> T
val set_edge_data   : edata -> E.name -> T -> T
val set_edge_dir    : dir_or_undir -> E.name -> T -> T

(* updating data *)
val update_vertex_data : (vdata -> vdata) -> V.name -> T -> T
val update_edge_data   : (edata -> edata) -> E.name -> T -> T
val update_edge_dir    : (dir_or_undir -> dir_or_undir) -> E.name -> T -> T

val is_wire_vertex    : T -> V.name -> bool
val is_node_vertex    : T -> V.name -> bool
val get_vertices      : T -> V.NSet.T
val get_wire_vertices : T -> V.NSet.T
val get_node_vertices : T -> V.NSet.T

val get_edges         : T -> E.NSet.T

(* check if two graphs are exactly the same (names, data, and structure) *)
(* note that the direction of Undirected edges *does* matter *)
val is_subgraph : T -> T -> bool
(* (exact_eq g1 g2) <=> (is_subgraph g1 g2) andalso (is_subgraph g2 g1) *)
val exact_eq : T -> T -> bool

(* adding a vertex gives back unique name for it and the new graph *)
val add_vertex : vdata -> T -> V.name * T 
val add_vertex_anon : vdata -> T -> T
(* raises duplicate_vertex_exp on clash *)
val add_named_vertex : V.name -> vdata -> T -> T  

(* V.names must already exist, else raises: no_such_vertex_exp *)
val add_edge : dir_or_undir * edata -> V.name -> V.name -> T -> E.name * T
val add_edge_anon : dir_or_undir * edata -> V.name -> V.name -> T -> T
val add_named_edge 
    : E.name -> dir_or_undir * edata -> V.name -> V.name -> T -> T (* can raise *)


(* delete raises an exception if there is no such edge/vertex *)
val delete_edge : E.name -> T -> T
val delete_vertex : V.name -> T -> T

val swap_edge_dir : E.name -> T -> T

(* merge two graphs; things with common names are unified *)
(* raises bad_graph_merge_exp if the graphs could not be merged
 * (eg: it would add too many edges to wire-vertices) *)
val merge : T -> T -> T

val get_arity : T -> V.name -> Arity.T
val get_self_loops : T -> V.name -> E.NSet.T


(* remove the first graph from the second, assumes first is a subgraph *)
val delete_subgraph : T -> T -> T

(* edges between two vertices *)
val has_edges_between : T -> V.name -> V.name -> bool
val edges_between : T -> V.name -> V.name -> E.NSet.T

val dir_edges_between : T -> V.name -> V.name 
        -> E.NSet.T (* from fst to snd vertex *) 
         * E.NSet.T (* from snd to fst vertex *)   




(* Renaming *)
(* Renames of non-existent items are successful, and have no effect *)

val rename_ograph      : (VSub.T * ESub.T) -> T -> (VSub.T * ESub.T) * T
val rename_ograph_anon : (VSub.T * ESub.T) -> T -> T
(* rename a bunch of vertices; raises VSub.name_clash_exp if there is a clash *)
val rename_vertices    : VSub.T -> T -> T
(* rename a vertex; raises duplicate_vertex_exp if dest vertex name exists *)
val rename_vertex      : V.name -> V.name -> T -> T
(* rename a vertex; NONE if dest vertex name exists *)
val rename_vertex_opt  : V.name -> V.name -> T -> T option
(* rename a bunch of edges; raises ESub.name_clash_exp if there is a clash *)
val rename_edges       : ESub.T -> T -> T
(* rename an edge; raises duplicate_vertex_exp if dest edge name exists *)
val rename_edge        : E.name -> E.name -> T -> T
(* rename an edge; NONE if dest edge name exists *)
val rename_edge_opt    : E.name -> E.name -> T -> T option



(*******************************)
(********* OPEN GRAPHS *********)
(*******************************)

(* a boundary is a wire vertex with at most one adjacent edge *)
val get_boundary : T -> V.NSet.T
(* NB: only isolated wire-vertices are both inputs and outputs *)
(* an input is a wire-vertex with no in edges and
 * no undirected out edges *)
val get_inputs   : T -> V.NSet.T
(* an output is a wire-vertex with no out edges and
 * no undirected in edges *)
val get_outputs  : T -> V.NSet.T
val is_boundary  : T -> V.name -> bool
val is_input     : T -> V.name -> bool
val is_output    : T -> V.name -> bool


(* Given a wire vertex and an edge, give the other connected edge, if it
   exists. In the case of a wire-vertex with a single, given self-loop,
   return NONE. *)
(* this may raise an exception if the vertex is not a wire-vertex
 * or if the edge is not adjacent to it *)
val wv_get_other_edge : T -> V.name -> E.name -> E.name option

(* Wire equality; assumes the wires are in the same graph *)
val wire_eq : wire -> wire -> bool
(* An arbitrary total order on wires; assumes the wires are in the same graph *)
val wire_ord : wire * wire -> order

val pretty_wire : wire -> Pretty.T


(* Deletes a wire, except for the end-points.
 *
 * Note that deleting a boundary wire, a bare wire or a circle
 * will leave an isolated wire-vertex behind.
 *)
val delete_wire : wire -> T -> T

(* Return the endpoints, intermediate wire-vertices, and edges of the wire containing the given
   edge. The first endpoint is the one closest to the source of the edge. *)
val get_wire_containing_edge : T -> E.name -> wire

(* Return a list of all the wires in the graph that contain any of the given edges *)
val get_wires_for_edges : T -> E.NSet.T -> wire list

(* Return a list of all the wires in the graph *)
val get_wire_list : T -> wire list

(* Return a graph that has exactly two edge points
 * on every wire between two node-vertices, and no
 * unnecessary edge points on any other wire.
 *
 * Note that this function assumes there are no
 * isolated points in the graph - it will not touch
 * isolated points, nor will it create any (bare
 * wires always have at least one edge)
 *)
val normalise : T -> T

(* Return a graph that has no unnecessary edge points.
 *
 * Note that this function assumes there are no
 * isolated points in the graph - it will not touch
 * isolated points, nor will it create any (bare
 * wires always have at least one edge)
 *)
(*val minimise : T -> T*)

(* Split an edge into two edges, connected by a wire-vertex *)
val split_edge : E.name -> T -> (E.name * V.name * E.name) * T


(* Get an edge that can join two boundary vertices, if
 * there is such an edge.
 *
 * This will return NONE exactly when are_pluggable
 * returns false
 *)
val get_plugging : T -> V.name -> V.name
  -> ((dir_or_undir * edata) * (V.name * V.name)) option

(* True if and only if one of the given vertices is
 * an input and one an output with the same edge data *)
val are_pluggable : T -> V.name -> V.name -> bool

(* Plug two boundary vertices together.
 *
 * Raises plugging_exp if the vertices are not pluggable.
 *
 * Returns the name of the created edge.
 *)
val plug : V.name -> V.name -> T -> (E.name * T)
(* Exactly like plug, but does not return the edge name. *)
val plug_anon : V.name -> V.name -> T -> T

(* Plug two boundary vertices together and minimises the
 * plugged wire (so all unnecessary wire vertices will be
 * removed).
 *
 * Raises plugging_exp if the vertices are not pluggable.
 *
 * Returns the new wire
 *)
(*val plug_and_minimise : V.name -> V.name -> T -> (wire * T)*)
(* Exactly like plug_and_minimise, but does not return the wire. *)
(*val plug_and_minimise_anon : V.name -> V.name -> T -> T*)
(* Plug two boundary vertices together and normalises the
 * plugged wire.
 *
 * Raises plugging_exp if the vertices are not pluggable.
 *
 * Returns the new wire
 *)
val plug_and_normalise : V.name -> V.name -> T -> (wire * T)
(* Exactly like plug_and_normalise, but does not return the wire. *)
val plug_and_normalise_anon : V.name -> V.name -> T -> T

(* Returns the minimal subgraph containing the given node vertices,
 * with wires preserved.
 *
 * Wires between vertices in the set will be preserved as-is.  Wires
 * from a vertex in the set to elsewhere in the graph will be
 * retained up to the first wire-vertex.
 *
 * The result is guaranteed to be a true subgraph (ie: no new or
 * renamed vertices) and to be normal, providing the input graph was
 * normal.
 *
 * Implementations may assume the input graph is normalised.
 *)
val get_open_subgraph : T -> V.NSet.T -> T


(*
(* TODO: ? *)
(*val fold_adj_wire_vertices : (V.name * E.name -> 'a -> 'a) -> T -> V.name -> E.name V.NTab.T*)

*)

end
