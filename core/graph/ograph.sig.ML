(* each edge is either directed or undirected. Undirected edges are stored the same as
 * directed edges, but may be matched in either direction. *)
datatype dir_or_undir = Directed | Undirected
fun dir_eq Directed Directed = true
    | dir_eq Undirected Undirected = true
    | dir_eq _ _ = false

signature OGRAPH_SHARING =
sig
  type T
  structure Data : GRAPH_DATA
end

signature OGRAPH =
sig

type T (* an open graph *)
structure Data : GRAPH_DATA

structure Sharing : OGRAPH_SHARING
sharing type Sharing.T = T
sharing Sharing.Data = Data

type wire = V.name * V.name * V.NSet.T * E.NSet.T (* a chain of edges, representing a wire *)
datatype vdata = NVert of Data.nvdata | WVert
type edata = Data.edata
val vdata_eq : vdata * vdata -> bool
val edata_eq : edata * edata -> bool (* alias for Data.edata_eq *)

val empty : T

(*********************************)
(********** EXCEPTIONS ***********)
(*********************************)

(*exception bug_exp of string*)
exception no_such_vname_exp of string * V.name * T
(*exception no_such_vnames_exp of string * V.NSet.T * T*)
exception vname_already_exists_exp of V.name * T

exception no_such_ename_exp of string * E.name * T
exception ename_already_exists_exp of E.name * T

exception bbname_already_exists_exp of B.name * T

exception bad_graph_merge_exp of T * T * T

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

(* get all adjacent vertices (not including the given one) *)
val get_adj_vertices : T -> V.name -> V.NSet.T

(* get incoming, outgoing, undirected edges adjacent to vertex *)
val adj_edge_classes: T -> V.name -> (E.NSet.T*E.NSet.T*E.NSet.T)


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


(* TODO *)
(*val apply_data_subst : Param.subst -> T -> T*)

(* check if two graphs are exactly the same (names, data, and structure) *)
val is_subgraph : T -> T -> bool
val exact_eq : T -> T -> bool

(* adding a vertex gives back unique name for it and the new graph *)
val add_vertex : vdata -> T -> V.name * T 
val add_vertex_anon : vdata -> T -> T
(* raises vname_already_exists_exp on clash *)
val add_named_vertex : V.name -> vdata -> T -> T  

(* V.names must already exist, else raises: no_such_vname_exp *)
val add_edge : dir_or_undir * Data.edata -> V.name -> V.name -> T -> E.name * T
val add_edge_anon : dir_or_undir * Data.edata -> V.name -> V.name -> T -> T
val add_named_edge 
    : E.name -> dir_or_undir * Data.edata -> V.name -> V.name -> T -> T (* can raise *)


(* delete raises an exception if there is no such edge/vertex *)
val delete_edge : E.name -> T -> T
val delete_vertex : V.name -> T -> T

val swap_edge_dir : E.name -> T -> T


val is_intersection : T -> T -> T -> bool

(* merge two graphs on a subgraph, which must be their intersection *)
val merge_by_subgraph : T -> T -> T -> T

(* merge two graphs which have distinct egdes by shared vertices:
   vertices with same name are kept only once, edges must be
   distinct. *)
val merge_by_vertices : T -> T -> T

val get_arity : T -> V.name -> Arity.T
val get_self_loops : T -> V.name -> E.NSet.T

(* TODO: change this names to *_vertex, *_edge, etc *)
(*val try_rename1_xname : X.name -> X.name -> T -> T option*)
val try_rename1_vname : V.name -> V.name -> T -> T option
val try_rename1_ename : E.name -> E.name -> T -> T option



(* remove the first graph from the second, assumes first is a subgraph *)
val delete_subgraph : T -> T -> T

(* edges between two vertices *)
val has_edges_between : T -> V.name -> V.name -> bool
val edges_between : T -> V.name -> V.name -> E.NSet.T

val dir_edges_between : T -> V.name -> V.name 
        -> E.NSet.T (* from fst to snd vertex *) 
         * E.NSet.T (* from snd to fst vertex *)   



(*
(* TODO: renaming *)


val rename : (X.renaming * V.renaming * E.renaming) -> T 
    -> (X.renaming * V.renaming * E.renaming) * T 
(* rename the 2nd graph to avoid all names in the first one *)
val rename_apart : T -> T -> (X.renaming * V.renaming * E.renaming) * T 
*)


(*******************************)
(********* OPEN GRAPHS *********)
(*******************************)

val get_boundary : T -> V.NSet.T
val get_inputs   : T -> V.NSet.T
val get_outputs  : T -> V.NSet.T
val is_boundary  : T -> V.name -> bool
val is_input     : T -> V.name -> bool
val is_output    : T -> V.name -> bool


(* Given a wire vertex and an edge, give the other connected edge, if it
   exists. In the case of a wire-vertex with a single, given self-loop,
   return NONE. *)
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
val minimise : T -> T


val get_plugging : T -> V.name -> V.name
  -> ((dir_or_undir * Data.edata) * (V.name * V.name)) option

(* True if and only if one of the given vertices is
 * an input and one an output with the same edge data *)
val are_pluggable : T -> V.name -> V.name -> bool
(* Get an edge that can join two boundary vertices, if
 * there is such an edge.
 *
 * This will return NONE exactly when are_verts_pluggable
 * returns false
 *)
(* copied from BASIC_BANG_GRAPH *)

(* Plug two boundary vertices together.
 *
 * Raises plugging_exp if the vertices are not pluggable.
 *
 * Returns the name of the created edge.
 *)
val plug : V.name -> V.name -> T -> (E.name * T)
(* Exactly like plug, but does not return the edge name. *)
val do_plug : V.name -> V.name -> T -> T

(* Plug two boundary vertices together and minimises the
 * plugged wire (so all unnecessary wire vertices will be
 * removed).
 *
 * Raises plugging_exp if the vertices are not pluggable.
 *
 * Returns the new wire
 *)
val plug_and_minimise : V.name -> V.name -> T -> (wire * T)
(* Exactly like plug_and_minimise, but does not return the wire. *)
val do_plug_and_minimise : V.name -> V.name -> T -> T
(* Plug two boundary vertices together and normalises the
 * plugged wire.
 *
 * Raises plugging_exp if the vertices are not pluggable.
 *
 * Returns the new wire
 *)
val plug_and_normalise : V.name -> V.name -> T -> (wire * T)
(* Exactly like plug_and_normalise, but does not return the wire. *)
val do_plug_and_normalise : V.name -> V.name -> T -> T

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


(************************)
(****** DEPRECATED ******)
(************************)

(*datatype incidence_type = Incoming | Outgoing | SelfLoop;
(* Fold over the incident edges.
 *
 * Each incident edge will feature exactly once, as an
 * incoming edge, an outgoing edge or a self-loop.
 *
 * In addition to the edge information and the incidence
 * type, the vertex on other other end will be given.
 * In the case of self-loops, this will be the same as
 * the vertex originally passed.
 *
 * The folding order is undefined.
 *)
val fold_adj : (incidence_type * V.name * (E.name * (dir_or_undir * Data.edata))
                -> 'a -> 'a)
      -> T -> V.name -> 'a -> 'a

(* the vertices connected to the given set, but outside it *) 
val incident_vertices : T -> V.NSet.T -> V.NSet.T

val get_self_loops : T -> V.name -> Data.edata E.NTab.T

val in_enames : T -> V.name -> E.NSet.T
val out_enames : T -> V.name -> E.NSet.T
val pred_vnames : T -> V.name -> V.NSet.T
val succ_vnames : T -> V.name -> V.NSet.T

val get_arity : T -> V.name -> Arity.T*)

(*(* depth-first traversal *)
val dft : (E.name -> 'a -> 'a) -> T -> 'a -> 'a
val dft_with_roots : (E.name -> 'a -> 'a) -> T -> 'a -> ('a * V.NSet.T)

(* directed acyclic graphs *)
val convert_to_dag : T -> T
val spanning_dag : T -> T
val spanning_tree : T -> T (* another spanning tree, using DAG traversal *)
val get_dag_ranks : T -> V.name list list*)

(*
(* get vertex and edge names *)
val get_vnames : T -> V.NSet.T
val get_enames : T -> E.NSet.T

(* *)
val has_vname : T -> V.name -> bool
val has_ename : T -> E.name -> bool

(* lookup stuff from graphs (return NONE if no entry) *)
val lookup_vertex : T -> V.name -> (vdata * (E.NSet.T * E.NSet.T)) option
val lookup_edge : T -> E.name -> ((dir_or_undir * Data.edata) * (V.name * V.name)) option


(* getting graph internal representation... *)
val get_vertex_list : T -> (V.name * (vdata 
                   * (E.NSet.T (* edges coming into this vertex *)
                      * E.NSet.T))) (* edges leaving this vertex *)
                    list;
val get_edge_list : T -> (E.name * ((dir_or_undir * Data.edata)
                   * (V.name (* from this vertex *)
                      * V.name))) (* to this vertex *)
                    list
                    
val enames_between : T -> V.name -> V.name -> E.NSet.T

CHANGED SIG: val edges_between : T -> V.name -> V.name -> (dir_or_undir * Data.edata) E.NTab.T (* in either dir *)


val dir_enames_between : T -> V.name -> V.name 
      -> E.NSet.T (* from fst to snd vertex *) 
         * E.NSet.T (* from snd to fst vertex *)
         
CHANGED SIG: val dir_edges_between : T -> V.name -> V.name 
      -> (dir_or_undir * Data.edata) E.NTab.T (* from fst to snd vertex *) 
         * (dir_or_undir * Data.edata) E.NTab.T (* from snd to fst vertex *) 


(* get adjacent vertices to some vertex *)
val adj_vnames : T -> V.name -> V.NSet.T

(* internal: don't preserve well-formedness *)
val add_to_boundary : V.name -> T -> T
val del_from_boundary : V.name -> T -> T
val update_boundary : (V.NSet.T -> V.NSet.T) -> T -> T


TODO: move this into controller_state (the only place it is used)
(* Given selection set of vertices, make subgraph: 
   all incident edges to selection are removed; Like copy in a graphics program *)
val delete_subgraph_outside_vset : V.NSet.T -> T -> T

*** Now called is_input, is_output, is_boundary

val is_input_vertex : T -> V.name -> bool
val is_output_vertex : T -> V.name -> bool
val is_boundary_vertex : T -> V.name -> bool


*** These are now all private functions

(* raises wire_exp if the wire is not coherent *)
val assert_wire_coherent : T -> wire -> wire

(* Returns true if minimising the wire would have no effect *)
val is_wire_minimal : T -> wire -> bool

(* Removes any unnecessary wire vertices from a wire
 *
 * Raises an exception if the wire is not coherent.
 *
 * Returns the new wire and the new graph
 *)
val minimise_wire : wire -> T -> wire * T
(* Same as minimise_wire, but does not give you the wire back *)
val do_minimise_wire : wire -> T -> T

(* Returns true if normalising the wire would have no effect *)
val is_wire_normalised : T -> wire -> bool
(* Ensures that there are exactly two wire-vertices on the wire if
 * it is an internal wire (one between two node-vertices), and
 * minimises the wire otherwise.
 *
 * Raises an exception if the wire is not coherent.
 *
 * Returns the new wire and the new graph
 *)
val normalise_wire : wire -> T -> wire * T
(* Same as normalise_wire, but does not give you the wire back *)
val do_normalise_wire : wire -> T -> T

*)

end
