(* each edge is either directed or undirected. Undirected edges are stored the same as
 * directed edges, but may be matched in either direction. *)
datatype dir_or_undir = Directed | Undirected

signature OGRAPH =
sig

type T (* an open graph *)


(*******************************)
(*********** GRAPHS ************)
(*******************************)

structure OVData : OVERTEX_DATA
structure EData : UNIFIABLE_DATA
structure Param : OGRAPH_PARAM
sharing OVData = Param.OVData
sharing EData = Param.EData

(* graph is empty *)
val is_empty : T -> bool

(* lookup stuff from graphs (return NONE if no entry) *)
val lookup_vertex : T -> V.name -> (OVData.data * (E.NSet.T * E.NSet.T)) option
val lookup_edge : T -> E.name -> ((dir_or_undir * EData.data) * (V.name * V.name)) option

(* getting stuff from graphs (exception if not there) *)
val get_edge_source : T -> E.name -> V.name
val get_edge_target : T -> E.name -> V.name

(* given an edge and vertex, get the other end *)
val edge_get_other_vertex : T -> E.name -> V.name -> V.name

val get_in_edges  : T -> V.name -> E.NSet.T
val get_out_edges : T -> V.name -> E.NSet.T

val get_vertex_data : T -> V.name -> OVData.data
val get_edge_data : T -> E.name -> EData.data

(* setting data *)
val set_vertex_data : OVData.data -> V.name -> T -> T
val set_edge_data : EData.data -> E.name -> T -> T
(* val set_edge_dir_or_undir : dir_or_undir -> E.name -> T -> T *)

(* TODO *)
(*val apply_data_subst : Param.subst -> T -> T*)

(* check if two graphs are exactly the same (names, data, and structure) *)
val exact_eq : T -> T -> bool

(* adding a vertex gives back unique name for it and the new graph *)
val add_vertex : OVData.data -> T -> V.name * T 
val doadd_vertex : OVData.data -> T -> T
(* raises vname_already_exists_exp on clash *)
val add_named_vertex : V.name -> OVData.data -> T -> T  

(* V.names must already exist, else raises: no_such_vname_exp *)
val add_edge : dir_or_undir * EData.data -> V.name -> V.name -> T -> E.name * T
val doadd_edge : dir_or_undir * EData.data -> V.name -> V.name -> T -> T
val add_named_edge 
    : E.name -> dir_or_undir * EData.data -> V.name -> V.name -> T -> T (* can raise *)
val swap_edge_dir : E.name -> T -> T

(* get vertex and edge names *)
val get_vnames : T -> V.NSet.T
val get_enames : T -> E.NSet.T

(* *)
val has_vname : T -> V.name -> bool;
val has_ename : T -> E.name -> bool;

(* renaming *)
val try_rename1_xname : X.name -> X.name -> T -> T option
val try_rename1_vname : V.name -> V.name -> T -> T option
val try_rename1_ename : E.name -> E.name -> T -> T option

val rename : (X.renaming * V.renaming * E.renaming) -> T 
    -> (X.renaming * V.renaming * E.renaming) * T 
(* rename the 2nd graph to avoid all names in the first one *)
val rename_apart : T -> T -> (X.renaming * V.renaming * E.renaming) * T 

(* merge two graphs which have distinct egdes by shared vertices:
   vertices with same name are kept only once, edges must be
   distinct. *)
val merge_by_vertices : T -> T -> T

(* remove the first graph from the second, assumes first is a subgraph *)
val delete_subgraph : T -> T -> T

(* getting graph internal representation... *)
val get_vertex_list : T -> (V.name * (OVData.data 
                   * (E.NSet.T (* edges coming into this vertex *)
                      * E.NSet.T))) (* edges leaving this vertex *)
                    list;
val get_edge_list : T -> (E.name * ((dir_or_undir * EData.data)
                   * (V.name (* from this vertex *)
                      * V.name))) (* to this vertex *)
                    list

(* edges between two vertices *)
val has_edges_between : T -> V.name -> V.name -> bool
val enames_between : T -> V.name -> V.name -> E.NSet.T
val edges_between : T -> V.name -> V.name 
      -> (dir_or_undir * EData.data) E.NTab.T (* in either dir *)
val dir_enames_between : T -> V.name -> V.name 
      -> E.NSet.T (* from fst to snd vertex *) 
         * E.NSet.T (* from snd to fst vertex *) 
val dir_edges_between : T -> V.name -> V.name 
      -> (dir_or_undir * EData.data) E.NTab.T (* from fst to snd vertex *) 
         * (dir_or_undir * EData.data) E.NTab.T (* from snd to fst vertex *) 

(* get adjacent vertices to some vertex *)
val adj_vnames : T -> V.name -> V.NSet.T
    
(* get incoming, outgoing, undirected edges adjacent to vertex *)
val adj_edge_classes: T -> V.name -> (E.NSet.T*E.NSet.T*E.NSet.T)

datatype incidence_type = Incoming | Outgoing | SelfLoop;
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
val fold_adj : (incidence_type * V.name * (E.name * (dir_or_undir * EData.data))
                -> 'a -> 'a)
      -> T -> V.name -> 'a -> 'a

(* the vertices connected to the given set, but outside it *) 
val incident_vertices : T -> V.NSet.T -> V.NSet.T

val get_self_loops : T -> V.name -> EData.data E.NTab.T

val in_enames : T -> V.name -> E.NSet.T
val out_enames : T -> V.name -> E.NSet.T
val pred_vnames : T -> V.name -> V.NSet.T
val succ_vnames : T -> V.name -> V.NSet.T

val get_arity : T -> V.name -> Arity.T

(*(* depth-first traversal *)
val dft : (E.name -> 'a -> 'a) -> T -> 'a -> 'a
val dft_with_roots : (E.name -> 'a -> 'a) -> T -> 'a -> ('a * V.NSet.T)

(* directed acyclic graphs *)
val convert_to_dag : T -> T
val spanning_dag : T -> T
val spanning_tree : T -> T (* another spanning tree, using DAG traversal *)
val get_dag_ranks : T -> V.name list list*)



(*******************************)
(********* OPEN GRAPHS *********)
(*******************************)

exception wire_vertex_exp of (string * T * V.name);

(* boundaries represent end of a half-edge.  *)
val get_boundary : T -> V.NSet.T
val get_inputs : T -> V.NSet.T
val get_outputs : T -> V.NSet.T
val is_boundary : T -> V.name -> bool

(* internal: don't presurve well-formedness *)
val add_to_boundary : V.name -> T -> T
val del_from_boundary : V.name -> T -> T
val update_boundary : (V.NSet.T -> V.NSet.T) -> T -> T

(* get_plugging is copied further down, with the other plugging functions *)

(* *** END OF COPIED SIGNATURE *** *)

(* Given selection set of vertices, make subgraph: 
   all incident edges to selection are removed; Like copy in a graphics program *)
val delete_subgraph_outside_vset : V.NSet.T -> T -> T

type Wire = V.name * V.name * V.NSet.T * E.NSet.T;

(*** functions useful when dealing ignorantly with undirected edges ***)

exception wire_exp of (string * T * Wire);

(* Given a wire vertex and an edge, give the other connected edge, if it
   exists. In the case of a wire-vertex with a single, given self-loop,
   return NONE. *)
val wv_get_other_edge : T -> V.name -> E.name -> E.name option

(* Wire equality; assumes the wires are in the same graph *)
val wire_eq : Wire -> Wire -> bool
(* An arbitrary total order on wires; assumes the wires are in the same graph *)
val wire_ord : Wire * Wire -> order

val pretty_wire : Wire -> Pretty.T

(* Deletes a wire, except for the end-points.
 *
 * Note that deleting a boundary wire, a bare wire or a circle
 * will leave an isolated wire-vertex behind.
 *)
val delete_wire : Wire -> T -> T

(* raises wire_exp if the wire is not coherent *)
val assert_wire_coherent : T -> Wire -> Wire

(* Returns true if minimising the wire would have no effect *)
val is_wire_minimal : T -> Wire -> bool
(* Removes any unnecessary wire vertices from a wire
 *
 * Raises an exception if the wire is not coherent.
 *
 * Returns the new wire and the new graph
 *)
val minimise_wire : Wire -> T -> Wire * T
(* Same as minimise_wire, but does not give you the wire back *)
val do_minimise_wire : Wire -> T -> T

(* Returns true if normalising the wire would have no effect *)
val is_wire_normalised : T -> Wire -> bool
(* Ensures that there are exactly two wire-vertices on the wire if
 * it is an internal wire (one between two node-vertices), and
 * minimises the wire otherwise.
 *
 * Raises an exception if the wire is not coherent.
 *
 * Returns the new wire and the new graph
 *)
val normalise_wire : Wire -> T -> Wire * T
(* Same as normalise_wire, but does not give you the wire back *)
val do_normalise_wire : Wire -> T -> T

(* Return the endpoints, intermediate wire-vertices, and edges of the wire containing the given
   edge. The first endpoint is the one closest to the source of the edge. *)
val get_wire_containing_edge : T -> E.name -> Wire

(* Return a list of all the wires in the graph that contain any of the given edges *)
val get_wires_for_edges : T -> E.NSet.T -> Wire list

(* Return a list of all the wires in the graph *)
val get_wire_list : T -> Wire list

val is_input_vertex : T -> V.name -> bool
val is_output_vertex : T -> V.name -> bool
val is_boundary_vertex : T -> V.name -> bool

val is_wire_vertex    : T -> V.name -> bool
val is_node_vertex    : T -> V.name -> bool
val get_wire_vertices : T -> V.NSet.T
val get_node_vertices : T -> V.NSet.T

exception plugging_exp of (T*V.name*V.name);

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
val get_plugging : T -> V.name -> V.name
  -> ((dir_or_undir * EData.data) * (V.name * V.name)) option
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
val plug_and_minimise : V.name -> V.name -> T -> (Wire * T)
(* Exactly like plug_and_minimise, but does not return the wire. *)
val do_plug_and_minimise : V.name -> V.name -> T -> T
(* Plug two boundary vertices together and normalises the
 * plugged wire.
 *
 * Raises plugging_exp if the vertices are not pluggable.
 *
 * Returns the new wire
 *)
val plug_and_normalise : V.name -> V.name -> T -> (Wire * T)
(* Exactly like plug_and_normalise, but does not return the wire. *)
val do_plug_and_normalise : V.name -> V.name -> T -> T

(* TODO: ? *)
(*val fold_adj_wire_vertices : (V.name * E.name -> 'a -> 'a) -> T -> V.name -> E.name V.NTab.T*)

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
 * This will NOT WORK if the graph is not normalised.
 *)
val get_open_subgraph : V.NSet.T -> T -> T

end
