(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Open Graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graphs with Boundary *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature OGRAPH_SHARING = 
sig 
include BASIC_GRAPH_SHARING;
structure OVData : OVERTEX_DATA_SHARING;
sharing VData = OVData;
end;

(* Basic open graph *)
signature BASIC_OGRAPH = 
sig
include BASIC_GRAPH
structure OVData : OVERTEX_DATA
sharing VData.Sharing = OVData.Sharing
structure SharingOGraph : OGRAPH_SHARING
sharing SharingBasicGraph = SharingOGraph
sharing SharingOGraph.OVData = OVData.SharingWithIData

(* points (edge and boundary, both kinds of vertices) 
   have no data. Boundary have only input edges, 
   or only output edges. Edgepoints have both input and output edges *)


(* FIXME: these methods should be deprecated in favour of the ones below.
   I suggest keeping track of boundary info in Rule, closer to where it is
   used. *)


(* boundaries represent end of a half-edge.  *)
val get_boundary : T -> V.NSet.T
val is_boundary : T -> V.name -> bool

(* edge points; parts of an edge *)
val get_edgepoints : T -> V.NSet.T
val is_edgepoint : T -> V.name -> bool

(* interior vertices are neither *)
val is_ivertex : T -> V.name -> bool

val is_endpoint' : OVData.data * (E.NSet.T * E.NSet.T) -> bool
(* returns the SOME non-empty edge set when exactly one of the inset of ourset of edges is empty, 
   else NONE *)
val endpoint_of_vertex' : OVData.data * (E.NSet.T * E.NSet.T) 
    -> (H.halfid * E.NSet.T) option

(* internal: don't presurve well-formedness *)
val add_to_boundary : V.name -> T -> T
val add_to_edgepoints : V.name -> T -> T
val del_from_boundary : V.name -> T -> T
val del_from_edgepoints : V.name -> T -> T
val update_boundary : (V.NSet.T -> V.NSet.T) -> T -> T
val update_edgepoints : (V.NSet.T -> V.NSet.T) -> T -> T

(* Get an edge that can join two boundary vertices, if
 * there is such an edge.
 *)
val get_plugging : T -> V.name -> V.name
  -> ((dir_or_undir * EData.data) * (V.name * V.name)) option

end (* Basic OGraph *)


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Extra stuff for open graphs (cut out subgraph etc.) *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature OGRAPH = 
sig 
include GRAPH
(* include BASIC_OGRAPH *** FIXME: fix ML: should be able to include union of signatures. Without it, we copy/paste code from BASIC_OGRAPH... *)
structure OVData : OVERTEX_DATA
sharing VData.Sharing = OVData.Sharing
structure SharingOGraph : OGRAPH_SHARING
sharing SharingBasicGraph = SharingOGraph
sharing SharingOGraph.OVData = OVData.SharingWithIData

(* points (edge and boundary, both kinds of vertices) 
   have no data. Boundary have only input edges, 
   or only output edges. Edgepoints have both input and output edges *)

(* boundaries represent end of a half-edge.  *)
val get_boundary : T -> V.NSet.T
val get_inputs : T -> V.NSet.T
val get_outputs : T -> V.NSet.T
val is_boundary : T -> V.name -> bool

(* given two vertices, check if they are boundary-coherent end-points and return 
   data for a compatible edge that could replace them. *)
val boundary_coherence : T -> V.name -> V.name -> 
     ((dir_or_undir * EData.data) * (V.name * V.name)) option

(* edge points; parts of an edge *)
val get_edgepoints : T -> V.NSet.T
val is_edgepoint : T -> V.name -> bool

(* internal: don't presurve well-formedness *)
val add_to_boundary : V.name -> T -> T
val add_to_edgepoints : V.name -> T -> T
val del_from_boundary : V.name -> T -> T
val del_from_edgepoints : V.name -> T -> T
val update_boundary : (V.NSet.T -> V.NSet.T) -> T -> T
val update_edgepoints : (V.NSet.T -> V.NSet.T) -> T -> T

(* get_plugging is copied further down, with the other plugging functions *)

(* *** END OF COPIED SIGNATURE *** *)

(* plug together two boundary-coherent edge points. In the process, this function creates an edge, and
   returns ((source, target), (edge, new graph)) *)
val plug_by_boundary : V.name -> V.name -> T -> (V.name * V.name) * (E.name * T)


val is_boundary_edge : T -> E.name -> bool

(* get points living on an edge; but not boundary ones *)
val get_edgepoint_vnames : T -> V.NSet.T

(* get the subsets of boundary, divided into either input or output. *)
val get_in_and_out_vnames : T -> V.NSet.T (* input *) 
                                 * V.NSet.T (* output *)

(* change vertex into a point (throw away data, making this an edge-point or a boundary point) *)
val change_into_edgepoint : V.name -> T -> T
val change_into_internal : V.name -> OVData.IData.data -> T -> T

(* merge points (all adj edges) and keep vertex data (replacing edge-point), assumes they have the same internal data if they are both internal vertices *)
val rename_or_merge_vname : V.name -> V.name -> T -> T

(* cut all edges that go outside the selected set of vertices from the 
   given vertex. Extends the gluing automorphism *)
val cut_around_vertex : V.NSet.T -> V.name 
    -> (VInjEndo.T * T) -> (VInjEndo.T * T)
(* cut all edges that go from within the subgraph to outside it; 
   returns the gluing automorphism, and a new graph which will be disjoint *)
val cut_around_subgraph: V.NSet.T -> T -> (VInjEndo.T * T)

(* Given selection set of vertices, make subgraph with with incident edges that 
   went to a vertex outside selection, now go to a single edge-point. Perserves 
   matching; doesn't (necessarily) produce a valid ograph. *)
val matched_subgraph_within_vset' : V.NSet.T -> T 
    -> VInjEndo.T * T (* returns gluing/cutting for new 'boundary', 
                              and cut graph *)
val matched_subgraph_within_vset : V.NSet.T -> T -> T

(* Given selection set of vertices, make subgraph: 
   all incident edges to selection are removed; Like copy in a graphics program *)
val delete_subgraph_outside_vset : V.NSet.T -> T -> T



(* NEW OGraph functions. These use the new terminology, and should
   supercede their equivalents *)



(* TODO: remove old functions when convenient *)

type Wire = V.name * V.name * V.NSet.T * E.NSet.T;

(*** functions useful when dealing ignorantly with undirected edges ***)

exception wire_exp of (string * T * Wire);

(* Given a wire vertex and an edge, give the other connected edge, if it
   exists. In the case of a wire-vertex with a single, given self-loop,
   return NONE. *)
val wv_get_other_edge : T -> V.name -> E.name -> E.name option

(* given an edge and vertex, get the other end *)
val edge_get_other_vertex : T -> E.name -> V.name -> V.name

val wire_eq : Wire -> Wire -> bool
val wire_ord : Wire * Wire -> order

val pretty_wire : Wire -> Pretty.T

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

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Basic Open Graph (Graph with a boundary/edge-points) *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor BasicOGraphFun(
(*         structure OVData : OVERTEX *)
        structure Param : OGRAPH_PARAM
        (* sharing Param.VData.SharingWithData = OVData.SharingWithData *)
        type data (* graph additional extensible/hidden data *)
        val empty_data : data
) 
: BASIC_OGRAPH
= struct
  structure InParam = Param;
  
  (* to avoid using clobbered data and empty graph from the 
     open G (see below) *)
  type data2 = data;
  
  (* we add boundary to Graph data, as well as edge-points; these are 
     disjoint sets *)
  type hdata = { boundary : V.NSet.T,
                 edgepoints : V.NSet.T,
                 data : data};
  val empty_hdata = {boundary = V.NSet.empty,
                     edgepoints = V.NSet.empty,
                     data = empty_data};
  fun get_data_of_hdata hdata = #data hdata;
  fun update_data_of_hdata f hdata = 
      {boundary = #boundary hdata, 
       edgepoints = (#edgepoints hdata),
       data = f (#data hdata)};
  fun get_boundary_of_hdata hdata = #boundary hdata;
  fun update_boundary_of_hdata f hdata = 
      {boundary = f (#boundary hdata), 
       edgepoints = (#edgepoints hdata),
       data = (#data hdata)};    
  fun get_edgepoints_of_hdata hdata = #edgepoints hdata;
  fun update_edgepoints_of_hdata f hdata = 
      {boundary = (#boundary hdata), 
       edgepoints = f (#edgepoints hdata),
       data = (#data hdata)};    


  (* make basic graph with this as internal data *)
  structure G = BasicGraphFun(
    structure Param = Param;
    type data = hdata
    val empty_data = empty_hdata
  );
  (* inherit functions from Graph *)
  open G;
  structure Param = InParam;
  structure OVData = Param.OVData;
  structure VData = OVData;
  structure EData = Param.EData;
  
  (* clobber sub-graph data *)
  type data = data2;
  val get_data = get_data_of_hdata o G.get_data;
  val update_data = G.update_data o update_data_of_hdata;

  (* for boundary part of added data *)
  val get_boundary = get_boundary_of_hdata o G.get_data;
  val update_boundary = G.update_data o update_boundary_of_hdata;
  (* for edgepoints *)
  val get_edgepoints = get_edgepoints_of_hdata o G.get_data;
  val update_edgepoints = G.update_data o update_edgepoints_of_hdata;

  (* *)
  val add_to_boundary = update_boundary o V.NSet.add;
  val del_from_boundary = update_boundary o V.NSet.delete;
  val is_boundary = V.NSet.contains o get_boundary;

  (* endpoints are edge-points with ins or outs empty. *)
  fun endpoint_of_vertex' (OVData.EVert,(ins,outs)) =
      (case (E.NSet.is_empty ins, E.NSet.is_empty outs) of
         (true, true) => NONE
       | (true,false) => SOME (H.SourceEnd, outs)
       | (false,true) => SOME (H.TargetEnd, ins)
       | (false,false) => NONE)
    | endpoint_of_vertex' (OVData.IVert _,_) = NONE;
  
  fun is_endpoint' (OVData.EVert,(ins,outs)) =
      (case (E.NSet.is_empty ins, E.NSet.is_empty outs) of
         (true, true) => false
       | (true,false) => true
       | (false,true) => true
       | (false,false) => false)
    | is_endpoint' (OVData.IVert _,_) = false;
  val is_edgepoint = V.NSet.contains o get_edgepoints;
  val add_to_edgepoints = update_edgepoints o V.NSet.add;
  val del_from_edgepoints = update_edgepoints o V.NSet.delete;
  
  fun is_ivertex g v = not (is_edgepoint g v orelse is_boundary g v)

  (* implicit: g *)
  fun rename1_in_boundary n1 n2 = 
      update_boundary (fn bs => 
                          case V.NSet.try_rename1 n1 n2 bs
                           of NONE => bs
                            | SOME bs2 => bs2);

  (* implicit: g *)
  fun rename1_in_edgepoints n1 n2 = 
      update_edgepoints (fn vs => 
                          case V.NSet.try_rename1 n1 n2 vs
                           of NONE => vs
                            | SOME vs2 => vs2);


  (* modified/new basic graph functions *)
  fun add_vertex_named_like n v g =
      let val (n2,g2) = G.add_vertex_named_like n v g
      in (n2, g2 |> (case v of (OVData.EVert) => add_to_edgepoints n2 
                             | (OVData.IVert _) => I))
      end;
  (* Note: deleting vertex also removes all it's edges. *)
  fun delete_vertex n g = 
      g |> del_from_boundary n (* if not in boundary, does nothing *)
        |> del_from_edgepoints n (* if not in edgepoints, does nothing *)
        |> G.delete_vertex n;

  (* *)
  fun update_point_kind_from_del_edge n g = 
      let val (d,(ins,outs)) = V.NTab.get (get_vertices g) n in
        case d of 
          OVData.IVert _ => g
        | OVData.EVert => 
          (case (E.NSet.is_empty ins, E.NSet.is_empty outs) of
             (true, true) => g |> del_from_boundary n (* became isolated *)
           | (false,true) => g |> add_to_boundary n
           | (true,false) => g |> add_to_boundary n
           | (false,false) => g (* had many incident edges *))
      end;

  (* *)
  fun update_point_kind_from_add_edge n g = 
      let val (d,(ins,outs)) = V.NTab.get (get_vertices g) n in
        case d of 
          OVData.IVert _ => g
        | OVData.EVert => 
          (case (E.NSet.is_empty ins, E.NSet.is_empty outs) of
             (* has both incoming and outgoing egdes, so not boundary. *)
             (false, false) => g |> del_from_boundary n |> add_to_edgepoints n
           | (_,_) => g |> add_to_boundary n)
      end;

  (* *)
  fun add_edge_named_like en e n1 n2 g = 
      let val (en2,g2) = G.add_edge_named_like en e n1 n2 g in 
        (en2, 
         g2 |> update_point_kind_from_add_edge n1 
            |> update_point_kind_from_add_edge n2)
      end;

  fun delete_edge en g = 
      let val (_,(fromv,tov)) = E.NTab.get (get_edges g) en in
        g |> G.delete_edge en
          |> update_point_kind_from_del_edge fromv
          |> update_point_kind_from_del_edge tov
      end;

  (* vname = suggested name *)
  (* rename all occurances of old name (1st arg) with new name (2nd arg) 
     n2 must not exist already, n1 must exist 
     raises 
  *)
  fun rename_vname n1 n2 g = 
      g |> rename1_in_boundary n1 n2
        |> rename1_in_edgepoints n1 n2 
        |> G.rename_vname n1 n2;

  fun get_only_edge' (OVData.EVert,(ins,outs)) =
    (case (E.NSet.is_empty ins,E.NSet.is_empty outs) of
          (true,false) => E.NSet.tryget_singleton outs
        | (false,true) => E.NSet.tryget_singleton ins
        | _            => NONE)
    | get_only_edge' (OVData.IVert _,_) = NONE;
  val get_only_edge = get_only_edge' oo get_vertex
  fun get_plugging g v1 v2 =
    case (get_only_edge g v1,get_only_edge g v2) of
         (SOME e1,SOME e2) =>
            (case (get_edge g e1,get_edge g e2) of
                  (((UnDirected,d1),_),((UnDirected,d2),_)) =>
                    if EData.data_eq (d1,d2)
                    then SOME ((UnDirected,d1),(v1,v2))
                    else NONE
                | (((Directed,d1),(e1s,e1t)),((Directed,d2),(e2s,e2t))) =>
                    if not (EData.data_eq (d1,d2))
                    then NONE
                    else if (V.name_eq (e1s,v1) andalso V.name_eq (e2t,v2))
                    then SOME ((Directed,d1),(v2,v1))
                    else if (V.name_eq (e1t,v1) andalso V.name_eq (e2s,v2))
                    then SOME ((Directed,d1),(v1,v2))
                    else NONE
                | _ => NONE
            )
       | _ => NONE


  (* prettifying *)
  fun pretty g = 
      Pretty.chunks 
        [ Pretty.str "OGraph{",
          Pretty.block 
            [Pretty.str "  ", (* indent *)
             Pretty.chunks 
               [Pretty.block 
                  [Pretty.str "Boundary Vertices: ",
                   Pretty.list "{" "}" 
                               (map V.pretty_name (V.NSet.list_of (get_boundary g)))],
                G.pretty g]],
          Pretty.str "}"
        ];
      
  val print = Pretty.writeln o pretty;
  
  structure SharingOGraph = 
    struct
      open SharingBasicGraph;
      structure OVData = OVData.SharingWithIData;
    end;

end; (* structure BasicOGraph *)



(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Extra stuff for open graphs (cut out subgraph etc.) *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor OGraphFun(G : BASIC_OGRAPH)
(* : OGRAPH *) 
= struct
  open G;
  
  structure GraphTools = GraphToolsFun(G);
  open GraphTools;
  
  (* change n to become a boundary vertex *)
  fun change_into_edgepoint n g = 
      g |> (if is_endpoint' (get_vertex g n)
            then add_to_boundary else add_to_edgepoints) n
        |> set_vertex_data OVData.EVert n;
  
  (* change n to become a boundary vertex *)
  fun change_into_internal n d g = 
      g |> del_from_boundary n
        |> del_from_edgepoints n
        |> set_vertex_data (OVData.IVert d) n;
  
  (* cut an edge, adding the introduced bounary vertices to the automorphism *)
  fun cut_edge' (e,(d,(fromv,tov))) g = 
      let val (newv1,g1) = add_vertex OVData.EVert g 
          val (newv2,g2) = add_vertex OVData.EVert g1
          val (newe,g3) = add_edge d fromv newv2 g2
      in
        (((e,newv2),(newe,newv1)),
         g3 |> update_edges (E.NTab.update (e, (d,(newv1,tov))))
            |> add_to_boundary newv1 
            |> add_to_boundary newv2)
      end;

  fun cut_edge e g = cut_edge' (e, get_edge g e) g

  (* v1 and v2 are boundary coherent (half-edges of opposite direction, or both undirected): they can be plugged together; return SOME ((fromv,tov),(dir,edgedata)) where fromv is the source of connecting edge, and tov is the target. If the half-edges were directed, this respects the edge direction. 
  Assumes edge data is the same for both edges. 
  *)
  (* FIXME: rewrite not to use half-edges or remove *)
  fun boundary_coherence g v1 v2 = 
      (case endpoint_of_vertex' (get_vertex g v1) of 
         NONE => NONE
       | SOME (halfid1,es1) => 
         (case endpoint_of_vertex' (get_vertex g v2) of 
            NONE => NONE
          | SOME (halfid2,es2) => 
            (case (E.NSet.tryget_singleton es1, 
                   E.NSet.tryget_singleton es2) of 
               (SOME e1,SOME e2) => 
               let val ((dir_or_undir1,ed1),(fromv1,tov1)) = get_edge g e1
                   val ((dir_or_undir2,_),(fromv2,tov2)) = get_edge g e2
               in 
                  if dir_or_undir1 = Directed andalso dir_or_undir2 = Directed
                     andalso halfid1 <> halfid2 then 
                    SOME ((dir_or_undir1, ed1),
                          if halfid1 = H.SourceEnd then (fromv2,tov1) 
                          else (fromv1,tov2))
                  else if dir_or_undir1 = UnDirected 
                       andalso dir_or_undir2 = UnDirected then 
                    SOME ((dir_or_undir1, ed1),
                          (if halfid1 = H.SourceEnd then tov1 
                           else fromv1, 
                           if halfid2 = H.SourceEnd then tov2 
                           else fromv2))
                  else NONE
               end
             | (_,_) => NONE)));

  (* plug two half-edges, represented by vertices. This is equiavlent to graph 
     composition with an identity. Assumes that e1 and e2 go to boundary 
     coherent edge-point vertices (vertices representing half-edges going in 
     opposite directions / both undirected) *)
  (* If we have e1 : v2 - v1 and e2: v1 - v2 then we cannot remove the
   * edge-points and add an edge between them: we remove them and add an
   * edge-point with a self-loop  *)
  (* FIXME(ldixon): *** needs modification for !-boxes *** *) 
  exception not_boundary_coherent_exp of string;

  fun plug_by_boundary v1 v2 g = 
      (case boundary_coherence g v1 v2 
        of NONE => raise not_boundary_coherent_exp "plug_by_boundary"
         | SOME ((dir, data),(startv,endv)) => (
        (* Would be much nicer with a list of booleans and pattern matching
         * but not really worth it...?  *)
          if ((V.NTab.name_eq (v1, startv) andalso
            V.NTab.name_eq (v2, endv)) orelse ( V.NTab.name_eq (v2, startv) andalso
              V.NTab.name_eq (v1, endv))) 
          then 
            let
              val (nn, g') = g |> delete_vertex v1
                               |> delete_vertex v2
                               (* Could be endv, does not change anything in
                                * this case  *)
                               |> add_vertex_named_like startv OVData.EVert
             in
             ((nn, nn), g' |> add_edge (dir, data) nn nn)
             end
          else
            ((startv,endv),
              g |> delete_vertex v1
                |> delete_vertex v2
                |> add_edge (dir, data) startv endv)));

  (* Cut edges around a vertex if they are not in the keepvset; change cut 
     incident edges into half-edges (add a boundary vertex to each one). 
     Creates V.InjEndo.T which explains how to glue cut back together; 
     automorphism is of the form: keepvset vertex => outside vertex.  
     NOT** INVARIANT: preserves names of the keepset, creates new 
     edges/boundary vertices for the rest of the graph. 
  *)
  fun cut_around_vertex keepvset n (vs_g as (amorph,g)) = 
      (case get_vertex g n of (OVData.EVert,_) => vs_g
       | (_,(ins,outs)) => 
         vs_g
          |> E.NSet.fold 
              (fn e => fn (newvs,g) => 
                let val (d,(fromv,_)) = get_edge g e in 
                  if V.NSet.contains keepvset fromv then (newvs, g)
                  else
                    let val (newv1,g1) = add_vertex OVData.EVert g 
                        val (newv2,g2) = add_vertex OVData.EVert g1
                        val (_,g3) = add_edge d fromv newv2 g2
                    in
                      (amorph |> VInjEndo.add newv1 newv2,
                       g3 |> update_edges (E.NTab.update (e, (d,(newv1,n))))
                          |> add_to_boundary newv1)
                    end
                end)
              ins 
          |> E.NSet.fold 
              (fn e => fn (newvs,g) => 
                let val (d,(_,tov)) = get_edge g e in 
                  if V.NSet.contains keepvset tov then (newvs, g)
                  else
                    let val (newv1,g1) = add_vertex OVData.EVert g 
                        val (newv2,g2) = add_vertex OVData.EVert g1
                        val (_,g3) = add_edge d newv2 tov g2
                    in
                      (amorph |> VInjEndo.add newv1 newv2,
                       g3 |> update_edges (E.NTab.update (e, (d,(n,newv1))))
                          |> add_to_boundary newv1)
                    end
                end)
              outs);
    
  (* Property to be checked -- all boundary vertices have degree one *)
  (* This is not maintained but can be checked with the is_correct function*) 
  (* in particular, the graph formalism allows hyper-edges 
     (internal points that branch) *)
  fun is_vertex_concrete_correct g n = 
      if is_boundary g n then
        let val (_,(ins,outs)) = (get_vertex g n) in
          (E.NSet.is_empty ins andalso E.NSet.is_singleton outs) 
          orelse
          (E.NSet.is_empty outs andalso E.NSet.is_singleton ins) 
        end
      else true
  fun is_concrete_correct g = 
      V.NSet.fold (fn n => fn b => (is_vertex_concrete_correct g n) 
                                   andalso b)  
                  (get_boundary g)
                  true;
  
  (* if n1 is boundary and something else is already called n2, then
  merge n1 and the vertex called n2, used for bang-box copy *)
  
  (* rename n1 to n2, if n2 already exists, if several vertices have
     non-boundary data, its assumed to be the same. 
     edges; get unioned
     Note: I've not really thought about what happens if n1 = n2, currently id. 
  *)
  fun rename_or_merge_vname n1 n2 bg =
      if V.name_eq (n1,n2) then bg (* should raise an error? *)
      else 
      (case lookup_vertex bg n2 
        of NONE => bg |> rename_vname n1 n2
         | SOME (d2,(ins2,outs2)) => (* clash, so merge egdes to n1 into n2 *)
           let val (d1,(ins1,outs1)) = get_vertex bg n1
           in
             bg |> (case d1 
                     of OVData.EVert => update_boundary (V.NSet.delete n1)
                      | OVData.IVert _ => 
                        (case d2 
                          of OVData.EVert => (* update boundary and data *)
                             update_boundary (V.NSet.delete n2)
                             o set_vertex_data d1 n2
                           | OVData.IVert _ => I)) (* nothing to do *)
                (* update edges *)
                |> E.NSet.fold (update_edge_vnames 
                                     (fn (from,_) => (from,n2))) ins1 
                |> E.NSet.fold (update_edge_vnames 
                                     (fn (_,to) => (n2,to))) outs1 
                |> update_vertex_enames (K (E.NSet.union_merge ins1 ins2,
                                            E.NSet.union_merge outs1 outs2)) n2
                (* drop boundary node n1; keep what it was merged with *)
                |> update_vertices (V.NTab.delete n1)
           end);
  
  (* cut around subgraph identified by vns; introduces new half-edges, so that 
     vns is disjoint from the rest of the graphs. *)
  fun cut_around_subgraph vns g = 
      V.NSet.fold 
        (cut_around_vertex vns)
        vns
        (VInjEndo.empty,g);
  

  (* remove any vertices that don't have an edge from the vset and for any vertices not in vset, but with edge(s) to vset, they get changed into edge-points. Note: this is a cheeky use of edge-points for variable-vertices. We should probably introduce a proper notion of variable vertex/uninstantiated vertex. *)
  fun abstract_subgraph_from_vset vset (pvs_g as (_,g)) = 
      E.NTab.fold 
        (fn (_,(_,(fromv,tov))) => fn (pvs_g as (pvs,g)) =>  
          case (V.NSet.contains vset fromv, V.NSet.contains vset tov) 
            of (true,true) => pvs_g
             | (false,true) => (V.NSet.add fromv pvs, change_into_edgepoint fromv g)
             | (true,false) => (V.NSet.add tov pvs, change_into_edgepoint fromv g)
             | (false,false) => pvs_g
        )
        (get_edges g)
        pvs_g;


  (* make a subgraph from a node selection: any vertices with edges
     outside selection become boundary, and all non-selected vertices are 
     deleted. Perserves matching.  *)
  fun matched_subgraph_within_vset' vns g = 
      cut_around_subgraph vns g
      |> (fn (amorph,g') => 
          (amorph, 
           g' |> V.NSet.fold delete_vertex (V.NSet.subtract (get_vnames g) vns)
              |> V.NSet.fold delete_vertex (VInjEndo.get_codset amorph)))
  
  (* as above, but ignores gluing automorphism (introduced boundary => old vs) *)
  val matched_subgraph_within_vset = 
      snd oo matched_subgraph_within_vset';
  
  
  (* make a subgraph from a node selection: any vertices with edges
     outside selection are deleted. NOTE: does not preserve type/Matching. 
     This is the usual graphics package form of "copying" a selected graph.
     (do we want this? or should we do a copy by cutting edges and throwing away 
      stuff outside the cut selection?)
  *)
  fun delete_subgraph_outside_vset vns g = 
      V.NSet.fold 
        (fn n => fn g => if V.NSet.contains vns n then g 
                         else delete_vertex n g)
        (get_vnames g) g;
  
  fun is_input_vertex' (OVData.EVert,(ins,_)) = E.NSet.is_empty ins
    | is_input_vertex' (OVData.IVert _,_) = false;
  fun is_output_vertex' (OVData.EVert,(_,outs)) = E.NSet.is_empty outs
    | is_output_vertex' (OVData.IVert _,_) = false;
  
  (* boundary vertex has either empty ins or empty outs. *)
  fun is_boundary_vertex' v = is_input_vertex' v orelse is_output_vertex' v;
  
  (* implicit: g n *)
  val is_boundary_vertex = is_boundary_vertex' oo get_vertex;
  val is_input_vertex = is_input_vertex' oo get_vertex;
  val is_output_vertex = is_output_vertex' oo get_vertex;
  
  (* goes to or comes from bounary vertex *)
  fun is_boundary_edge g e = 
      let val (_,(s,t)) = get_edge g e 
      in is_boundary_vertex g s orelse is_boundary_vertex g t end;
  
  fun get_edgepoint_vnames g = 
      V.NTab.fold 
        (fn (n,dio) => 
            if is_boundary_vertex' dio then V.NSet.add n
            else I)
        (get_vertices g)
        V.NSet.empty;
  
  (* identify in and out boundary nodes (all edges come in-to or
     out-of this node) and try to preserve them in rhs *)
  fun get_in_and_out_vnames g = 
      V.NSet.fold 
        (fn n => 
         fn (invs,outvs) => 
            let val (_,(ins,outs)) = get_vertex g n
            in
              if E.NSet.is_empty outs then 
                (invs, V.NSet.add n outvs)
              else if E.NSet.is_empty ins then 
                (V.NSet.add n invs, outvs) 
              else (invs,outvs)
            end)
        (get_boundary g)
        (V.NSet.empty, V.NSet.empty);

  (* rewrite to do this efficiently: don't calculate both when you only 
     want one. *)
  val get_inputs = fst o get_in_and_out_vnames;
  val get_outputs = fst o get_in_and_out_vnames;
  
  (* IMPROVE: move these into graph *)
  fun pair_from_of_edge g e = 
      let val (ed,(from,_)) = get_edge g e in (e,ed,from) end;
  fun pair_to_of_edge g e = 
      let val (ed,(_,to)) = get_edge g e in (e,ed,to) end;

  fun is_wire_vertex g v = case get_vertex_data g v of OVData.EVert => true | _ => false
  val is_node_vertex = not oo is_wire_vertex
  fun get_wire_vertices g = V.NSet.filter (is_wire_vertex g) (get_vnames g)
  fun get_node_vertices g = V.NSet.filter (is_node_vertex g) (get_vnames g)

  fun wv_get_other_edge g wv e = let
    val (_,(in_e,out_e)) = get_vertex g wv
  in if E.NSet.contains in_e e
     then if E.NSet.contains out_e e then NONE else E.NSet.tryget_singleton out_e
     else E.NSet.tryget_singleton in_e
  end
  
  fun edge_get_other_vertex g e v = let
    val (_,(s,t)) = get_edge g e
  in if V.name_eq(s,v) then t else s
  end
  
  (* use lexicographic order *)
  fun wire_ord ( (s,t,w,e) , (s',t',w',e') ) =
    prod_ord (prod_ord V.name_ord V.name_ord) (prod_ord V.NSet.ord E.NSet.ord)
    (   ((s,t),(w,e))   ,   ((s',t'),(w',e'))   )
  
  fun wire_eq w1 w2 = wire_ord (w1, w2) = EQUAL
  
  fun pretty_wire (s,t,w,e) = Pretty.list "wire(" ")" [V.pretty_name s, V.pretty_name t, V.NSet.pretty w, E.NSet.pretty e]

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

  fun get_wires_for_edges g es = let
    fun gw es' = if E.NSet.is_empty es' then []
                else let val wire = get_wire_containing_edge g ((the o
                E.NSet.get_min) es')
                     in wire :: gw (es' |> E.NSet.remove_set (#4 wire))
                     end
  in gw es
  end
  
  fun get_wire_list g = get_wires_for_edges g (get_enames g)

  fun delete_wire (_,_,wvs,es) h = V.NSet.fold delete_vertex wvs (E.NSet.fold delete_edge es h)

  type Wire = V.name * V.name * V.NSet.T * E.NSet.T;
  exception wire_exp of (string * T * (V.name * V.name * V.NSet.T * E.NSet.T));

  fun assert_wire_coherent g (w as (_,_,_,es)) = let
    val sample_edge = the (E.NSet.get_local_bot es)
    val ((dir_or_undir,data),_) = get_edge g sample_edge
  in
    (* FIXME: check all in same direction if directed *)
    E.NSet.fold (fn e => fn w =>
      let
        val ((dou',data'),_) = get_edge g e
      in
        if (dir_or_undir <> dou')
        then raise wire_exp ("Directedness of edges differs",g,w)
        else if not (EData.data_eq (data,data'))
        then raise wire_exp ("Data of edges differs",g,w)
        else w 
      end) es w
  end

  fun is_wire_minimal _ (_,_,wvs,_) = V.NSet.is_empty wvs

  fun minimise_wire' (wire as (s,t,_,es)) g = let
    val (dir_and_data,_) = get_edge g ((the o E.NSet.get_local_bot) es)
    val (e,g') = g |> add_edge dir_and_data s t
  in
    ((s,t,V.NSet.empty,E.NSet.single e),
     g' |> delete_wire wire)
  end
  fun minimise_wire wire g = let
    val _ = assert_wire_coherent g wire
  in
    if is_wire_minimal g wire
    then (wire,g)
    else minimise_wire' wire g
  end
  val do_minimise_wire = snd oo minimise_wire

  fun is_wire_normalised g (s,t,wvs,_) =
    if is_node_vertex g s andalso is_node_vertex g t then
      V.NSet.cardinality wvs = 2
    else
      V.NSet.is_empty wvs

  fun normalise_wire' (wire as (s,t,_,es)) g =
    if is_wire_vertex g s orelse is_wire_vertex g t then
      (* add no extra wire-verts on boundary wires, bare wires, or circles *)
      minimise_wire' wire g
    else
      (* add two extra wire verts on internal wires *)
      let
        val (dir_and_data,_) = get_edge g ((the o E.NSet.get_local_bot) es)
        val (b1,g) = g |> add_vertex OVData.EVert
        val (b2,g) = g |> add_vertex OVData.EVert
        val (e1,g) = g |> add_edge dir_and_data s b1
        val (e2,g) = g |> add_edge dir_and_data b1 b2
        val (e3,g) = g |> add_edge dir_and_data b2 t
      in
        ((s,t,V.NSet.of_list [b1,b2],E.NSet.of_list [e1,e2,e3]),
         g |> delete_wire wire)
      end
  fun normalise_wire wire g = let
    val _ = assert_wire_coherent g wire
  in
    if is_wire_normalised g wire
    then (wire,g)
    else normalise_wire' wire g
  end
  val do_normalise_wire = snd oo normalise_wire

  fun minimise g = g |> fold do_minimise_wire (get_wire_list g)
  fun normalise g = g |> fold do_normalise_wire (get_wire_list g)

  exception plugging_exp of (T * V.name * V.name);

  val are_pluggable = is_some ooo get_plugging
  fun plug v1 v2 g =
    case get_plugging g v1 v2 of
         NONE => raise plugging_exp (g,v1,v2)
         (* FIXME: direction *)
       | SOME (ed,(v1',v2')) => add_edge ed v1' v2' g;
  val do_plug = snd ooo plug
  fun plug_and_minimise g v1 v2 = let
    val (e,g') = plug g v1 v2
  in
    minimise_wire (get_wire_containing_edge g' e) g'
  end
  val do_plug_and_minimise = snd ooo plug_and_minimise
  fun plug_and_normalise g v1 v2 = let
    val (e,g') = plug g v1 v2
  in
    normalise_wire (get_wire_containing_edge g' e) g'
  end
  val do_plug_and_normalise = snd ooo plug_and_normalise

  (* finds the open subgraph defined by a node-vertex set *)
  (* assumes graph is normalised *)
  (* returns a set of vertices *)
  fun find_open_subgraph vset g = let
    fun add_edge_source e = V.NSet.add (get_edge_source g e)
    fun add_edge_target e = V.NSet.add (get_edge_target g e)
    fun absorb vn =
      case get_vertex g vn of
           (OVData.EVert,_) => V.NSet.add vn
         | (_,(ins,outs)) =>
             (E.NSet.fold add_edge_source ins)
             o (E.NSet.fold add_edge_target outs)
             o (V.NSet.add vn)
  in
    V.NSet.fold absorb vset vset
  end

  fun get_open_subgraph vset g = let
    val retained = find_open_subgraph vset g
    val removed = V.NSet.subtract (get_vnames g) retained
  in
    V.NSet.fold delete_vertex removed g
  end

end; (* OpenGraphFun *)
