(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Ross' Quantum Graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graphs with Boundary *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature OGRAPH_SHARING = 
sig 
include BASIC_GRAPH_SHARING;
(* structure Edge : EDGE_AND_DATA_SHARING;
structure Vertex : NAME_AND_DATA_SHARING; *)
structure OVertex : OVERTEX_SHARING;
sharing Vertex = OVertex;
end;

signature OGRAPH = 
sig
include GRAPH
structure OVertex : OVERTEX
sharing Vertex.SharingWithData = OVertex.SharingWithData
structure SharingOGraph : OGRAPH_SHARING
sharing SharingBasicGraph = SharingOGraph
sharing SharingOGraph.OVertex = OVertex.SharingWithIData

(* points (edge and boundary, both kinds of vertices) 
   have no data. Boundary have only input edges, 
   or only output edges. Edgepoints have both input and output edges *)

(* boundaries represent end of a half-edge.  *)
val get_boundary : T -> Vertex.NSet.T
val is_boundary : T -> Vertex.name -> bool
val is_boundary_edge : T -> Edge.name -> bool
(* get the subsets of boundary, divided into either input or output. *)
val get_in_and_out_vnames : T -> Vertex.NSet.T (* input *) 
                                 * Vertex.NSet.T (* output *)

(* edge points; parts of an edge *)
val get_edgepoints : T -> Vertex.NSet.T
val is_edgepoint : T -> Vertex.name -> bool
(* get points living on an edge; but not boundary ones *)
val get_edgepoint_vnames : T -> Vertex.NSet.T

(* change vertex into a point (throw away data, making this an edge-point or a boundary point) *)
val change_into_point : Vertex.name -> T -> T
val change_into_vertex : Vertex.name -> OVertex.IData.data -> T -> T

val rename_or_merge_vname : Vertex.name -> Vertex.name -> T -> T

(* Given selection set of vertices, make subgraph: 
   incident edges outside selection become boundary, 
   respecting vertex love *)
val cut_subgraph_of_selection : Vertex.NSet.T -> T -> T
val cut_subgraph_of_selection' : Vertex.NSet.T -> T -> Vertex.NSet.T * T

(* Given selection set of vertices, make subgraph: 
   vertices with incident edges outside selection become boundary *)
val matched_subgraph_within_selection : Vertex.NSet.T -> T -> T
val matched_subgraph_within_selection' : Vertex.NSet.T -> T -> Vertex.NSet.T * T

(* Given selection set of vertices, make subgraph: 
   all incident edges to selection are removed. = copy for graphics program *)
val pull_subgraph_within_selection : Vertex.NSet.T -> T -> T

(* 
val adj_tgt_info : 
    T -> Vertex.name 
    -> Vertex.data 
       * (Edge.data Edge.NTab.T (* directed self-loops *)
          * Edge.data Edge.NTab.T (* undireted self-loops *)
          * Vertex.NSet.T) (* other adj vertices *)
*)

(* adjacent information next to an edge *)
(* datatype adj_info = 
  AdjInfo of { undir_bndry : (Edge.Ehn.name * Edge.data) Vertex.NTab.T,
               in_bndry : (Edge.Ehn.name * Edge.data) Vertex.NTab.T,
               out_bndry : (Edge.Ehn.name * Edge.data) Vertex.NTab.T,
               dir_loops : Edge.data Edge.NTab.T,
               undir_loops : Edge.data Edge.NTab.T,
               other_vs : Vertex.NSet.T
             };

val adj_info : 
    T -> Vertex.name 
    -> Vertex.data 
       * ((Edge.Ehn.name * Edge.data) Vertex.NTab.T (* undirected boundary *)
          * (Edge.Ehn.name * Edge.data) Vertex.NTab.T (* incoming boundary *)  
          * (Edge.Ehn.name * Edge.data) Vertex.NTab.T (* outgoing boundary *)
          * Edge.data Edge.NTab.T (* direted self-loops *)
          * Edge.data Edge.NTab.T (* undirected self-loops *)
          * Vertex.NSet.T) (* other adj vertices *)
*)

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  A Graph With Boundary *)
functor OGraphFun(
        structure OVertex : OVERTEX
        structure Edge : EDGE
        type data (* graph additional extensible/hidden data *)
        val empty_data : data
) 
: OGRAPH
= struct

structure OVertex = OVertex;
structure V = OVertex;
structure E = Edge;

(* to avoid using clobbered data and empty graph from the opened Basic Graph *)
type data2 = data;
val empty_data2 = empty_data;

(* we add boundary to Graph data, as well as edge-points *)
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

structure BasicOGraph =
struct 
  (* make basic graph with this as internal data *)
  structure Graph = BasicGraphFun(
    structure Vertex = V 
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

  (* for boundary part of added data *)
  val get_boundary = get_boundary_of_hdata o Graph.get_data;
  val update_boundary = Graph.update_data o update_boundary_of_hdata;
  val set_boundary = update_boundary o K;
  (* for edgepoints *)
  val get_edgepoints = get_edgepoints_of_hdata o Graph.get_data;
  val update_edgepoints = Graph.update_data o update_edgepoints_of_hdata;
  val set_edgepoints = update_edgepoints o K;

  (* *)
  val add_to_boundary = update_boundary o Vertex.NSet.add;
  val del_from_boundary = update_boundary o Vertex.NSet.delete;
  val is_boundary = V.NSet.contains o get_boundary;

  fun is_endpoint' (d,(ins,outs)) = 
    (case (V.NSet.is_empty ins, V.NSet.is_empty outs) of
     (* FIXME: XOR in ML ?*)
       (true, true) => false
     | (false,true) => true
     | (true,false) => true
     | (false,false) => false);
  val is_edgepoint = V.NSet.contains o get_edgepoints;
  val add_to_edgepoints = update_edgepoints o Vertex.NSet.add;
  val del_from_edgepoints = update_edgepoints o Vertex.NSet.delete;

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

  fun add_named_vertex n v g =
      let val (n2,g2) = Graph.add_named_vertex n v g
      in (n2, g2 |> (case v of (V.EVert) => add_to_edgepoints n2 
                             | (V.IVert _) => I))
      end;
  (* Note: deleting vertex also removes all it's edges. *)
  fun delete_vertex n g = 
      g |> del_from_boundary n (* if not in boundary, does nothing *)
        |> del_from_edgepoints n (* if not in edgepoints, does nothing *)
        |> Graph.delete_vertex n;

  (* *)
  fun update_point_kind_from_del_edge n g = 
      let val (d,(ins,outs)) = get_vertex g n in
        case d of 
          V.IVert _ => g
        | V.EVert => 
          (case (V.NSet.is_empty ins, V.NSet.is_empty outs) of
             (true, true) => g (* maybe had many incident edges *)
           | (false,true) => g |> add_to_boundary n
           | (true,false) => g |> add_to_boundary n
           | (false,false) => g |> del_from_boundary)
      end;

  (* *)
  fun update_point_kind_from_add_edge g n = 
      let val (d,(ins,outs)) = get_vertex g n in
        case d of 
          V.IVert _ => g
        | V.EVert => 
          (case (V.NSet.is_empty ins, V.NSet.is_empty outs) of
             (true, true) => g |> del_from_boundary n |> add_to_edgepoints n
           | (false,true) => g |> add_to_boundary n
           | (true,false) => g |> add_to_boundary n
           | (false,false) => raise bug_exp "OGraph: update_point_kind_from_add_edge: newly connected vertex still has no incident edges")
      end;

  (* *)
  fun add_named_edge en e n1 n2 g = 
      let val (en2,g2) = Graph.add_named_edge en e n1 n2 g in 
        (en2, 
         g2 |> update_point_kind_from_add_edge n1 
            |> update_point_kind_from_add_edge n2)
      end;

  fun delete_edge en g = 
      let val (e,(fromv,tov)) = get_edge g en in
        g |> Graph.delete_edge en
          |> update_point_kind_from_del_edge fromv
          |> update_point_kind_from_del_edge tov
      end;

      
  (* vname = suggested name *)
  (* rename all occurances of old name (1st arg) with new name (2nd arg) 
     n2 must not exist already, n1 must exist *)
  fun rename_vname n1 n2 g = 
      g |> rename1_in_boundary n1 n2
        |> rename1_in_edgepoints n1 n2 
        |> Graph.rename_vname n1 n2;

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
                Graph.pretty g]],
          Pretty.str "}"
        ];
      
  val print = Pretty.writeln o pretty;

end; (* BasicOGraph *)

structure OGraphTools = GraphToolsFun(BasicOGraph);
open BasicOGraph;
open OGraphTools;

structure SharingOGraph = 
struct
open SharingBasicGraph;
structure OVertex = OVertex.SharingWithIData;
end;

(* change n to become a boundary vertex *)
fun change_into_point n g = 
    g |> (if is_endpoint' (get_vertex g n)
          then add_to_boundary else add_to_edgepoints) n
      |> set_vertex_data V.EVert n;

(* change n to become a boundary vertex *)
fun change_into_internal n d g = 
    g |> del_from_boundary n
      |> del_from_edgepoints n
      |> set_vertex_data (V.IVert d) n;

(* remove a vertex, and change incident edges to into half-edges (add a 
   boundary vertex to each one)  *)
        (* THINK: isn't there a way to avoid this repeated very similar code? 
           FIXME: YES! make a double fold function for incident edges, fold over ins and outs, and provides info as to what this one is, what it's opposite is. 
        *)
fun cutout_internal_point n g = 
    let val (d,(ins,outs)) = get_vertex g n
    in
      g |> E.NSet.fold 
          (fn e => fn g => 
            let val (d,(fromv,tov)) = get_edge g e
                val (newv,g') = add_vertex V.EVert g
            in 
              g' |> update_edges (Edge.NTab.update (e, (d,(fromv,newv))))
                 |> add_to_boundary newv
            end
           end)
          ins 
        |> E.NSet.fold 
          (fn e => fn g => 
            let val (d,(fromv,tov)) = get_edge g e
                val (newv,g') = add_vertex V.EVert g
            in 
              g' |> update_edges (Edge.NTab.update (e, (d,(newv,tov))))
                 |> add_to_boundary newv
            end
           end)
          outs
        |> delete_vertex n 
    end;

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
merge n1 and the vertex called n2: used for non-strict matching where
boundary vertices can become unified to the same tgt vertex. *)

(* rename n1 to n2, if n2 already exists, merge names and destination
   edges; keep non-boundary data; if several vertices have
   non-boundary data, its assumed to be the same. 
   Note: for general use, assumes n1s are distinct from n2s.
*)
fun rename_or_merge_vname n1 n2 bg =
    if Vertex.name_eq (n1,n2) then bg (* should raise an error? *)
    else 
    (case lookup_vertex bg n2 
      of NONE => bg |> rename_vname n1 n2
       | SOME (d2,(ins2,outs2)) => (* clash, so merge egdes to n1 into n2 *)
         let val (d1,(ins1,outs1)) = get_vertex bg n1
         in
           bg |> (case d1 
                   of V.EVert => update_boundary (V.NSet.delete n1)
                    | V.IVert id2 => 
                      (case d2 
                        of V.EVert => (* update boundary and data *)
                           update_boundary (V.NSet.delete n2)
                           o set_vertex_data d1 n2
                         | V.IVert id1 => I)) (* nothing to do *)
              (* update edges *)
              |> Edge.NSet.fold (update_edge_vnames 
                                   (fn (from,to) => (from,n2))) ins1 
              |> Edge.NSet.fold (update_edge_vnames 
                                   (fn (from,to) => (n2,to))) outs1 
              |> update_vertex_enames (K (Edge.NSet.union_merge ins1 ins2,
                                          Edge.NSet.union_merge outs1 outs2)) n2
              (* drop boundary node n1; keep what it was merged with *)
              |> update_vertices (Vertex.NTab.delete n1)
         end);

(* all edges leaving the selection get cut at the next vertex,
   introducing bounary vertices. All introduced bounary vertices only have edges 
   into the selection (vns). Strictly increases set of matched graphs. *)
fun cut_subgraph_of_selection' vns g = 
    let val new_boundary_vs = incident_vertices g vns;
    in
    (* cut at all vertices outside of the selection *)
      (new_boundary_vs, 
       V.NSet.fold 
        (fn n => fn g => 
                    if V.NSet.contains vns n then g 
                    else if V.NSet.contains new_boundary_vs n 
                    then 
                      g |> set_vertex_data V.EVert n
                        (* delete any edges not incident to subgraph *)
                        |> (Edge.NTab.fold 
                              (fn (e,n2) => 
                                  if V.NSet.contains vns n2 then I 
                                  else delete_edge e)      
                              (adj_etab_to_vnames g n Edge.NTab.empty))
                    else delete_vertex n g)
        (get_vnames g) g)
    end;

val cut_subgraph_of_selection = snd oo cut_subgraph_of_selection';

(* make a subgraph from a node selection: any vertices with edges
   outside selection become boundary, and all non-selected vertices are 
   deleted. Perserves matching.  *)
fun matched_subgraph_within_selection' vns g = 
    V.NSet.fold 
      (fn n => fn (newbs, g) => 
                  if V.NSet.contains vns n then 
                    if V.NSet.is_empty 
                         (V.NSet.subtract (adj_vnames g n) vns)
                    then (newbs, g)
                    else (V.NSet.add n newbs,
                          change_into_edgepoint n g)
                  else (newbs, delete_vertex n g))
      (get_vnames g) (V.NSet.empty, g);
    
val matched_subgraph_within_selection = 
    snd oo matched_subgraph_within_selection';


(* make a subgraph from a node selection: any vertices with edges
   outside selection become boundary, and all non-selected vertices
   are deleted. NOTE: does not preserve type/Matching. This is the
   usual graphics package style copy. *)
fun pull_subgraph_within_selection vns g = 
    V.NSet.fold 
      (fn n => fn g => if V.NSet.contains vns n then g 
                       else delete_vertex n g)
      (get_vnames g) g;


(* boundary vertex has either empty ins or empty outs. *)
fun is_boundary_vertex' (OVertex.EVert,(ins,outs)) = 
    (Edge.NSet.is_empty outs orelse Edge.NSet.is_empty ins);
(* implicit: g n *)
val is_boundary_vertex = is_boundary_vertex' oo get_vertex;
(* goes to or comes from bounary vertex *)
fun is_boundary_edge g e = 
    let val (d,(s,t)) = get_edge g e 
    in is_boundary_vertex g s orelse is_boundary_vertex g t end;

fun get_edgepoint_vnames g = 
    Vertex.NTab.fold 
      (fn (n,dio) => 
          if is_boundary_vertex' dio then Vertex.NSet.add n
          else I)
      (get_vertices g)
      Vertex.NSet.empty;

(* identify in and out boundary nodes (all edges come in-to or
   out-of this node) and try to preserve them in rhs *)
fun get_in_and_out_vnames g = 
    Vertex.NSet.fold 
      (fn n => 
       fn (invs,outvs) => 
          let val (_,(ins,outs)) = get_vertex g n
          in
            if Edge.NSet.is_empty ins then 
              (invs, Vertex.NSet.add n outvs)
            else if Edge.NSet.is_empty outs then 
              (Vertex.NSet.add n invs, outvs) 
            else (invs,outvs)
          end)
      (get_boundary g)
      (Vertex.NSet.empty, Vertex.NSet.empty);


(* get adjacent boundary vertices *)
fun get_adj_boundary g n = 
    filtered_adj_vnames (is_boundary_vertex g) g n;


(* IMPROVE: move these into graph *)
fun pair_from_of_edge g e = 
    let val (ed,(from,to)) = get_edge g e in (e,ed,from) end;
fun pair_to_of_edge g e = 
    let val (ed,(from,to)) = get_edge g e in (e,ed,to) end;

(* find all adjacent vertices, except self, divided into: 
   boundary, and others *)
fun adj_tgt_info' g n0 (d,(ins,outs)) = 
    let 
      fun consider_edge (e,ed,n) (loopes, others) = 
          if V.name_eq (n,n0) then 
            (E.NTab.update (e, ed) loopes, others)
          else (loopes, V.NSet.add n others);
    in 
      (d, (E.NTab.empty, V.NSet.empty)
            |> (Edge.NSet.fold (consider_edge o (pair_from_of_edge g)) ins)
            |> (Edge.NSet.fold (consider_edge o (pair_to_of_edge g)) outs))
    end;

fun adj_tgt_info g n =  adj_tgt_info' g n (get_vertex g n);



(* 
(* find all adjacent vertices, except self, divided into: 
   boundary, and others *)
fun adj_info' g n0 (d,(ins,outs)) = 
    let 
      fun consider_edge eh adjinfo = 
          let val n = get_ehn_vname g (Edge.Ehn.other eh) 
              val en = Edge.Ehn.get_ename eh in
          if V.name_eq (n,n0) then  (* loop *)
            (leaftab, E.NTab.update (en,fst (get_edge g en)) loopes, others)
          else if V.NSet.contains others n then adjinfo
          else if V.NTab.contains leaftab n then 
            (V.NTab.delete n leaftab, loopes,
             V.NSet.add n others)
          (* IMPROVE: avoid edge data lookup when we later throw 
             away things from boundary set... *)
          else if is_boundary g n then 
            (V.NTab.ins (n,(eh,fst (get_edge g en))) leaftab, loopes, others)
          else 
            (leaftab, loopes, V.NSet.add n others)
          end;
    in 
      (d, (V.NTab.empty, E.NTab.empty, V.NSet.empty)
            |> (Edge.NSet.fold (consider_edge o Edge.Ehn.mk_end) ins)
            |> (Edge.NSet.fold (consider_edge o Edge.Ehn.mk_start) outs))
    end;

fun adj_info g n = adj_pat_info' g n (get_vertex g n);
*)



(* find all adjacent vertices, except self, divided into: 
   boundary, and others *)
fun adj_pat_info' g n0 (d,(ins,outs)) = 
    let 
      fun consider_edge eh (adjinfo as (leaftab, loopes, others)) = 
          let val n = get_ehn_vname g (Edge.Ehn.other eh) 
              val en = Edge.Ehn.get_ename eh in
          if V.name_eq (n,n0) then 
            (leaftab, E.NTab.update (en,fst (get_edge g en)) loopes, others)
          else if V.NSet.contains others n then adjinfo
          else if V.NTab.contains leaftab n then 
            (V.NTab.delete n leaftab, loopes,
             V.NSet.add n others)
          (* IMPROVE: avoid edge data lookup when we later throw 
             away things from boundry set... *)
          else if is_boundary g n then 
            (V.NTab.ins (n,(eh,fst (get_edge g en))) leaftab, loopes, others)
          else 
            (leaftab, loopes, V.NSet.add n others)
          end;
    in 
      (d, (V.NTab.empty, E.NTab.empty, V.NSet.empty)
            |> (Edge.NSet.fold (consider_edge o Edge.Ehn.mk_end) ins)
            |> (Edge.NSet.fold (consider_edge o Edge.Ehn.mk_start) outs))
    end;

fun adj_pat_info g n = adj_pat_info' g n (get_vertex g n);


end;
