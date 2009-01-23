(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Ross' Quantum Graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graphs with Boundary *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature BGRAPH = 
sig
include GRAPH
structure BVertex : BVERTEX
sharing Vertex = BVertex

val get_boundary : T -> Vertex.NSet.T
val is_boundary : T -> Vertex.name -> bool
val change_into_boundary : Vertex.name -> T -> T

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

(* *)
val adj_tgt_info : 
    T -> Vertex.name 
    -> Edge.NSet.T (* names of edges that are self-loops *)
       * Vertex.NSet.T (* other adj vertices *)

val adj_pat_info : 
    T -> Vertex.name 
    -> Edge.name Vertex.NTab.T (* leaf vertex to edge that connects to it *)
       * Edge.NSet.T (* names of edges that are self-loops *)
       * Vertex.NSet.T (* other ajd vertices *)

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  A Graph With Boundary *)
functor BGraphFun(
        structure BVertex : BVERTEX
        structure Edge : EDGE
        type data 
        val empty_data : data
) 
: BGRAPH
= struct

structure BVertex = BVertex;
structure V = BVertex;
structure E = Edge;

(* to avoid using clobbered data and empty graph from the opened Basic Graph *)
type data2 = data;
val empty_data2 = empty_data;

(* we add boundary to Graph data, this is hidden *)
type hdata = { boundary : V.NSet.T,
               data : data};         
val empty_hdata = {boundary = V.NSet.empty,
                   data = empty_data};
fun get_data_of_hdata hdata = #data hdata;
fun update_data_of_hdata f hdata = 
    {boundary = #boundary hdata, 
     data = f (#data hdata)};
fun get_boundary_of_hdata hdata = #boundary hdata;
fun update_boundary_of_hdata f hdata = 
    {boundary = f (#boundary hdata), 
     data = #data hdata};

structure BasicBGraph =
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
  val add_to_boundary = update_boundary o Vertex.NSet.add;
  val del_from_boundary = update_boundary o Vertex.NSet.delete;
  val is_boundary = V.NSet.contains o get_boundary;

  (* modified/new basic graph functions *)
  fun add_named_vertex n v g =
      let val (n2,g2) = Graph.add_named_vertex n v g
      in (n2, g2 |> (case v of (V.BVert) => add_to_boundary n2 
                             | (V.IVert _) => I))
      end;

  (* deleting vertex also removes all it's edges. *)
  fun delete_vertex n g = 
      g |> del_from_boundary n (* if not in boundary, does nothing *)
        |> Graph.delete_vertex n;
      
  (* vname = suggested name *)
  (* rename all occurances of old name (1st arg) with new name (2nd arg) 
     n2 must not exist already, n1 must exist *)
  fun rename_vname n1 n2 g = 
      if (is_boundary g n1) then 
        g |> update_boundary (V.NSet.delete n1 o V.NSet.add n2)
          |> Graph.rename_vname n1 n2
      else
        g |> Graph.rename_vname n1 n2;

  (* prettifying *)
  fun pretty g = 
      Pretty.chunks 
        [ Pretty.str "BGraph{",
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

end; (* BasicBGraph *)

structure BGraphTools = GraphToolsFun(BasicBGraph);
open BasicBGraph;
open BGraphTools;

(* change n to become a boundary vertex *)
fun change_into_boundary n g = 
    g |> add_to_boundary n
      |> set_vertex V.BVert n;


(* Property to be checked -- all boundary vertices have degree one *)
(* This is not maintained but can be checked with the is_correct function*) 
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
fun rename_or_merge_vname n1 n2 bg =
      if (is_boundary bg n1) then 
        case lookup_vertex bg n2 
         of NONE => (* no clash, so rename n1 to n2 in boundary and graph *)
            bg |> update_boundary (V.NSet.delete n1 o V.NSet.add n2)
               |> rename_vname n1 n2
          | SOME (d2,(ins2,outs2)) => (* clash, so merge egdes to n1 into n2 *)
            let val (d1,(ins1,outs1)) = get_vertex bg n1
            in
              bg |> update_boundary (V.NSet.delete n1)
                 |> Edge.NSet.fold (update_edge_vnames (fn (from,to) => (from,n2))) ins1 
                 |> Edge.NSet.fold (update_edge_vnames (fn (from,to) => (n2,to))) outs1 
                 |> update_vertex_enames (K (Edge.NSet.union_merge ins1 ins2,
                                             Edge.NSet.union_merge outs1 outs2)) n2
                 |> update_vertices (Vertex.NTab.delete n1)
            end
      else (* not boundary then rename as normal *)
        bg |> rename_vname n1 n2;

(* all edges leaving the selection get cut at the next vertex,
   introducing bounary vertices. Preserves matching *)
fun cut_subgraph_of_selection' vns g = 
    let val new_boundary_vs = incident_vertices g vns;
    in
    (* cut at all vertices outside of the selection *)
      (new_boundary_vs, 
       V.NSet.fold 
        (fn n => fn g => 
                    if V.NSet.contains vns n then g 
                    else if V.NSet.contains new_boundary_vs n 
                    then set_vertex V.BVert n g else delete_vertex n g)
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
                          change_into_boundary n g) 
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


(* IMPROVE: push the use of these back into graph *)
fun pair_from_of_edge g e = 
    let val (_,(from,to)) = get_edge g e in (e,from) end;
fun pair_to_of_edge g e = 
    let val (_,(from,to)) = get_edge g e in (e,to) end;

(* find all adjacent vertices, except self, divided into: 
   boundary leafs, and others *)
fun adj_tgt_info' g n0 (ins,outs) = 
    let 
      fun consider_edge (e,n) (adjinfo as (loopes, others)) = 
          if V.name_eq (n,n0) then (E.NSet.add e loopes, others)
          else (loopes, V.NSet.add n others);
    in 
      (E.NSet.empty, V.NSet.empty)
         |> (Edge.NSet.fold (consider_edge o (pair_from_of_edge g)) ins)
         |> (Edge.NSet.fold (consider_edge o (pair_to_of_edge g)) outs)
    end;

fun adj_tgt_info g n = adj_tgt_info' g n (snd (get_vertex g n));

(* leaves are boundary nodes with only one edge to them - they are
   special in that they can match on half of a loopy edge (the mapping
   assigns them to the loopy node - and the edge map does a half
   assignment to the edge) *)
fun is_leaf g n = 
    let val (d,(ins,outs)) = get_vertex g n
    in (not (BVertex.is_internal_data d)) andalso 
       ((Edge.NSet.is_empty outs andalso Edge.NSet.is_singleton ins)
        orelse (Edge.NSet.is_singleton outs andalso Edge.NSet.is_empty ins))
    end;

(* find all adjacent vertices, except self, divided into: 
   boundary leafs, and others *)
fun adj_pat_info' g n0 (ins,outs) = 
    let 
      fun consider_edge (e,n) (adjinfo as (leaftab, loopes, others)) = 
          if V.name_eq (n,n0) then 
            (leaftab, E.NSet.add e loopes, others)
          else if V.NSet.contains others n then adjinfo
          else if V.NTab.contains leaftab n then 
            (V.NTab.delete n leaftab, loopes,
             V.NSet.add n others)
          else if is_boundary g n then 
            (V.NTab.ins (n,e) leaftab, loopes, others)
          else 
            (leaftab, loopes, V.NSet.add n others);
    in 
      (V.NTab.empty, E.NSet.empty, V.NSet.empty)
         |> (Edge.NSet.fold (consider_edge o (pair_from_of_edge g)) ins)
         |> (Edge.NSet.fold (consider_edge o (pair_to_of_edge g)) outs)
    end;

fun adj_pat_info g n = adj_pat_info' g n (snd (get_vertex g n));

end;
