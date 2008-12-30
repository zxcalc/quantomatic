(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Ross' Quantum Graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graphs with Boundary *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature BGRAPH = 
sig
include GRAPH;
structure BVertex : BVERTEX
sharing Vertex = BVertex
structure IVertex : VERTEX
sharing IVertex = BVertex.IVertex

val get_boundary : T -> Vertex.NSet.T
val is_boundary : T -> Vertex.name -> bool

val rename_or_merge_vname : Vertex.name -> Vertex.name -> T -> T

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* A  Graph With Boundary *)
functor BGraphFun(
        structure BVertex : BVERTEX
        structure Edge : EDGE
) : BGRAPH
= struct

structure IVertex = BVertex.IVertex;
structure BVertex = BVertex;
structure V = BVertex;
structure E = Edge;

structure BasicBGraph =
struct
  structure Vertex = V and Edge = E;
  structure Graph = BasicGraphFun(structure Vertex = V and Edge = E);

  datatype T = BGraph of 
           { graph : Graph.T,
             boundary : V.NSet.T
           };

  exception no_such_vname_exp of string * Vertex.name * T;
  exception vname_already_exists_exp of Vertex.name * T;
            
  exception no_such_ename_exp of string * Edge.name * T;
  exception ename_already_exists_exp of Edge.name * T;

  val empty = BGraph { graph = Graph.empty, 
                       boundary = V.NSet.empty };

  fun get_graph (BGraph rep) = #graph rep;
  fun get_boundary (BGraph rep) = #boundary rep;

  fun update_graph f (BGraph rep) = 
      BGraph { graph = f (#graph rep), 
               boundary = #boundary rep }; 

  fun update_boundary f (BGraph rep) = 
      BGraph { graph = #graph rep, 
               boundary = f(#boundary rep) };
  val set_graph = update_graph o K;
  val set_boundary = update_boundary o K;
  val add_to_boundary = update_boundary o Vertex.NSet.add;
  val del_from_boundary = update_boundary o Vertex.NSet.delete;
  val is_boundary = V.NSet.contains o get_boundary;
  val get_vertex = Graph.get_vertex o get_graph;

  (* Property to be checked -- all boundary vertices have degree one *)
  (* This is not maintained but can be checked with the is_correct function*) 
  fun is_vertex_correct bg n = 
      if is_boundary bg n then
        let val (_,(ins,outs)) = (get_vertex bg n) in
	        (E.NSet.is_empty ins andalso E.NSet.is_singleton outs) 
	        orelse
	        (E.NSet.is_empty outs andalso E.NSet.is_singleton ins) 
        end
      else true
  fun is_correct bg = 
      V.NSet.fold (fn n => fn b => (is_vertex_correct bg n) andalso b)  
	                (get_boundary bg)
	                true;

  fun add_named_vertex n v bg =
      let val (n2,g2) = Graph.add_named_vertex n v (get_graph bg)
      in (n2, bg |> (case v of (V.BVert _) => add_to_boundary n2 
                             | (V.IVert _) => I)
	               |> set_graph g2)
      end;

  fun add_named_edge en e n1 n2 bg = 
      let val (en2,g2) = Graph.add_named_edge en e n1 n2 (get_graph bg) 
      in (en2, set_graph g2 bg) end;

  (* deleting vertex also removes all it's edges. *)
  fun delete_vertex n bg = 
      bg |> del_from_boundary n (* if not in boundary, does nothing *)
         |> update_graph (Graph.delete_vertex n);
      
  (* implicit args: en bg *)
  val delete_edge = update_graph o Graph.delete_edge;

  (* vname = suggested name *)
  (* rename all occurances of old name (1st arg) with new name (2nd arg) 
     n2 must not exist already, n1 must exist *)
  fun rename_vname n1 n2 bg = 
      if (is_boundary bg n1) then 
        bg |> update_boundary (V.NSet.delete n1 o V.NSet.add n2)
           |> update_graph (Graph.rename_vname n1 n2)
      else
        bg |> update_graph (Graph.rename_vname n1 n2);

  (* val implicit args: en1 en2 bg *)
  val rename_ename = update_graph oo Graph.rename_ename;

  val update_vertex = update_graph oo Graph.update_vertex;
  val update_edge = update_graph oo Graph.update_edge;

  val get_vertices = Graph.get_vertices o get_graph;
  val get_edges = Graph.get_edges o get_graph;

  val update_vertices = update_graph o Graph.update_vertices;
  val update_edges = update_graph o Graph.update_edges;

  (* prettifying *-------------------------------------------------*)
  fun pretty bg = 
      Pretty.chunks 
        [ Pretty.str "BGraph{",
          Pretty.block 
            [Pretty.str "  ", (* indent *)
             Pretty.chunks 
               [Pretty.block 
                  [Pretty.str "Boundary Vertices: ",
                   Pretty.list "{" "}" 
                               (map V.pretty_name (V.NSet.list_of (get_boundary bg)))],
                Graph.pretty (get_graph bg)]],
          Pretty.str "}"
        ];
      
  val print = Pretty.writeln o pretty;
      
end; (* Basic BGraph*)

structure BGraphTools = GraphToolsFun(BasicBGraph);
open BasicBGraph;
open BGraphTools;


  (* if n1 is boundary and something else is already called n2, then
merge n1 and the vertex called n2: used for non-strict matching where
boundary vertices can become unified to the same tgt vertex. *)
  fun rename_or_merge_vname n1 n2 bg =
      if (is_boundary bg n1) then 
        case lookup_vertex bg n2 
         of NONE => (* no clash, so rename n1 to n2 in boundary and graph *)
            bg |> update_boundary (V.NSet.delete n1 o V.NSet.add n2)
               |> BasicBGraph.update_graph (BasicBGraph.Graph.rename_vname n1 n2)
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
        bg |> BasicBGraph.update_graph (BasicBGraph.Graph.rename_vname n1 n2);

end;
