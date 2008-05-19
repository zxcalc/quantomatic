(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Ross' Quantum Graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graphs with Boundary *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature BGRAPH = 
sig
include GRAPH;
structure BVertex : BVERTEX
sharing type BVertex.data = Vertex.data
val get_boundary : T -> Vertex.NSet.T
val is_boundary : T -> Vertex.name -> bool
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* A  Graph With Boundary *)
functor BGraphFun(
        structure Vertex : VERTEX
        structure Edge : EDGE
	      structure Boundary : ORD_DATA (* boundary data *)
)
= struct

  structure V = BVertexFun(
    structure BasicVert = Vertex and Boundary = Boundary
  );
  structure BVertex = V;
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
  val useup_vnames = update_graph o Graph.useup_vnames;
  val useup_enames = update_graph o Graph.useup_enames;

  val get_vertices = Graph.get_vertices o get_graph;
  val get_edges = Graph.get_edges o get_graph;

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
                               (map V.pretty_name (V.NSet.dest (get_boundary bg)))],
                Graph.pretty (get_graph bg)]],
          Pretty.str "}"
        ];
      
  val print = Pretty.writeln o pretty;
      
end; (* Basic BGraph*)

structure BGraphTools = GraphToolsFun(BasicBGraph);
open BasicBGraph;
open BGraphTools;

end;

(* define some concrete structures ----- *)

structure DB_StrBGraph = BGraphFun(
  structure Vertex = StrVertex 
  and Edge = StrEdge 
  and Boundary = StrData);
structure StrBGraph : BGRAPH = DB_StrBGraph;
