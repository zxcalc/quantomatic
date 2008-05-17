(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Ross' Quantum Graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graphs with Boundary *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature BGRAPH = 
sig

structure Graph : BASIC_GRAPH

include BASIC_GRAPH;

val get_graph : T -> Graph.T
val get_boundary : T -> Vertex.NSet.T
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* A  Graph With Boundary *)
functor BGraphFun(
	structure G : BASIC_GRAPH
)
= struct

structure V = G.Vertex;
structure E = G.Edge;
structure Graph = G;

(* Property to be checked -- all boundary vertices have degree one *)
(* This is not maintained but can be checked with the is_correct function*) 
datatype T = BGraph of 
     { graph : Graph.T,
       boundary : V.NSet.T
     };

exception no_such_vname_exp of string * V.name * T;
exception vname_already_exists_exp of V.name * T;

exception no_such_ename_exp of string * E.name * T;
exception ename_already_exists_exp of E.name * T;

val empty = BGraph { graph = Graph.empty, 
                     boundary = V.NSet.empty };

fun update_graph f (BGraph rep) = 
    BGraph { graph = f (#graph rep), 
             boundary = #boundary rep }; 

fun update_boundary f (BGraph rep) = 
    BGraph { graph = #graph rep, 
             boundary = f(#boundary rep) };

val set_graph = update_graph o K;
val set_boundary = update_boundary o K;
val add_boundary_vertex = update_boundary o V.NSet.add;
val add_boundaries_vertices = update_boundary o V.NSet.union_merge;
val del_boundary_vertex = update_boundary o V.NSet.delete;
val del_boundary_vertices = update_boundary o V.NSet.subtract;

fun get_graph (BGraph rep) = #graph rep;
fun get_boundary (BGraph rep) = #boundary rep;

val get_vertices = Graph.get_vertices o get_graph;
val get_edges = Graph.get_edges o get_graph;
val get_vertex_list = V.NTab.dest o get_vertices;
val get_edge_list = E.NTab.dest o get_edges;

(* implicit args: bg n *)
val get_vertex = Graph.get_vertex o get_graph;
val get_edge = Graph.get_edge o get_graph;
val lookup_vertex = Graph.lookup_vertex o get_graph;
val lookup_edge = Graph.lookup_edge o get_graph;



fun is_boundary (BGraph rep) n = 
    V.NSet.contains (#boundary rep) n


fun is_vertex_correct bg n = 
    if is_boundary bg n then
      let val (_,(ins,outs)) = (get_vertex bg n) in
	(E.NSet.is_empty ins andalso E.NSet.is_singleton outs) 
	orelse
	(E.NSet.is_empty outs andalso E.NSet.is_singleton ins) 
      end
    else
      true

fun is_correct bg = 
    V.NSet.fold (fn n => fn b => (is_vertex_correct bg n) andalso b)  
	       (get_boundary bg)
	       true
    
fun add_named_vertex n v isBoundary bg =
    let val (n2,g2) = Graph.add_named_vertex n v (get_graph bg)
    in (n2, 
	bg |> (if isBoundary then add_boundary_vertex n2 else I)
	   |> set_graph g2) 
    end;

fun new_named_vertex n v isBoundary bg =
    let val g2 = Graph.new_named_vertex n v (get_graph bg)
    in 	bg |> (if isBoundary then add_boundary_vertex n else I)
	   |> set_graph g2
    end;
    
(* implicit args: en e n1 n2 bg *)
fun add_named_edge en e n1 n2 bg = 
    let val (en2,g2) = Graph.add_named_edge en e n1 n2 (get_graph bg) 
    in (en2, set_graph g2 bg) end;
(* implicit: bg *)
fun new_named_edge en e n1 n2 =
    update_graph (Graph.new_named_edge en e n1 n2);

(* deleting vertex also removes all it's edges. *)
fun delete_vertex n bg = 
    bg |> (if (is_boundary bg n) then del_boundary_vertex n else I)
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
(* implicit arg: f n bg *)
val update_vertex = update_graph oo Graph.update_vertex;
(* implicit arg: f en bg *)
val update_edge = update_graph oo Graph.update_edge;
(* implicit arg: v n bg *)
val set_vertex = update_graph oo Graph.set_vertex;
(* implicit arg: en e bg *)
val set_edge = update_graph oo Graph.set_edge;

(* useup names... *)
val useup_vnames = update_graph o Graph.useup_vnames;
val useup_enames = update_graph o Graph.useup_enames;

(* implicit args: g1 g2: rename g2 to avoid names in g1 *)
val rename_apart = 
		update_graph o (Graph.rename_apart o get_graph);


(*-------------------------------------------------------*)
(* prettifying *)
(*-------------------------------------------------------*)

fun pretty bg = 
    Pretty.chunks 
    [ Pretty.str "BGraph{",
      Pretty.block 
        [Pretty.str "  ", (* indent *)
         Pretty.chunks 
           [Pretty.block 
              [Pretty.str "Boundary Vertices: ",
               Pretty.list "{" "}" 
                           (map V.Name.pretty (V.NSet.dest (get_boundary bg)))],
            Graph.pretty (get_graph bg)]],
      Pretty.str "}"
    ];

val print = Pretty.writeln o pretty;

end;
(*======================= E N D =============================*)



(* ------------------------------------------------------- *)
(* define some concrete structures ----- *)
(*-------------------------------------------------------*)

structure DB_BGraph = BGraphFun(V,E);
structure BGraph : BGRAPH = DB_Graph;

