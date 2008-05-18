(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graphs with Compact Closed Structure *)
(* Ross Duncan 17-May-2008 *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* 
  | CGraphs have a boundary which is partitioned into a domain 
  | and a codomain; both of these are ordered.  Vertics in the 
  | boundary should have degree exactly one.  These conditions are not
  | enforced, but can be checked via the is_correct function
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 

signature CGRAPH = 
sig

structure Graph : BASIC_GRAPH

include BGRAPH;

val get_graph : T -> CGraph.T
val get_domain : T -> Vertex.Name.T list
val get_codomain : T -> Vertex.Name.T list
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* A  Graph With Boundary *)
functor CGraphFun(V : VERTEX, E : EDGES)
= struct 

structure V = V;
structure E = E;
structure Graph = BGraphFun(V,E);

type vname = E.Name.T;
type vdata = V.data.T;

type ename = V.Name.T;
type edata = E.data.T

exception no_such_vname_exp of string * vname * T;
exception vname_already_exists_exp of vname * T;

exception no_such_ename_exp of string * ename * T;
exception ename_already_exists_exp of ename * T;

(* Property to be checked -- all boundary vertices have degree one *)
(* This is not maintained but can be checked with the is_correct function*) 
datatype T = CGraph of 
     { graph : Graph.T,
       domain : V.Name.T list,
       codomain : V.Name.T list
     };

datatype kind = Int | Dom | Cod;

val empty = CGraph { graph = Graph.empty,
		     domain = [],
		     codomain = []
		   };

fun update_graph f (CGraph rep) = 
    CGraph { graph = f (#graph rep), 
             domain = #domain rep,
             codomain = #codomain rep }; 

fun update_domain f (CGraph rep) =  
    CGraph { graph = #graph rep, 
             domain = f(#domain rep),
             codomain = #codomain rep }; 

fun update_codomain f (CGraph rep) =  
    CGraph { graph = #graph rep, 
             domain = #domain rep,
             codomain = f(#codomain rep) }; 

val set_graph = update_graph o K;
fun get_graph (CGraph rep) = #graph rep;

(* these are some helper functions used to maintain the 
   domain/codomain lists;  these lists should probably be replaced 
   by special Ordered NSet *)
(* insert an element at the nth position of a list.  0 is the front *)
(* if the list is too short element is added at end *)
fun ins_at [] (x,_) = x::[]
  | xs (x,0) = x::xs
  | h::t (x,pos) = h::(insert_at t x (pos-1))

(* delete the first occurence of y in a list;  
   if y does not occur, return the list unchanged *)
fun del_el [] _ = []
  | x::xs y = if x = y then xs else del_el xs y


val set_domain = update_domain o K;
val insert_domain_vertex  = update_domain o (op ::) ;
val insert_domain_vertex_at_pos   = update_domain o ins_at
val add_domain_vertices = update_domain o (op @);
val add_domain_vertices_at = update_domain o (foldl ins_at);
val del_domain_vertex = update_domain o del_el;
val del_domain_vertices = update_domain o (foldl del_el)
fun get_domain (CGraph rep) = #domain rep;
fun is_domain (CGraph rep) n = 
    exists (fn x => x = n) (#domain rep);

val set_codomain = update_codomain o K;
val insert_codomain_vertex  = update_codomain o (op ::) ;
val insert_codomain_vertex_at_pos   = update_codomain o ins_at
val add_codomain_vertices = update_codomain o (op @);
val add_codomain_vertices_at = update_codomain o (foldl ins_at);
val del_codomain_vertex = update_codomain o del_el;
val del_codomain_vertices = update_codomain o (foldl del_el)
fun get_codomain (CGraph rep) = #codomain rep;
fun is_codomain (CGraph rep) n = 
    exists (fn x => x = n) (#codomain rep);

fun vertex_kind cg n = 
    if is_domain cg n then Dom
    else if is_codomain cg n then Cod
    else Int

fun is_boundary cg n = (vertex_kind cg n) <> INT
    
fun get_boundary (CGraph rep) = 
    V.NSet.of_list ((#domain rep) @ (#codomain rep));

fun is_vertex_correct cg n = 
    let val (_,ins,outs) = (get_vertex cg n) in
      if is_boundary cg n then 
	(
	 (is_domain cg n andalso not is_codomain cg n)
	 orelse
	 (is_codomain cg n andalso not is_domain cg n)
	)
	andalso 
	(
	 (E.NSet.is_empty ins andalso E.NSet.is_singleton outs) 
	 orelse
	 (E.NSet.is_empty outs andalso E.NSet.is_singleton ins) 
	)
      else 
	true
    end

fun is_correct bg = 
    foldl (fn n b => (is_vertex_correct n) andalso b)  
	  true
	  (get_boundary bg)
	 

val get_vertices = Graph.get_vertices o get_graph;
val get_edges = Graph.get_edges o get_graph;
val get_vertex_list = V.NTab.dest o get_vertices;
val get_edge_list = E.NTab.dest o get_edges;

(* implicit args: bg n *)
val get_vertex = Graph.get_vertex o get_graph;
val get_edge = Graph.get_edge o get_graph;
val lookup_vertex = Graph.lookup_vertex o get_graph;
val lookup_edge = Graph.lookup_edge o get_graph;

fun add_named_vertex n v kind bg =
    let val (n2,g2) = Graph.add_named_vertex n v (get_graph bg)
    in (n2, 
	bg |> (case kind of Dom =>  add_domain_vertex n 
			  | Cod =>  add_codomain_vertex n 
			  | Int => I)
	   |> set_graph g2 bg) 
    end;

fun new_named_vertex n v isBoundary bg =
    let val (n2,g2) = Graph.new_named_vertex n v (get_graph bg)
    in (n2, 
	bg |> (case kind of Dom =>  add_domain_vertex n 
			  | Cod =>  add_codomain_vertex n 
			  | Int => I)
	   |> set_graph g2 bg) 
    end;
    
(* implicit args: v bg *)
val add_vertex = add_named_vertex NE.default_vname;
val doadd_vertex = snd oo add_vertex;

(* implicit args: en e n1 n2 bg *)
fun add_named_edge en e n1 n2 bg = 
    let val (en2,g2) = Graph.add_named_edge en e n1 n2 (get_graph bg) 
    in (en2, set_graph g2 bg) end;
(* implicit: bg *)
fun new_named_edge en e n1 n2 =
    update_graph (Graph.new_named_edge en e n1 n2);

(* implicit args: e n1 n2 bg *)
val add_edge = add_named_edge NE.default_ename;
val doadd_edge  = snd oo add_edge;

(* deleting vertex also removes all it's edges. *)
fun delete_vertex n cg = 
    bg |> (case kind_of_vertex cg n of Dom => del_domain_vertex n
				     | Cod => del_codomain_vertex n
				     | Int => I)
      |> update_graph (Graph.delete_vertex n);

(* implicit args: en bg *)
val delete_edge = update_graph o Graph.delete_edge;

(* replace a list elt with another one, keeping the order 
   the same !!NOTE only works on first occurence!! *)
fun repl_elt x y [] = []
  | repl_elt x y h::t = if h = x then y::t else h::(repl_elt x y t)

(* vname = suggested name *)
(* rename all occurances of old name (1st arg) with new name (2nd arg) 
   n2 must not exist already, n1 must exist *)
fun rename_vname n1 n2 cg = 
    case kind_of_vertex cg n1 
     of Dom =>       
	bg |> update_domain (repl_elt n1 n2)
           |> update_graph (Graph.rename_vname n1 n2) 
      | Cod =>
	bg |> update_codomain (repl_elt n1 n2)
           |> update_graph (Graph.rename_vname n1 n2) 
      | Int => 
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
    [ Pretty.str "CGraph{",
      Pretty.block 
        [Pretty.str "  ", (* indent *)
         Pretty.chunks 
           [Pretty.block 
              [Pretty.str "Boundary Vertices: ",
               Pretty.list "{" "}" 
                           (map Vname.pretty (V.NSet.dest (get_boundary bg)))],
            Graph.pretty (get_graph bg)]],
      Pretty.str "}"
    ];

val print = Pretty.writeln o pretty;

end;
(*======================= E N D =============================*)



(* ------------------------------------------------------- *)
(* define some concrete structures ----- *)
(*-------------------------------------------------------*)

structure DB_CGraph = CGraphFun(V,E);
structure CGraph : CGRAPH = DB_Graph;

