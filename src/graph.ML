(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Graphs with named vertices and edges. *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* *)

signature BASIC_GRAPH = 
sig

structure Vertex : NAME_AND_DATA
structure Edge : NAME_AND_DATA

type T; (* a graph *)

exception no_such_vname_exp of string * Vertex.name * T;
exception vname_already_exists_exp of Vertex.name * T;

exception no_such_ename_exp of string * Edge.name * T;
exception ename_already_exists_exp of Edge.name * T;

(* Making Graphs *)
val empty : T
(* adding a vertex gives back unique name for it and the new graph *)
val add_vertex : Vertex.data -> T -> Vertex.name * T 
val doadd_vertex : Vertex.data -> T -> T
(* Vertex.name = suggested name *)
val add_named_vertex : Vertex.name -> Vertex.data -> T -> Vertex.name * T 
val new_named_vertex : Vertex.name -> Vertex.data -> T -> T  (* can raise *)

(* Vertex.names must already exist, else raises: no_such_vname_exp *)
val add_edge : Edge.data -> Vertex.name -> Vertex.name -> T -> Edge.name * T
val doadd_edge : Edge.data -> Vertex.name -> Vertex.name -> T -> T
val add_named_edge : Edge.name -> Edge.data -> Vertex.name -> Vertex.name -> T -> Edge.name * T 
val new_named_edge : Edge.name -> Edge.data -> Vertex.name -> Vertex.name -> T -> T (* can raise *)

(* delete raises an exception if there is no such edge/vertex *)
val delete_edge : Edge.name -> T -> T
val delete_vertex : Vertex.name -> T -> T

(* replace all occurances of old name (1st arg) with new name (2nd arg) *)
(*  2nd name must not already exist and 1st must already exist, 
    else an exception is raised *)
val rename_vname : Vertex.name -> Vertex.name -> T -> T 
val rename_ename : Edge.name -> Edge.name -> T -> T 

(* *)
val has_vname : T -> Vertex.name -> bool;
val has_ename : T -> Edge.name -> bool;


(* rename the 2nd graph to avoid all names in the first one *)
val rename_apart : T -> T -> T 

(* update a vertexe's data *)
val update_vertex : (Vertex.data -> Vertex.data) -> Vertex.name -> T -> T
val update_edge : (Edge.data -> Edge.data) -> Edge.name -> T -> T
val set_vertex : Vertex.data -> Vertex.name -> T -> T
val set_edge : Edge.data -> Edge.name -> T -> T

(* getting stuff from graphs *)
val get_vertex : T -> Vertex.name -> (Vertex.data * (Edge.NSet.T * Edge.NSet.T))
val get_edge : T -> Edge.name -> (Edge.data * (Vertex.name * Vertex.name))
val lookup_vertex : T -> Vertex.name 
									-> (Vertex.data * (Edge.NSet.T * Edge.NSet.T)) option
val lookup_edge : T -> Edge.name 
									-> (Edge.data * (Vertex.name * Vertex.name)) option

(* working with names in the graph, no exceptions *)
val useup_vnames : Vertex.NSet.T -> T -> T
val useup_enames : Edge.NSet.T -> T -> T

(* getting graph internal representation... *)
val get_vertex_list : T -> (Vertex.name * (Vertex.data 
                   * (Edge.NSet.T (* edges coming into this vertex *)
                      * Edge.NSet.T))) (* edges leaving this vertex *)
                    list;
val get_edge_list : T -> (ename * (Edge.data
                   * (Vertex.name (* from this vertex *)
                      * Vertex.name))) (* to this vertex *)
                    list

val get_vertices : T 
                -> (Vertex.data 
                    * (Edge.NSet.T (* edges coming into this vertex *)
                       * Edge.NSet.T)) (* edges leaving this vertex *)
                   Vertex.NTab.T;
val get_edges : T 
                -> (Edge.data
                    * (Vertex.name (* from this vertex *)
                       * Vertex.name)) (* to this vertex *)
                   Edge.NTab.T

(* pretty printing *)
val pretty : T -> Pretty.T
val print : T -> unit

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graphs with strings at edges and vertices *)
functor BasicGraphFun(structure Vertex : NAME_AND_DATA 
                            and Edge : NAME_AND_DATA) 
= struct

structure Vertex = Vertex;
structure Edge = Edge;

(* a graph *)
datatype T = Graph of 
         {
          vertices : (Vertex.data 
                   * (Edge.NSet.T (* edges coming into this vertex *)
                      * Edge.NSet.T)) (* edges leaving this vertex *)
                    Vertex.NTab.T,
          edges : (Edge.data
                   * (Vertex.name (* from this vertex *)
                      * Vertex.name)) (* to this vertex *)
                    Edge.NTab.T
         };

val empty = Graph { vertices = Vertex.NTab.empty, edges = Edge.NTab.empty };
fun update_vertices f (Graph rep) = 
    Graph { vertices = f (#vertices rep), edges = #edges rep };
fun update_edges f (Graph rep) = 
    Graph { vertices = #vertices rep, edges = f (#edges rep)};
val set_vertices = update_vertices o K;
val set_edges = update_edges o K;
fun get_vertices (Graph rep) = #vertices rep;
fun get_edges (Graph rep) = #edges rep;
val get_vertex_list = Vertex.NTab.dest o get_vertices;
val get_edge_list = Edge.NTab.dest o get_edges;
val get_vertex_nset = Vertex.NTab.get_names o get_vertices;
val get_edge_nset = Edge.NTab.get_names o get_edges;


fun lookup_vertex g n = 
    case Vertex.NTab.lookup (get_vertices g) n 
     of NONE => NONE | SOME x => SOME x;
fun lookup_edge g en = 
    case Edge.NTab.lookup (get_edges g) en 
     of NONE => NONE | SOME x => SOME x;

exception no_such_vname_exp of string * Vertex.name * T;
exception vname_already_exists_exp of Vertex.name * T;
exception no_such_ename_exp of string * Edge.name * T;
exception ename_already_exists_exp of Edge.name * T;

fun get_vertex g n = 
    case lookup_vertex g n 
     of NONE => raise no_such_vname_exp ("get_vertex", n, g)
      | SOME x => x;
fun get_edge g en = 
    case lookup_edge g en 
     of NONE => raise no_such_ename_exp ("get_edge", en, g)
      | SOME x => x;

(* implicit arg: g *)
fun update_vertex f n = 
    update_vertices (Vertex.NTab.map_entry (fn (nd,ios) => (f nd,ios)) n);
fun update_vertex_ins f n = 
    update_vertices (Vertex.NTab.map_entry 
                       (fn (nd,(ins,outs)) => (nd,(f ins, outs))) n);
fun update_vertex_outs f n = 
    update_vertices (Vertex.NTab.map_entry 
                       (fn (nd,(ins,outs)) => (nd,(ins, f outs))) n);
fun update_edge f en = 
    update_edges (Edge.NTab.map_entry (fn (e,fromto) => (f e,fromto)) en);
val set_vertex = update_vertex o K;
val set_edge = update_edge o K;


(* adding a vertex gives back unique name for it and the new graph *)
fun add_named_vertex n nd g =
    let val (n2,vertices2) = 
            Vertex.NTab.add (n,(nd,(Edge.NSet.empty,Edge.NSet.empty))) 
                            (get_vertices g)
    in (n2, set_vertices vertices2 g) end;
(* implicit args: nd g *)
val add_vertex = add_named_vertex Vertex.default_name;
val doadd_vertex = snd oo add_vertex;
(* implicit arg: g *)
fun new_named_vertex n nd =
    update_vertices 
      (Vertex.NTab.ins (n,(nd,(Edge.NSet.empty,Edge.NSet.empty))));

(* working with names in the graph, no exceptions *)
val useup_vnames = update_vertices o Vertex.NTab.useup_names;
val useup_enames = update_edges o Edge.NTab.useup_names;

(* implicit args: g n *)
val has_vname = Vertex.NTab.contains_name o get_vertices;
val has_ename = Edge.NTab.contains_name o get_edges;

fun add_named_edge en e n1 n2 g = 
    let 
      (* make sure the vertices exist *) 
      val _ = if has_vname g n1 then
                if has_vname g n2 then () 
                else raise no_such_vname_exp ("add_named_edge", n2, g)
              else raise no_such_vname_exp ("add_named_edge", n1, g);
      (* add the new egde *)
      val (en2,edges2) = Edge.NTab.add (en,(e,(n1,n2))) (get_edges g)

      (* update the vertices: add edge name as appropriate to ins and outs *)
      val vertices2 = 
          (get_vertices g)
            |> Vertex.NTab.map_entry 
						(fn (n,(ins,outs)) => (n,(ins,Edge.NSet.ins_fresh en2 outs))) n1
            |> Vertex.NTab.map_entry 
						(fn (n,(ins,outs)) => (n,(Edge.NSet.ins_fresh en2 ins,outs))) n2
      (* give back new edge name and new graph *)
    in (en2, Graph{vertices = vertices2, edges = edges2}) end;

(* implicit args: e n1 n2 g *)
val add_edge = add_named_edge Edge.default_name;
val doadd_edge = snd oooo add_edge;

fun new_named_edge en e n1 n2 g =
    let 
      (* make sure the vertices exist *) 
      val _ = if has_vname g n1 then
                if has_vname g n2 then () 
                else raise no_such_vname_exp ("add_named_edge", n2, g)
              else raise no_such_vname_exp ("add_named_edge", n1, g);
      (* add the new egde *)
      val edges2 = Edge.NTab.ins (en,(e,(n1,n2))) (get_edges g)

      (* update the vertices: add edge name as appropriate to ins and outs *)
      val vertices2 = 
          (get_vertices g)
            |> Vertex.NTab.map_entry 
						(fn (n,(ins,outs)) => (n,(ins,Edge.NSet.ins_fresh en outs))) n1
            |> Vertex.NTab.map_entry 
						(fn (n,(ins,outs)) => (n,(Edge.NSet.ins_fresh en ins,outs))) n2
      (* give back new edge name and new graph *)
    in Graph{vertices = vertices2, edges = edges2} end;

(* deleting vertex also removes all it's edges. *)
fun delete_vertex n g = 
    let 
      val vertices = get_vertices g
      val (nd,(ins,outs)) =
          case Vertex.NTab.lookup vertices n of 
            NONE => raise no_such_vname_exp ("delete_vertex", n, g)
          | SOME x => x;

      val vertices2 = vertices |> Vertex.NTab.delete n

      (* delete all edges going into or out of this vertex. *)
      val edges2 = 
          (get_edges g)
            |> Edge.NSet.fold Edge.NTab.delete ins
            |> Edge.NSet.fold Edge.NTab.delete outs

      (* give back new edge name and new graph *)
    in Graph{vertices = vertices2, edges = edges2} end;



(* implicit args: en g *)
(* *)
fun delete_edge en g = 
    let 
      val edges = get_edges g
      val (ed,(fromvn,tovn)) =
          case Edge.NTab.lookup edges en of 
            NONE => raise no_such_ename_exp ("delete_edge", en, g)
          | SOME x => x;

      val edges2 = edges |> Edge.NTab.delete en

      (* delete all edges going into or out of this vertex. *)
      val vertices2 = 
          (get_vertices g)
            |> Vertex.NTab.map_entry (fn (d,(ins,outs)) => 
                                         (d,(ins,Edge.NSet.delete en outs))) fromvn
            |> Vertex.NTab.map_entry (fn (d,(ins,outs)) => 
                                         (d,(Edge.NSet.delete en ins,outs))) tovn;

      (* give back new edge name and new graph *)
    in Graph{vertices = vertices2, edges = edges2} end;


(* Vertex.name = suggested name *)
(* rename all occurances of old name (1st arg) with new name (2nd arg) 
   n2 must not exist already, n1 must exist *)
fun rename_vname n1 n2 g = 
    let 
      val vertices = (get_vertices g)
      (* gets vertex for n1, makes ure it exists *)
      val (nd,(ins,outs)) = case Vertex.NTab.lookup vertices n1 
                             of NONE => raise no_such_vname_exp ("rename_vname", n1, g)
                              | SOME x => x;

      (* update the vertex entry for n1, we make sure that n2 doesn't already exist *)
      val vertices2 = 
          case (vertices |> Vertex.NTab.delete n1
                      |> Vertex.NTab.try_ins (n2,(nd,(ins,outs))))
           of NONE => raise vname_already_exists_exp (n2,g)
            | SOME vertices2 => vertices2;

      (* update any edges to use the new name n1 instead of n1, using ins and outs lists *)
      val edges2 = 
          (get_edges g)
            |> Edge.NSet.fold (Edge.NTab.map_entry (fn (e,(from,_)) => 
                                                       (e,(from,n2)))) ins
            |> Edge.NSet.fold (Edge.NTab.map_entry (fn (e,(_,tos)) => 
                                                       (e,(n2,tos)))) outs;

      (* give back new edge name and new graph *)
    in Graph{vertices = vertices2, edges = edges2} end;


fun rename_ename en1 en2 g = 
    let 
      val edges = (get_edges g)
      (* gets edge for n1, makes ure it exists *)
      val (e,(from_n,to_n)) = case Edge.NTab.lookup edges en1
                           of NONE => raise no_such_ename_exp ("rename_vname", en1, g)
                            | SOME x => x;

      (* update the edge entry for n1, we make sure that n2 doesn't already exist *)
      val edges2 = 
          case (edges |> Edge.NTab.delete en1
                      |> Edge.NTab.try_ins (en2,(e,(from_n,to_n))))
           of NONE => raise ename_already_exists_exp (en2,g)
            | SOME edges2 => edges2;

      (* update any edges to use the new name n1 instead of n1, using ins and outs lists *)
      val vertices2 = 
          (get_vertices g)
            |> Vertex.NTab.map_entry (fn (n,(ins,outs)) => 
                                   let val outs2 = outs |> Edge.NSet.delete en1
                                                        |> Edge.NSet.ins_fresh en2
                                   in (n,(ins,outs2)) end) from_n
            |> Vertex.NTab.map_entry (fn (n,(ins,outs)) =>
                                   let val ins2 = ins |> Edge.NSet.delete en1
                                                      |> Edge.NSet.ins_fresh en2
                                   in (n,(ins2,outs)) end) to_n;

      (* give back new edge name and new graph *)
    in Graph{vertices = vertices2, edges = edges2} end;



(* rename the names in g2 to avoid those in g1 *)

(* IMPROVE: avoid conversion to lists as uses up lots of memory: have
a function in FNAMETAB to useup names from a name collection *)
fun rename_apart avoidg ing = 
		let val vnames = Vertex.NTab.get_names (get_vertices avoidg)
				val enames = Edge.NTab.get_names (get_edges avoidg)
				val g2 = ing |> useup_vnames vnames |> useup_enames enames
		in
			g2 |> (Vertex.NTab.fold 
							 (fn (n,_) => fn g3 =>
									 if Vertex.NSet.contains vnames n then
										 rename_vname 
											 n (Vertex.NSet.new n (Vertex.NTab.get_names 
																							 (get_vertices g3)))
											 g3
									 else g3)
							 (get_vertices g2))
				 |> (Edge.NTab.fold 
							 (fn (n,_) => fn g3 =>
									if Edge.NSet.contains enames n then
										rename_ename 
											n (Edge.NSet.new n (Edge.NTab.get_names 
																							 (get_edges g3)))
											g3
									else g3)
							 (get_edges g2))
		end;


(* pretty printing *)
fun pretty g = 
    Pretty.chunks 
      [Pretty.str "Graph{",
       Pretty.block 
         [Pretty.str "  ", (* indent *)
          Pretty.chunks 
          [Pretty.block 
             [Pretty.str "Vertices: ",
              Pretty.chunks 
                (map 
                   (fn (n, (nd,(ins,outs))) => 
                       Pretty.block 
                         [Vertex.pretty_name n,
                          Pretty.str "; ",
                          Pretty.list "i{" "}" (map Edge.pretty_name (Edge.NSet.dest ins)),
                          Pretty.list "; o{" "}" (map Edge.pretty_name (Edge.NSet.dest outs)),
                          Pretty.str "; ",
                          Vertex.pretty_data nd
                   ])
                   (Vertex.NTab.dest (get_vertices g)))],
           Pretty.block
             [Pretty.str "Edges: ",
              Pretty.chunks 
                (map 
                   (fn (en, (e, (n1, n2))) => 
                       Pretty.block 
                         [Edge.pretty_name en,
                          Pretty.str ": ",
                          Vertex.pretty_name n1,
                          Pretty.str " - ",
                          Vertex.pretty_name n2,
                          Pretty.str "; ",
                          Edge.pretty_data e
                   ])
                   (Edge.NTab.dest (get_edges g)))]]],
       Pretty.str "}"
      ];
     
val print = Pretty.writeln o pretty;

end;


(* An instance of Basic Graph: strings at vertices and edges *)
structure DB_StrGraph = 
BasicGraphFun(
  structure Vertex = StrVertex
  and Edge = StrEdge);

structure StrGraph : BASIC_GRAPH = DB_StrGraph;

install_pp (make_pp ["StrGraph", "T"] 
                    (Pretty.pprint o StrGraph.pretty));


(* another basic kind of graph with unit for edge data *)
structure DB_StrUnitGraph = 
BasicGraphFun(
  structure Vertex = StrVertex
  and Edge = UnitEdge);

structure StrUnitGraph : BASIC_GRAPH = DB_StrUnitGraph;

install_pp (make_pp ["StrUnitGraph", "T"] 
                    (Pretty.pprint o StrUnitGraph.pretty));
