(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Graphs with named vertices and edges. *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* 
IMPROVE: 
make undirected and add direction, when you want it, as data. Make
data for vertex and edges a separate table from edge/vertex
connectedness.
*)

(* Basic (Minimal) interface for Graphs *)
signature BASIC_GRAPH = 
sig

structure Vertex : VERTEX
structure Edge : EDGE

type T; (* a graph *)

exception no_such_vname_exp of string * Vertex.name * T;
exception vname_already_exists_exp of Vertex.name * T;

exception no_such_ename_exp of string * Edge.name * T;
exception ename_already_exists_exp of Edge.name * T;

(* Making Graphs *)
val empty : T
(* adding a vertex gives back unique name for it and the new graph *)
(* Vertex.name = suggested name *)
val add_named_vertex : Vertex.name -> Vertex.data -> T -> Vertex.name * T 

(* Vertex.names must already exist, else raises: no_such_vname_exp *)
val add_named_edge : Edge.name -> Edge.data -> Vertex.name -> Vertex.name -> T -> Edge.name * T 

(* delete raises an exception if there is no such edge/vertex *)
val delete_edge : Edge.name -> T -> T
val delete_vertex : Vertex.name -> T -> T

(* replace all occurances of old name (1st arg) with new name (2nd arg) *)
(*  2nd name must not already exist and 1st must already exist, 
    else an exception is raised *)
val rename_vname : Vertex.name -> Vertex.name -> T -> T 
val rename_ename : Edge.name -> Edge.name -> T -> T 

(* update a data of a vertex or edge *)
val update_vertex : (Vertex.data -> Vertex.data) -> Vertex.name -> T -> T
val update_edge : (Edge.data -> Edge.data) -> Edge.name -> T -> T

(* getting graph internal representation... *)
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

(*  GRAPH is BASIC_GRAPH + useful utilities/API *)
signature GRAPH = 
sig

include BASIC_GRAPH
(* getting stuff from graphs (gets raise exceptions if no entry) *)
val lookup_vertex : T -> Vertex.name 
									-> (Vertex.data * (Edge.NSet.T * Edge.NSet.T)) option
val lookup_edge : T -> Edge.name 
									-> (Edge.data * (Vertex.name * Vertex.name)) option
(* getting stuff from graphs (exception if not there) *)
val get_vertex : T -> Vertex.name -> (Vertex.data * (Edge.NSet.T * Edge.NSet.T))
val get_edge : T -> Edge.name -> (Edge.data * (Vertex.name * Vertex.name))

(* adding a vertex gives back unique name for it and the new graph *)
val add_vertex : Vertex.data -> T -> Vertex.name * T 
val doadd_vertex : Vertex.data -> T -> T
val new_named_vertex : Vertex.name -> Vertex.data -> T -> T  (* can raise *)

(* Vertex.names must already exist, else raises: no_such_vname_exp *)
val add_edge : Edge.data -> Vertex.name -> Vertex.name -> T -> Edge.name * T
val doadd_edge : Edge.data -> Vertex.name -> Vertex.name -> T -> T
val new_named_edge : Edge.name -> Edge.data -> Vertex.name -> Vertex.name -> T -> T (* can raise *)
(* get vertex and edge names *)
val get_vnames : T -> Vertex.NSet.T
val get_enames : T -> Edge.NSet.T

(* *)
val has_vname : T -> Vertex.name -> bool;
val has_ename : T -> Edge.name -> bool;

(* renaming *)
val rename : (Vertex.renaming * Edge.renaming) -> T -> (Vertex.renaming * Edge.renaming) * T 
(* rename the 2nd graph to avoid all names in the first one *)
val rename_apart : T -> T -> (Vertex.renaming * Edge.renaming) * T 

(* merge two graphs - vertices with same name are kept only once, edges
   must be distinct. *)
val merge_by_vertices : T -> T -> T

(* remove the first graph from the second, assumes first is a subgraph *)
val delete_subgraph : T -> T -> T

(* update a vertexe's data *)
val set_vertex : Vertex.data -> Vertex.name -> T -> T
val set_edge : Edge.data -> Edge.name -> T -> T

(* getting graph internal representation... *)
val get_vertex_list : T -> (Vertex.name * (Vertex.data 
                   * (Edge.NSet.T (* edges coming into this vertex *)
                      * Edge.NSet.T))) (* edges leaving this vertex *)
                    list;
val get_edge_list : T -> (Edge.name * (Edge.data
                   * (Vertex.name (* from this vertex *)
                      * Vertex.name))) (* to this vertex *)
                    list

(* edges between two vertices *)
val enames_between : T -> Vertex.name -> Vertex.name -> Edge.NSet.T
val edges_between : T -> Vertex.name -> Vertex.name -> Edge.data Edge.NTab.T


(* get adjacent vertices to some vertex *)
val adj_vnames : T -> Vertex.name -> Vertex.NSet.T
val adj_vnames' : T -> (Edge.NSet.T * Edge.NSet.T) -> Vertex.NSet.T

(* get adj vertices (from edge info) and from vertex name *)

val adj_vertices_with_edges : T -> Vertex.name 
                   -> (Edge.data Edge.NTab.T) Vertex.NTab.T
val adj_vertices_with_edges' : T -> (Edge.NSet.T * Edge.NSet.T)
                    -> (Edge.data Edge.NTab.T) Vertex.NTab.T

val adj_vertices_with_enames : T -> Vertex.name 
                               -> Edge.NSet.T Vertex.NTab.T
val adj_vertices_with_enames' : T -> (Edge.NSet.T * Edge.NSet.T)
                                -> Edge.NSet.T Vertex.NTab.T

end;




(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Basic Graphs with strings at edges and vertices *)
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

(* implicit: g n *)
val lookup_vertex = Vertex.NTab.lookup o get_vertices
val lookup_edge = Edge.NTab.lookup o get_edges

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



(* deleting vertex also removes all it's edges. *)
fun delete_vertex n g = 
    let 
      val vertices = get_vertices g
      val (nd,(ins,outs)) =
          case Vertex.NTab.lookup vertices n of 
            NONE => raise no_such_vname_exp ("delete_vertex", n, g)
          | SOME x => x;

      (* delete all edges going into or out of this vertex. *)
      val g2 = 
          g |> Edge.NSet.fold delete_edge ins
            |> Edge.NSet.fold delete_edge (Edge.NSet.subtract outs ins)

    in g2 |> update_vertices (Vertex.NTab.delete n) end;


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
                         (* raises exception if n2 already exsists *)
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




(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  extra functions for Basic Graphs: provides rest of the GRAPH signature *)
functor GraphToolsFun(BasicGraph : BASIC_GRAPH) 
= struct

local 
open BasicGraph;
in

val get_vertex_list = Vertex.NTab.dest o get_vertices;
val get_edge_list = Edge.NTab.dest o get_edges;

val get_vnames = Vertex.NTab.get_names o get_vertices;
val get_enames = Edge.NTab.get_names o get_edges;

(* implicit: g n *)
val lookup_vertex = Vertex.NTab.lookup o get_vertices
val lookup_edge = Edge.NTab.lookup o get_edges

(* *)
fun get_vertex g n = 
    case lookup_vertex g n 
     of NONE => raise no_such_vname_exp ("get_vertex", n, g)
      | SOME x => x;
fun get_edge g en = 
    case lookup_edge g en 
     of NONE => raise no_such_ename_exp ("get_edge", en, g)
      | SOME x => x;

val set_vertex = update_vertex o K;
val set_edge = update_edge o K;

(* implicit args: nd g *)
val add_vertex = add_named_vertex Vertex.default_name;
val doadd_vertex = snd oo add_vertex;
fun new_named_vertex n nd g =
    let val (n2,g2) = add_named_vertex n nd g in
      if Vertex.name_eq (n2, n) then g2 else 
      raise vname_already_exists_exp (n, g)
    end;

(* implicit args: g n *)
val has_vname = Vertex.NTab.contains_name o get_vertices;
val has_ename = Edge.NTab.contains_name o get_edges;

(* implicit args: e n1 n2 g *)
val add_edge = add_named_edge Edge.default_name;
val doadd_edge = snd oooo add_edge;
fun new_named_edge en e n1 n2 g =
    let val (en2,g2) = add_named_edge en e n1 n2 g in
      if Edge.name_eq (en2, en) then g2 else 
      raise ename_already_exists_exp (en,g)
    end;


(* merge graphs, assumes disjoint edge names adds new vertices, keeps
   existing ones. *)
fun merge_by_vertices g1 g2 = 
    g2 |> (Vertex.NTab.fold 
             (fn (vn,(d,_)) => fn g3 => 
                 if has_vname g3 vn then g3 else new_named_vertex vn d g3)
             (get_vertices g1))
       |> (Edge.NTab.fold 
             (fn (en,(d,(vn1,vn2))) => fn g3 => 
                 if has_ename g3 en then 
                   raise ename_already_exists_exp (en, g3)
                 else new_named_edge en d vn1 vn2 g3)
             (get_edges g1));

(* delete subg from fromg; assumes edge and vetrex names of subg are a
   subset of fromg; else delete will raise exception *)
fun delete_subgraph subg fromg = 
    fromg |> (Edge.NSet.fold 
                delete_edge 
                (get_enames subg))
          |> (Vertex.NSet.fold 
                delete_vertex
                (get_vnames subg));

(* given: vertex renaming, edge renaming, rename g *)
fun rename (vrn,ern) g = 
		let val (vrn2,g2) = 
            Vertex.NTab.fold 
						  (fn (n,_) => fn (renaming,g') =>
                  case Vertex.rename1 n renaming of
                    (NONE, renaming') => (renaming', g')
                  | (SOME n', renaming') => 
                    (renaming', rename_vname n n' g'))
							(get_vertices g)
              (vrn, g);
        val (ern2,g3) = 
            Edge.NTab.fold 
						 	(fn (e,_) => fn (renaming,g') =>
                  case Edge.rename1 e renaming of
                    (NONE, renaming') => (renaming', g')
                  | (SOME e', renaming') => 
                    (renaming', rename_ename e e' g'))
							(get_edges g2)
              (ern, g2);
    in
      ((vrn2,ern2),g3)
    end;

(* rename ing to be different from avoidg *)
(* IMPROVE: avoid conversion to lists as uses up lots of memory: have
a function in FNAMETAB to useup names from a name collection *)
fun rename_apart avoidg ing = 
		let val avoid_vnames = Vertex.NTab.get_names (get_vertices avoidg)
				val avoid_enames = Edge.NTab.get_names (get_edges avoidg)
    in
      rename (Vertex.init_renaming avoid_vnames, Edge.init_renaming avoid_enames) ing
    end;


(* make a table: (Edge.data Edge.NTab.T) Vertex.NTab.T
holding every edge's data that is connects n and to the key Vertex.NTab, 
the Vertex.NTab has every vertex adjacent to n. (Note: ignores direction) *)
fun adj_vertices_with_edges' g (ins,outs) = 
    Vertex.NTab.empty
      |> (Edge.NSet.fold 
            (fn e => fn ntab => 
             let val (d,(from,to)) = get_edge g e
             in if (Vertex.NTab.contains ntab from) then 
                  Vertex.NTab.map_entry 
                    (Edge.NTab.ins (e,d)) (* no dup exp for well-formed graphs *)
                    from ntab
                else Vertex.NTab.ins (from,Edge.NTab.ins (e,d) Edge.NTab.empty) ntab
             end)
            ins)
      |> (Edge.NSet.fold 
            (fn e => fn ntab => 
             let val (d,(from,to)) = get_edge g e
             in if (Vertex.NTab.contains ntab to) then 
                  Vertex.NTab.map_entry 
                    (Edge.NTab.ins (e,d)) (* no dup exp for well-formed graphs *)
                    to ntab
                else Vertex.NTab.ins (to,Edge.NTab.ins (e,d) Edge.NTab.empty) ntab
             end)
            outs);

fun adj_vertices_with_edges g n = 
    adj_vertices_with_edges' g (snd (get_vertex g n));

fun adj_vertices_with_enames' g (ins,outs) = 
    Vertex.NTab.empty
      |> (Edge.NSet.fold 
            (fn e => fn ntab => 
             let val (_,(from,to)) = get_edge g e
             in if (Vertex.NTab.contains ntab from) then 
                  Vertex.NTab.map_entry 
                    (Edge.NSet.add e) (* no dup exp for well-formed graphs *)
                    from ntab
                else 
                  Vertex.NTab.ins (from,Edge.NSet.add e Edge.NSet.empty) ntab
             end)
            ins)
      |> (Edge.NSet.fold 
            (fn e => fn ntab => 
             let val (d,(from,to)) = get_edge g e
             in if (Vertex.NTab.contains ntab to) then 
                  Vertex.NTab.map_entry 
                    (Edge.NSet.add e) (* no dup exp for well-formed graphs *)
                    to ntab
                else 
                  Vertex.NTab.ins (to,Edge.NSet.add e Edge.NSet.empty) ntab
             end)
            outs);

fun adj_vertices_with_enames g n = 
    adj_vertices_with_enames' g (snd (get_vertex g n));

fun adj_vnames' g (ins,outs) = 
    Vertex.NSet.empty
      |> (Edge.NSet.fold 
            (fn e => 
                let val (_,(from,to)) = get_edge g e
                in Vertex.NSet.add from end)
            ins)
      |> (Edge.NSet.fold 
            (fn e => 
                let val (_,(from,to)) = get_edge g e
                in Vertex.NSet.add to end)
            outs);

fun adj_vnames g n = 
    adj_vnames' g (snd (get_vertex g n));

fun enames_between g n1 n2 = 
    let 
      val (_,(ins1,outs1)) = get_vertex g n1
      val (_,(ins2,outs2)) = get_vertex g n2
    in
      Edge.NSet.union_merge
        (Edge.NSet.intersect ins1 outs2)
        (Edge.NSet.intersect ins2 outs1)
    end;    

fun edges_between g n1 n2 = 
    Edge.NSet.fold 
      (fn e => Edge.NTab.ins (e, fst (get_edge g e)))
      (enames_between g n1 n2)
      Edge.NTab.empty;

end; (* local -- hides opened Graph data types *)
end;


(* An instance of Basic Graph: strings at vertices and edges *)
structure DB_StrGraph = 
struct 
  structure BasicStrGraph = 
    BasicGraphFun(
      structure Vertex = StrVertex
      and Edge = StrEdge);
  structure GraphTools = GraphToolsFun(BasicStrGraph);
  open BasicStrGraph 
  open GraphTools
end;

structure StrGraph : GRAPH = DB_StrGraph;

install_pp (make_pp ["StrGraph", "T"] 
                    (Pretty.pprint o StrGraph.pretty));


(* another basic kind of graph with unit for edge data *)
structure DB_StrUnitGraph = 
struct 
  structure BasicStrUnitGraph = 
    BasicGraphFun(
      structure Vertex = StrVertex
            and Edge = UnitEdge);
  structure GraphTools = GraphToolsFun(BasicStrUnitGraph);
  open BasicStrUnitGraph 
  open GraphTools
end;

structure StrUnitGraph : GRAPH = DB_StrUnitGraph;

install_pp (make_pp ["StrUnitGraph", "T"] 
                    (Pretty.pprint o StrUnitGraph.pretty));


