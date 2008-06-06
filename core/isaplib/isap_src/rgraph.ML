(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Relatuon Graphs with named vertexs (vars) and 
    edges (relations). *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* *)

signature BASIC_RGRAPH = 
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
(* adding a vertex gives back unique name for it and the new graph
val add_vertex : Vertex.data -> T -> Vertex.name * T 
val doadd_vertex : Vertex.data -> T -> T *)
(* Vertex.name = suggested name *)
val add_named_vertex : Vertex.name -> Vertex.data -> T -> Vertex.name * T 
val new_named_vertex : Vertex.name -> Vertex.data -> T -> T  (* can raise *)

(* Vertex.names must already exist, else raises: no_such_vname_exp *)
val add_named_edge : Edge.name -> Edge.data -> Vertex.NSet.T -> T -> Edge.name * T 
val new_named_edge : Edge.name -> Edge.data -> Vertex.NSet.T -> T -> T (* can raise *)

(* delete raises an exception if there is no such edge/vertex *)
val delete_edge : Edge.name -> T -> T
val delete_vertex : Vertex.name -> T -> T

(* replace all occurances of old name (1st arg) with new name (2nd arg) *)
(*  2nd name must not already exist and 1st must already exist, 
    else an exception is raised *)
val rename_vname : Vertex.name -> Vertex.name -> T -> T 
val rename_ename : Edge.name -> Edge.name -> T -> T 

(* update a vertex's data *)
val update_vertex : (Vertex.data -> Vertex.data) -> Vertex.name -> T -> T
val update_edge : (Edge.data -> Edge.data) -> Edge.name -> T -> T
val set_vertex : Vertex.data -> Vertex.name -> T -> T
val set_edge : Edge.data -> Edge.name -> T -> T

(* getting stuff from graphs *)
val get_vertex : T -> Vertex.name -> (Vertex.data * Edge.NSet.T)
val get_edge : T -> Edge.name -> (Edge.data * Vertex.NSet.T)
val lookup_vertex : T -> Vertex.name 
									-> (Vertex.data * Edge.NSet.T) option
val lookup_edge : T -> Edge.name 
									-> (Edge.data * Vertex.NSet.T) option

(* working with names in the graph, no exceptions *)
val useup_vnames : Vertex.NSet.T -> T -> T
val useup_enames : Edge.NSet.T -> T -> T

val get_connected_vertices : T -> Vertex.name -> (Edge.name * Vertex.name) list 

(* paths *)
val shortest_path : T -> Vertex.name -> Vertex.name -> (Edge.name * Vertex.name) list option

(* getting graph internal representation... *)
val get_vertex_list : T -> (Vertex.name * (Vertex.data * Edge.NSet.T)) (* edges leaving this vertex *)
                    list;
val get_edge_list : T -> (Edge.name * (Edge.data * Vertex.NSet.T)) (* to this vertex *)
                    list

val get_vertices : T -> (Vertex.data * Edge.NSet.T) (* relations we are in... *)
                       Vertex.NTab.T;
val get_edges : T -> (Edge.data * Vertex.NSet.T) (* to this vertex *)
                     Edge.NTab.T

(* pretty printing *)
val pretty : T -> Pretty.T
val pretty_vertex : Vertex.data -> Pretty.T
val pretty_edge : Edge.data -> Pretty.T

val print : T -> unit
val print_vertex : Vertex.data -> unit
val print_edge : Edge.data -> unit

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Basic Relational Graphs *)
functor BasicRGraphFun(
   structure Vertex : VERTEX
         and Edge : EDGE
) 
(* : BASIC_GRAPH where type Vertex.data = Vertex.data and type Edge.data = Edge.data *)
= struct

structure Vertex = Vertex;
structure Edge = Edge;

(* a graph *)
datatype T = RGraph of 
         {
          vertices : (Vertex.data * Edge.NSet.T) (* edges incident to this vertex *)
                    Vertex.NTab.T,
          edges : (Edge.data * Vertex.NSet.T) (* vertices connected by this relation *) 
                    Edge.NTab.T
         };

val empty = RGraph { vertices = Vertex.NTab.empty, edges = Edge.NTab.empty };
fun update_vertices f (RGraph rep) = 
    RGraph { vertices = f (#vertices rep), edges = #edges rep };
fun update_edges f (RGraph rep) = 
    RGraph { vertices = #vertices rep, edges = f (#edges rep)};
val set_vertices = update_vertices o K;
val set_edges = update_edges o K;
fun get_vertices (RGraph rep) = #vertices rep;
fun get_edges (RGraph rep) = #edges rep;
val get_vertex_list = Vertex.NTab.dest o get_vertices;
val get_edge_list = Edge.NTab.dest o get_edges;

fun lookup_vertex g n = 
    case Vertex.NTab.lookup (get_vertices g) n 
     of NONE => NONE | SOME x => SOME x;
fun lookup_edge g en = 
    case Edge.NTab.lookup (get_edges g) en 
     of NONE => NONE | SOME x => SOME x;


fun get_connected_vertices g n = 
    let 
      val vertices = get_vertices g
      val edges = get_edges g
      val (nd,ens) = Vertex.NTab.get vertices n
    in 
      Edge.NSet.fold (fn e => 
                      let val (ed,ns) = Edge.NTab.get edges e
                      in Vertex.NSet.fold (fn n' => 
                                       fn nes => 
                                          if Vertex.NTab.name_eq (n', n) then nes
                                          else (e,n')::nes)
                                     ns end)
                  ens
                  []
    end;

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

(* adding a vertex gives back unique name for it and the new graph *)
fun add_named_vertex n nd g =
    let val (n2,vertices2) = Vertex.NTab.add (n,(nd,Edge.NSet.empty)) (get_vertices g)
    in (n2, set_vertices vertices2 g) end;
(* implicit args: nd g *)
(* val add_vertex = add_named_vertex Vertex.default_name;
val doadd_vertex = snd oo add_vertex;*)

(* implicit arg: g *)
fun new_named_vertex n nd = update_vertices (Vertex.NTab.ins (n,(nd,Edge.NSet.empty)));

(* working with names in the graph, no exceptions *)
val useup_vnames = update_vertices o Vertex.NTab.useup_names;
val useup_enames = update_edges o Edge.NTab.useup_names;

(* implicit args: g n *)
val has_vname = Vertex.NTab.contains o get_vertices;
val has_ename = Edge.NTab.contains o get_edges;


fun add_named_edge en e ns g = 
    let 
      (* make sure the vertices exist *) 
      val _ = Vertex.NSet.fold (fn n => if has_vname g n then I 
                                    else raise no_such_vname_exp 
                                                 ("add_named_edge", n, g))
                           ns ();

      (* add the new egde *)
      val (en2,edges2) = Edge.NTab.add (en,(e,ns)) (get_edges g)

      (* update the vertices: add edge name as appropriate to ins and outs *)
      val vertices2 = 
          Vertex.NSet.fold
            (Vertex.NTab.map_entry (fn (n,ees) => (n,Edge.NSet.ins_fresh en2 ees)))            
            ns
            (get_vertices g)
      (* give back new edge name and new graph *)
    in (en2, RGraph{vertices = vertices2, edges = edges2}) end;

fun new_named_edge en e ns g =
    let 
      (* make sure the vertices exist *) 
      val _ = Vertex.NSet.fold (fn n => if has_vname g n then I 
                                    else raise no_such_vname_exp 
                                                 ("add_named_edge", n, g))
                           ns ();

      (* add the new egde *)
      val edges2 = Edge.NTab.ins (en,(e,ns)) (get_edges g)

      (* update the vertices: add edge name as appropriate to ins and outs *)
      val vertices2 = 
          Vertex.NSet.fold
            (Vertex.NTab.map_entry (fn (n,ees) => (n,Edge.NSet.ins_fresh en ees)))
            ns
            (get_vertices g)
      (* give back new edge name and new graph *)
    in RGraph{vertices = vertices2, edges = edges2} end;

(* deleting vertex also removes all it's edges. *)
fun delete_vertex n g = 
    let 
      fun delete_connecting_vertex n (ed,ns) = 
          (ed, Vertex.NSet.delete n ns);

      val vertices = get_vertices g
      val (nd,ens) =
          case Vertex.NTab.lookup vertices n of 
            NONE => raise no_such_vname_exp ("delete_vertex", n, g)
          | SOME x => x;

      val vertices2 = vertices |> Vertex.NTab.delete n

      (* delete all edges going into or out of this vertex. *)
      val edges2 = 
          (get_edges g)
            |> Edge.NSet.fold 
            (Edge.NTab.map_entry (delete_connecting_vertex n)) ens

      (* give back new edge name and new graph *)
    in RGraph{vertices = vertices2, edges = edges2} end;
    

(* deleting an edge also removes all it's vertices if they are not 
   connected to anything else. *)
fun delete_edge e g = 
    let 
      fun delete_connecting_edge e (nd,ees) = 
          (nd, Edge.NSet.delete e ees);

      val edges = get_edges g
      val (ed,ns) =
          case Edge.NTab.lookup edges e of 
            NONE => raise no_such_ename_exp ("delete_edge", e, g)
          | SOME x => x;

      val edges2 = edges |> Edge.NTab.delete e

      (* delete all edges going into or out of this vertex. *)
      val vertices2 = 
          (get_vertices g)
            |> Vertex.NSet.fold 
            (Vertex.NTab.map_entry (delete_connecting_edge e)) ns

      (* give back new edge name and new graph *)
    in RGraph{vertices = vertices2, edges = edges2} end;



(* vname = suggested name *)
(* rename all occurances of old name (1st arg) with new name (2nd arg) 
   n2 must not exist already, n1 must exist *)
fun rename_vname n1 n2 g = 
    let 
      val vertices = (get_vertices g)
      (* gets vertex for n1, makes ure it exists *)
      val (nd,ens) = case Vertex.NTab.lookup vertices n1 
                      of NONE => raise no_such_vname_exp ("rename_vname", n1, g)
                       | SOME x => x;

    (* update the vertex entry for n1, make sure that n2 doesn't already exist *)
      val vertices2 = 
          case (vertices |> Vertex.NTab.delete n1
                      |> Vertex.NTab.try_ins (n2,(nd,ens)))
           of NONE => raise vname_already_exists_exp (n2,g)
            | SOME vertices2 => vertices2;

      (* update any edges to use the new name instead of n1 *)
      val edges2 = 
          Edge.NSet.fold 
            (Edge.NTab.map_entry (fn (e,nns) => 
                                 (e, nns |> Vertex.NSet.delete n1
                                         |> Vertex.NSet.add n2))) 
            ens
            (get_edges g);

      (* give back new edge name and new graph *)
    in RGraph{vertices = vertices2, edges = edges2} end;


fun rename_ename en1 en2 g = 
    let 
      val edges = (get_edges g)
      (* gets vertex for n1, makes ure it exists *)
      val (e,nns) = case Edge.NTab.lookup edges en1
                     of NONE => raise no_such_ename_exp ("rename_vname", en1, g)
                      | SOME x => x;

      (* update the vertex entry for n1, we make sure that n2 doesn't already exist *)
      val edges2 = 
          case (edges |> Edge.NTab.delete en1
                      |> Edge.NTab.try_ins (en2,(e,nns)))
           of NONE => raise ename_already_exists_exp (en2,g)
            | SOME edges2 => edges2;

      (* update any edges to use the new name n1 instead of n1, using ins and outs lists *)
      val vertices2 = 
          Vertex.NSet.fold 
            (Vertex.NTab.map_entry (fn (nd,ens) => 
                                 (nd, ens |> Edge.NSet.delete en1
                                          |> Edge.NSet.add en2))) 
            nns
            (get_vertices g);

      (* give back new edge name and new graph *)
    in RGraph{vertices = vertices2, edges = edges2} end;

    

(* finding shortest path between two vertices, forward search, breadth first,
avoids any loops *) 
fun shortest_path g n1 n2 =
    let 
      fun search dest ptab [] = NONE
        | search dest ptab ((a,alist)::agenda) = 
          (if Vertex.NTab.name_eq (a, dest) then SOME alist
           else if Vertex.NSet.contains ptab a then 
             search dest ptab agenda
           else
             let 
               val agenda' = 
                   agenda @ (map (fn (e,b) => (b, (e,a)::alist))
                                 (get_connected_vertices g a))
               val ptab' = Vertex.NSet.add a ptab
             in search dest ptab' agenda' end)
    in search n2 Vertex.NSet.empty [(n1,[])] end;


(* implicit arg: g *)
fun update_vertex f n = 
    update_vertices (Vertex.NTab.map_entry (fn (nd,ens) => (f nd,ens)) n);
fun update_edge f en = 
    update_edges (Edge.NTab.map_entry (fn (e,nns) => (f e,nns)) en);
val set_vertex = update_vertex o K;
val set_edge = update_edge o K;

(* pretty printing *)
val pretty_vname = Vertex.pretty_name;
val pretty_ename = Edge.pretty_name;
val pretty_vertex = Vertex.pretty_data;
val pretty_edge = Edge.pretty_data;

val print_vname = Pretty.writeln o pretty_vname;
val print_ename = Pretty.writeln o pretty_ename;
val print_vertex = Pretty.writeln o pretty_vertex;
val print_edge = Pretty.writeln o pretty_edge;

fun pretty g = 
    Pretty.chunks 
      [Pretty.str "RGraph{",
       Pretty.block 
         [Pretty.str "  ", (* indent *)
          Pretty.chunks 
          [Pretty.block 
             [Pretty.str "Vertices: ",
              Pretty.chunks 
                (map 
                   (fn (n, (nd,ens)) => 
                       Pretty.block 
                         [pretty_vname n,
                          Pretty.str "; ",
                          Pretty.list "{" "}" (map pretty_ename (Edge.NSet.dest ens)),
                          Pretty.str "; ",
                          pretty_vertex nd
                   ])
                   (Vertex.NTab.dest (get_vertices g)))],
           Pretty.block
             [Pretty.str "Edges: ",
              Pretty.chunks 
                (map 
                   (fn (en, (ed,nns)) => 
                       Pretty.block 
                         [pretty_ename en,
                          Pretty.str "; ",
                          Pretty.list "{" "}" (map pretty_vname (Vertex.NSet.dest nns)),
                          Pretty.str "; ",
                          pretty_edge ed
                   ])
                   (Edge.NTab.dest (get_edges g)))]]],
       Pretty.str "}"
      ];
     
val print = Pretty.writeln o pretty;

end;


(* An instance of Basic Graph: strings at vertices and edges *)
structure DB_StrRGraph = BasicRGraphFun(
  structure Vertex = StrVertex
  and Edge = StrEdge
);
(* structure StrRGraph :> BASIC_RGRAPH where 
type Vertex.data = StrVertex.data 
 and type Edge.data = StrEdge.data
 and type Vertex.name = string
 and type Edge.name = string
= DB_StrRGraph;
*)

structure StrRGraph : BASIC_RGRAPH 
where type Vertex.name = string
  and type Vertex.data = string
  and type Edge.name = string
  and type Edge.data = string
= DB_StrRGraph;


open StrRGraph;

val g = StrRGraph.empty;
val g = g |> StrRGraph.new_named_vertex ("a") ("");
val g = g |> StrRGraph.new_named_vertex ("b") ("");
val g = g |> StrRGraph.new_named_vertex ("c") ("");
StrRGraph.print g; 
val g = g |> StrRGraph.new_named_edge ("R0") ("") 
        (StrRGraph.Vertex.NSet.of_list (["a","b"]));
val g = g |> StrRGraph.new_named_edge ("R1") ("")
        (StrRGraph.Vertex.NSet.of_list (["b","c"]));;

StrRGraph.print g; 
StrRGraph.shortest_path g ("a") ("c"); 

StrRGraph.get_connected_vertices g ("a");


structure DB_ParamRGraph = BasicRGraphFun(
  structure Vertex = UnitVertex
  and Edge = UnitEdge
);

structure ParamRGraph : BASIC_RGRAPH 
where type Vertex.name = string
  and type Vertex.data = unit
  and type Edge.name = string
  and type Edge.data = unit
= DB_ParamRGraph;


val g = ParamRGraph.empty;
val g = g |> ParamRGraph.new_named_vertex ("a") ();
val g = g |> ParamRGraph.new_named_vertex ("b") ();
val g = g |> ParamRGraph.new_named_vertex ("c") ();
ParamRGraph.print g; 
val g = g |> ParamRGraph.new_named_edge ("R0") () 
        (ParamRGraph.Vertex.NSet.of_list (["a","b"]));
val g = g |> ParamRGraph.new_named_edge ("R1") ()
        (ParamRGraph.Vertex.NSet.of_list (["b","c"]));;
ParamRGraph.print g; 
ParamRGraph.shortest_path g ("a") ("c"); 

ParamRGraph.get_connected_vertices g ("a");
