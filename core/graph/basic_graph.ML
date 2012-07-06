(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Graphs with named vertices and edges. *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

signature BASIC_GRAPH_SHARING = 
sig
structure EData : UNIFIABLE_DATA_SHARING;
structure VData : UNIFIABLE_DATA_SHARING;
sharing type EData.subst = VData.subst; 
structure Param : GRAPH_PARAM_SHARING;
sharing Param.VData = VData;
sharing Param.EData = EData;
type dir_or_undir;
type T;
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* Basic (Minimal) interface for Graphs *)
signature BASIC_GRAPH = 
sig

structure VData : UNIFIABLE_DATA
structure EData : UNIFIABLE_DATA
structure Param : GRAPH_PARAM

(* each edge is either directed, or bi-directed: this is the family of graphs
   with these edges in both directions. *)
datatype dir_or_undir = Directed | UnDirected;

type T; (* a graph *)

structure SharingBasicGraph : BASIC_GRAPH_SHARING
sharing SharingBasicGraph.EData = EData.Sharing;
sharing SharingBasicGraph.VData = VData.Sharing;
sharing SharingBasicGraph.Param = Param.Sharing;
sharing type SharingBasicGraph.T = T;
sharing type SharingBasicGraph.dir_or_undir = dir_or_undir;

exception bug_exp of string;

exception no_such_xname_exp of string * X.name * T;
exception xname_already_exists_exp of X.name * T;

exception no_such_vname_exp of string * V.name * T;
exception no_such_vnames_exp of string * V.NSet.T * T;
exception vname_already_exists_exp of V.name * T;

exception no_such_ename_exp of string * E.name * T;
exception ename_already_exists_exp of E.name * T;

(* Making Graphs *)
val empty : T

val get_xnames : T -> X.NSet.T

(* adding a vertex gives back unique name for it and the new graph *)
(* V.name = suggested name *)
val add_named_vertex : V.name -> VData.data -> T -> V.name * T 

(* V.names must already exist, else raises: no_such_vname_exp *)
val add_named_edge : E.name -> (dir_or_undir * EData.data)
    -> V.name -> V.name -> T -> E.name * T 

(* delete raises an exception if there is no such edge/vertex *)
val delete_edge : E.name -> T -> T
val delete_vertex : V.name -> T -> T

(* replace all occurances of old name (1st arg) with new name (2nd arg) *)
(*  2nd name must not already exist and 1st must already exist, 
    else an exception is raised *)
val rename_xname : X.name -> X.name -> T -> T 
val rename_vname : V.name -> V.name -> T -> T 
val rename_ename : E.name -> E.name -> T -> T 

(* update a data of a vertex or edge *)
val update_vertex_data : (VData.data -> VData.data) -> V.name -> T -> T
val update_edge_data : (EData.data -> EData.data) -> E.name -> T -> T

(* getting graph internal representation... *)
val get_vertices : T 
                -> (VData.data 
                    * (E.NSet.T (* edges coming into this vertex *)
                       * E.NSet.T (* edges leaving this vertex *)
                       ))
                   V.NTab.T;
val get_edges : T 
                -> ((dir_or_undir * EData.data) 
                    * (V.name (* from this vertex *)
                       * V.name)) (* to this vertex *)
                   E.NTab.T
val get_param : T -> Param.T

(* updating graph internal representation... *)
val update_vertices : ((VData.data * (E.NSet.T * E.NSet.T)) 
                         V.NTab.T 
                         -> (VData.data * (E.NSet.T * E.NSet.T)) 
                              V.NTab.T) -> T -> T
val update_edges : (((dir_or_undir * EData.data) 
                      * (V.name * V.name)) E.NTab.T
                    -> ((dir_or_undir * EData.data) 
                        * (V.name * V.name)) E.NTab.T)
                   -> T -> T
val update_param : (Param.T -> Param.T) -> T -> T


(* pretty printing *)
val pretty : T -> Pretty.T
val print : T -> unit

val pretty_theory_name : T -> Pretty.T

(* extra data for graphs, for extending the datatype cos ML can't do
it properly from the outside; this is basically a form of record
extensibility, allowing hidden types, but will doing the lazy
extension of data -- allows OOP style extension of signature/structure
by refining data. *)
type data
val get_data : T -> data;
val update_data : (data -> data) -> T -> T

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Basic Graphs with strings at edges and vertices *)
functor BasicGraphFun(
  structure Param : GRAPH_PARAM
  type data (* graph additional extensible/hidden data *)
  val empty_data : data
) 
: BASIC_GRAPH
= struct

structure VData = Param.VData;
structure EData = Param.EData;
structure Param = Param;

exception bug_exp of string;

(* extra data, allows extra data to be added later in decendent
signatures/structures; this provides a style of OOP for normal
structured ML. *)
type data = data;

(* is the edge representing a singtle graph with the directed edge, or is it
 representing the family of graphs with this edge in both directions. *)
datatype dir_or_undir = Directed | UnDirected;

(* a graph *)
datatype T = Graph of 
         {
          vertices : (VData.data 
                   * (E.NSet.T (* edges coming into this vertex *)
                      * E.NSet.T)) (* edges leaving this vertex *)
                    V.NTab.T,
          edges : ((dir_or_undir * EData.data)
                   * (V.name (* from this vertex *)
                      * V.name)) (* to this vertex *)
                    E.NTab.T,  
          (* dependencies between names in vertex data and other graph things *)
          param : Param.T,
          (* other global data that extends the graph definition, 
             e.g. boundary vertices, bang-boxes, etc *)
          data : data
         };

(* *)
structure SharingBasicGraph = struct
  structure VData = VData.Sharing;
  structure EData = EData.Sharing;
  structure Param = Param.Sharing;
  type T = T;
  type dir_or_undir = dir_or_undir;
end;

(* extensible/internal data stuff *)
fun get_data (Graph rep) = #data rep;
val empty_data = empty_data;
fun update_data f (Graph rep) = 
    Graph { vertices = #vertices rep, edges = #edges rep,
            param = #param rep, data = f (#data rep)};

(* basic graph stuff... *)
val empty = Graph { vertices = V.NTab.empty, 
                    edges = E.NTab.empty,
                    param = Param.empty,
                    data = empty_data};
fun update_vertices f (Graph rep) = 
    Graph { vertices = f (#vertices rep), edges = #edges rep, 
            param = #param rep, data = #data rep };
fun update_edges f (Graph rep) = 
    Graph { vertices = #vertices rep, edges = f (#edges rep),
            param = #param rep, data = #data rep };
fun update_param f (Graph rep) = 
    Graph { vertices = #vertices rep, edges = #edges rep,
            param = f(#param rep), data = #data rep };
val set_vertices = update_vertices o K;
val set_edges = update_edges o K;
val set_param = update_param o K;

fun get_vertices (Graph rep) = #vertices rep;
fun get_edges (Graph rep) = #edges rep;
fun get_param (Graph rep) = #param rep;

fun get_xnames g = Param.get_xnames (get_param g);

val get_vertex_list = V.NTab.list_of o get_vertices;
val get_edge_list = E.NTab.list_of o get_edges;
val get_vertex_nset = V.NTab.get_nameset o get_vertices;
val get_edge_nset = E.NTab.get_nameset o get_edges;

(* implicit: g n *)
val lookup_vertex = V.NTab.lookup o get_vertices
val lookup_edge = E.NTab.lookup o get_edges

exception no_such_xname_exp of string * X.name * T;
exception xname_already_exists_exp of X.name * T;
exception no_such_vname_exp of string * V.name * T;
exception no_such_vnames_exp of string * V.NSet.T * T;
exception vname_already_exists_exp of V.name * T;
exception no_such_ename_exp of string * E.name * T;
exception ename_already_exists_exp of E.name * T;

fun get_vertex g n = 
    case lookup_vertex g n 
     of NONE => raise no_such_vname_exp ("get_vertex", n, g)
      | SOME x => x;
fun get_edge g en = 
    case lookup_edge g en 
     of NONE => raise no_such_ename_exp ("get_edge", en, g)
      | SOME x => x;

(* implicit arg: g *)
fun update_vertex_data f n g = 
    let val (d,_) = get_vertex g n 
        val d2 = f d
    in
      g |> update_vertices (V.NTab.map_entry (fn (_,ios) => (d2,ios)) n)
        |> update_param (Param.replace_vertex d2 n)
    end;
fun update_vertex_ins f n = 
    update_vertices (V.NTab.map_entry 
                       (fn (nd,(ins,outs)) => (nd,(f ins, outs))) n);
fun update_vertex_outs f n = 
    update_vertices (V.NTab.map_entry 
                       (fn (nd,(ins,outs)) => (nd,(ins, f outs))) n);
fun update_edge f n g = 
    let val e = get_edge g n 
        val e2 as ((bd,d2),fromto) = f e
    in
      g |> update_edges (E.NTab.map_entry (fn _ => e2) n)
        |> update_param (Param.replace_edge d2 n)
    end;
fun update_edge_data f n g = 
    let val ((bd,d),fromto) = get_edge g n 
        val d2 = f d
    in
      g |> update_edges (E.NTab.map_entry (fn _ => ((bd,d2),fromto)) n)
        |> update_param (Param.replace_edge d2 n)
    end;
(*     update_edge (fn ((bd,d),fromto) => ((bd,f d),fromto)); *)
fun update_edge_dir_or_undir f = 
    update_edge (fn ((bd,d),fromto) => ((f bd,d),fromto));

(* adding a vertex gives back unique name for it and the new graph *)
fun add_named_vertex n nd g =
    let val (n2,vertices2) = 
            V.NTab.add (n,(nd,(E.NSet.empty,E.NSet.empty))) 
                            (get_vertices g)
    in (n2, g |> set_vertices vertices2
              |> update_param (Param.add_vertex nd n2)) 
    end;

(* implicit args: g n *)
val has_vname = V.NTab.contains_name o get_vertices;
val has_ename = E.NTab.contains_name o get_edges;

fun add_named_edge en (e as (bd,d)) n1 n2 g = 
    let 
      (* make sure the vertices exist *) 
      val _ = if has_vname g n1 then
                if has_vname g n2 then () 
                else raise no_such_vname_exp ("add_named_edge", n2, g)
              else raise no_such_vname_exp ("add_named_edge", n1, g);
      (* add the new egde *)
      val (en2,edges2) = E.NTab.add (en,(e,(n1,n2))) (get_edges g)

      (* update the vertices: add edge name as appropriate to ins and outs *)
      val vertices2 = 
          (get_vertices g)
            |> V.NTab.map_entry 
						(fn (n,(ins,outs)) => (n,(ins,E.NSet.ins_fresh en2 outs))) n1
            |> V.NTab.map_entry 
						(fn (n,(ins,outs)) => (n,(E.NSet.ins_fresh en2 ins,outs))) n2
      (* give back new edge name and new graph *)
    in (en2, g |> set_vertices vertices2
               |> set_edges edges2
               |> update_param (Param.add_edge d en2)) 
    end;

(* implicit args: en g *)
(* *)
fun delete_edge en g = 
    let 
      val edges = get_edges g
      val (ed,(fromvn,tovn)) =
          case E.NTab.lookup edges en of 
            NONE => raise no_such_ename_exp ("delete_edge", en, g)
          | SOME x => x;

      val edges2 = edges |> E.NTab.delete en

      (* delete all edges going into or out of this vertex. *)
      val vertices2 = 
          (get_vertices g)
            |> V.NTab.map_entry (fn (d,(ins,outs)) => 
                                         (d,(ins,E.NSet.delete en outs))) fromvn
            |> V.NTab.map_entry (fn (d,(ins,outs)) => 
                                         (d,(E.NSet.delete en ins,outs))) tovn;

      (* give back new edge name and new graph *)
    in g |> set_vertices vertices2
         |> set_edges edges2 
         |> update_param (Param.delete_ename en)
    end;

(* deleting vertex also removes all it's edges. *)
fun delete_vertex n g = 
    let 
      val vertices = get_vertices g
      val (nd,(ins,outs)) =
          case V.NTab.lookup vertices n of 
            NONE => raise no_such_vname_exp ("delete_vertex", n, g)
          | SOME x => x;

      (* delete all edges going into or out of this vertex. *)
      val g2 = 
          g |> E.NSet.fold delete_edge ins
            |> E.NSet.fold delete_edge (E.NSet.subtract outs ins)

    in g2 |> update_vertices (V.NTab.delete n)
          |> update_param (Param.delete_vname n)
    end;

exception rename_do_nothing_exp of unit;

(* implicit args: n1 n2 g; raises X.Rnm. *)
(* renames internal variable name n1 to n2 (assuming n2 is fresh in this graph)
   Updates the name in the param data as well as in each vertex and edge data in
   which the name occurs. *)
fun rename_xname n1 n2 g =
    let 
      val param = get_param g;
      (* val _ = if X.NSet.contains (Param.get_xnames param) n2 then 
                raise xname_already_exists_exp (n2,g)
              else (); *)
      val vnames_to_update = Param.dep_vnames_of_xname param n1 
      val enames_to_update = Param.dep_enames_of_xname param n1
    in 
      g |> update_param (Param.rename_xname n1 n2)
        |> update_vertices (
            V.NSet.fold (V.NTab.map_entry (fn (d,ios) => (
                  Param.subst_xname_in_vdata n1 n2 d,ios))) 
              vnames_to_update)
        |> update_edges (
            E.NSet.fold (E.NTab.map_entry (fn ((dir,d),fromto) => 
                  ((dir,Param.subst_xname_in_edata n1 n2 d),fromto))) 
              enames_to_update)
    end handle X.Rnm.invalid_renaming_exp _ => 
        raise xname_already_exists_exp (n2,g);

(* V.name = suggested name *)
(* rename all occurances of old name (1st arg) with new name (2nd arg) 
   n2 must not exist already, n1 must exist *)
fun rename_vname n1 n2 g = 
    let 
      val vertices = (get_vertices g)
      (* gets vertex for n1, makes ure it exists *)
      val (nd,(ins,outs)) = case V.NTab.lookup vertices n1 
                             of NONE => raise rename_do_nothing_exp ()
                              | SOME x => x;

      (* update the vertex entry for n1, we make sure that n2 doesn't already exist *)
      val vertices2 = 
          case (vertices |> V.NTab.delete n1
                         (* raises exception if n2 already exsists *)
                         |> V.NTab.try_ins (n2,(nd,(ins,outs))))
           of NONE => raise vname_already_exists_exp (n2,g)
            | SOME vertices2 => vertices2;

      (* update any edges to use the new name n1 instead of n1, using ins and outs lists *)
      val edges2 = 
          (get_edges g)
            |> E.NSet.fold (E.NTab.map_entry (fn (e,(from,_)) => 
                                                       (e,(from,n2)))) ins
            |> E.NSet.fold (E.NTab.map_entry (fn (e,(_,tos)) => 
                                                       (e,(n2,tos)))) outs;

      (* give back new graph *)
    in g |> set_vertices vertices2
         |> set_edges edges2
         |> update_param (Param.rename_vname n1 n2)
    end handle rename_do_nothing_exp () => g;
    (* raise no_such_vname_exp ("rename_vname", n1, g) *)

fun rename_ename en1 en2 g = 
    let 
      val edges = (get_edges g)
      (* gets edge for n1, makes ure it exists *)
      val (e,(from_n,to_n)) = case E.NTab.lookup edges en1
                           of NONE => raise rename_do_nothing_exp ()
                            | SOME x => x;

      (* update the edge entry for n1, we make sure that n2 doesn't 
         already exist *)
      val edges2 = 
          case (edges |> E.NTab.delete en1
                      |> E.NTab.try_ins (en2,(e,(from_n,to_n))))
           of NONE => raise ename_already_exists_exp (en2,g)
            | SOME edges2 => edges2;

      (* update any edges to use the new name n1 instead of n1, using 
         ins and outs lists *)
      val vertices2 = 
          (get_vertices g)
            |> V.NTab.map_entry (fn (n,(ins,outs)) => 
                                   let val outs2 = outs |> E.NSet.delete en1
                                                        |> E.NSet.ins_fresh en2
                                   in (n,(ins,outs2)) end) from_n
            |> V.NTab.map_entry (fn (n,(ins,outs)) =>
                                   let val ins2 = ins |> E.NSet.delete en1
                                                      |> E.NSet.ins_fresh en2
                                   in (n,(ins2,outs)) end) to_n;

    (* return new graph *)
    in g |> set_vertices vertices2
         |> set_edges edges2
         |> update_param (Param.rename_ename en1 en2)
    end handle rename_do_nothing_exp () => g;
    (* no_such_ename_exp ("rename_vname", en1, g) *)


fun pretty_edge_dir_or_undir Directed = Pretty.str "dir"
  | pretty_edge_dir_or_undir UnDirected = Pretty.str "bidir";

fun pretty_edge_connector Directed = Pretty.str "->"
  | pretty_edge_connector UnDirected = Pretty.str "--";

fun pretty_theory_name g = Param.pretty_theory_name;

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
                         [V.pretty_name n,
                          Pretty.str ":=",
                          Pretty.list "i{" "}" (map E.pretty_name (E.NSet.list_of ins)),
                          Pretty.list "o{" "}" (map E.pretty_name (E.NSet.list_of outs)),
                          Pretty.str "(",
                          VData.pretty_data nd,
                          Pretty.str ")"
                   ])
                   (V.NTab.list_of (get_vertices g)))],
           Pretty.block
             [Pretty.str "Edges: ",
              Pretty.chunks 
                (map 
                   (fn (en, ((bd,e), (n1, n2))) => 
                       Pretty.block 
                         [E.pretty_name en,
                          Pretty.str ":=",
                          V.pretty_name n1,
                          pretty_edge_connector bd,
                          V.pretty_name n2,
                          Pretty.str "(",
                          EData.pretty_data e,
                          Pretty.str ") "
                   ])
                   (E.NTab.list_of (get_edges g)))]]],
       Pretty.str "}"
      ];
     
val print = Pretty.writeln o pretty;

end;


