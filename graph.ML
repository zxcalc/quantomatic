(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Graphs with named nodes and edges. *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* *)

signature BASIC_GRAPH = 
sig

structure NE : NODES_AND_EDGES;

(* node names and nodes, edge names and edges *)
type nname = NName.T
type node = NE.node
type ename = EName.T
type edge = NE.edge;

type T; (* a graph *)

exception no_such_nname_exp of string * nname * T;
exception nname_already_exists_exp of nname * T;

exception no_such_ename_exp of string * ename * T;
exception ename_already_exists_exp of ename * T;

(* Making Graphs *)
val empty : T
(* adding a node gives back unique name for it and the new graph *)
val add_node : node -> T -> nname * T 
val doadd_node : node -> T -> T
(* nname = suggested name *)
val add_named_node : nname -> node -> T -> nname * T 
val new_named_node : nname -> node -> T -> T  (* can raise *)

(* nnames must already exist, else raises: no_such_nname_exp *)
val add_edge : edge -> nname -> nname -> T -> ename * T
val doadd_edge : edge -> nname -> nname -> T -> T
val add_named_edge : ename -> edge -> nname -> nname -> T -> ename * T 
val new_named_edge : ename -> edge -> nname -> nname -> T -> T (* can raise *)

(* delete raises an exception if there is no such edge/node *)
val delete_edge : ename -> T -> T
val delete_node : nname -> T -> T

(* replace all occurances of old name (1st arg) with new name (2nd arg) *)
(*  2nd name must not already exist and 1st must already exist, 
    else an exception is raised *)
val rename_nname : nname -> nname -> T -> T 
val rename_ename : ename -> ename -> T -> T 

(* rename the 2nd graph to avoid all names in the first one *)
val rename_apart : T -> T -> T 

(* update a node's data *)
val update_node : (node -> node) -> nname -> T -> T
val update_edge : (edge -> edge) -> ename -> T -> T
val set_node : node -> nname -> T -> T
val set_edge : edge -> ename -> T -> T

(* getting stuff from graphs *)
val get_node : T -> nname -> (node * (EName.NTab.N.T * EName.NTab.N.T))
val get_edge : T -> ename -> (edge * (nname * nname))
val lookup_node : T -> nname 
									-> (node * (EName.NTab.N.T * EName.NTab.N.T)) option
val lookup_edge : T -> ename 
									-> (edge * (nname * nname)) option

(* working with names in the graph, no exceptions *)
val useup_nnames : nname list -> T -> T
val useup_enames : ename list -> T -> T

(* getting graph internal representation... *)
val get_node_list : T -> (nname * (node 
                   * (EName.NTab.N.T (* edges coming into this node *)
                      * EName.NTab.N.T))) (* edges leaving this node *)
                    list;
val get_edge_list : T -> (ename * (edge 
                   * (nname (* from this node *)
                      * nname))) (* to this node *)
                    list

val get_nodes : T 
                -> (node 
                    * (EName.NTab.N.T (* edges coming into this node *)
                       * EName.NTab.N.T)) (* edges leaving this node *)
                   NName.NTab.T;
val get_edges : T 
                -> (edge 
                    * (nname (* from this node *)
                       * nname)) (* to this node *)
                   EName.NTab.T

(* pretty printing *)
val pretty : T -> Pretty.T
val pretty_node : node -> Pretty.T
val pretty_edge : edge -> Pretty.T

val print : T -> unit
val print_node : node -> unit
val print_edge : edge -> unit

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graphs with strings at edges and nodes *)
functor BasicGraphFun(NE : NODES_AND_EDGES) 
(* : BASIC_GRAPH where type NE.node = NE.node and type NE.edge = NE.edge *)
= struct

structure NE = NE;

(* node names and nodes *)
type nname = NName.T;
type node = NE.node;
structure NnTab = NName.NTab;
structure NnCol = NName.Col;

(* edge names and edges *)
type ename = EName.T;
type edge = NE.edge;
structure EnTab = EName.NTab;
structure EnCol = EName.Col;

(* a graph *)
datatype T = Graph of 
         {
          nodes : (node 
                   * (EName.NTab.N.T (* edges coming into this node *)
                      * EName.NTab.N.T)) (* edges leaving this node *)
                    NnTab.T,
          edges : (edge 
                   * (nname (* from this node *)
                      * nname)) (* to this node *)
                    EnTab.T
         };

val empty = Graph { nodes = NnTab.empty, edges = EnTab.empty };
fun update_nodes f (Graph rep) = 
    Graph { nodes = f (#nodes rep), edges = #edges rep };
fun update_edges f (Graph rep) = 
    Graph { nodes = #nodes rep, edges = f (#edges rep)};
val set_nodes = update_nodes o K;
val set_edges = update_edges o K;
fun get_nodes (Graph rep) = #nodes rep;
fun get_edges (Graph rep) = #edges rep;
val get_node_list = NnTab.dest o get_nodes;
val get_edge_list = EnTab.dest o get_edges;

fun lookup_node g n = 
    case NnTab.lookup (get_nodes g) n 
     of NONE => NONE | SOME x => SOME x;
fun lookup_edge g en = 
    case EnTab.lookup (get_edges g) en 
     of NONE => NONE | SOME x => SOME x;

exception no_such_nname_exp of string * nname * T;
exception nname_already_exists_exp of nname * T;
exception no_such_ename_exp of string * ename * T;
exception ename_already_exists_exp of ename * T;

fun get_node g n = 
    case lookup_node g n 
     of NONE => raise no_such_nname_exp ("get_node", n, g)
      | SOME x => x;
fun get_edge g en = 
    case lookup_edge g en 
     of NONE => raise no_such_ename_exp ("get_edge", en, g)
      | SOME x => x;

(* adding a node gives back unique name for it and the new graph *)
fun add_named_node n nd g =
    let val (n2,nodes2) = NnTab.add (n,(nd,(EName.NTab.N.empty,EName.NTab.N.empty))) (get_nodes g)
    in (n2, set_nodes nodes2 g) end;
(* implicit args: nd g *)
val add_node = add_named_node NE.default_nname;
val doadd_node = snd oo add_node;
(* implicit arg: g *)
fun new_named_node n nd =
    update_nodes (NnTab.ins (n,(nd,(EName.NTab.N.empty,EName.NTab.N.empty))));

(* working with names in the graph, no exceptions *)
val useup_nnames = update_nodes o NName.NTab.useup_names;
val useup_enames = update_edges o EName.NTab.useup_names;

(* implicit args: g n *)
val has_nname = NnTab.contains o get_nodes;
val has_ename = EnTab.contains o get_edges;


fun add_named_edge en e n1 n2 g = 
    let 
      (* make sure the nodes exist *) 
      val _ = if has_nname g n1 then
                if has_nname g n2 then () 
                else raise no_such_nname_exp ("add_named_edge", n2, g)
              else raise no_such_nname_exp ("add_named_edge", n1, g);
      (* add the new egde *)
      val (en2,edges2) = EnTab.add (en,(e,(n1,n2))) (get_edges g)

      (* update the nodes: add edge name as appropriate to ins and outs *)
      val nodes2 = 
          (get_nodes g)
            |> NnTab.map_entry 
						(fn (n,(ins,outs)) => (n,(ins,EName.NTab.N.ins_fresh en2 outs))) n1
            |> NnTab.map_entry 
						(fn (n,(ins,outs)) => (n,(EName.NTab.N.ins_fresh en2 ins,outs))) n2
      (* give back new edge name and new graph *)
    in (en2, Graph{nodes = nodes2, edges = edges2}) end;

(* implicit args: e n1 n2 g *)
val add_edge = add_named_edge NE.default_ename;
val doadd_edge = snd oooo add_edge;

fun new_named_edge en e n1 n2 g =
    let 
      (* make sure the nodes exist *) 
      val _ = if has_nname g n1 then
                if has_nname g n2 then () 
                else raise no_such_nname_exp ("add_named_edge", n2, g)
              else raise no_such_nname_exp ("add_named_edge", n1, g);
      (* add the new egde *)
      val edges2 = EnTab.ins (en,(e,(n1,n2))) (get_edges g)

      (* update the nodes: add edge name as appropriate to ins and outs *)
      val nodes2 = 
          (get_nodes g)
            |> NnTab.map_entry 
						(fn (n,(ins,outs)) => (n,(ins,EName.NTab.N.ins_fresh en outs))) n1
            |> NnTab.map_entry 
						(fn (n,(ins,outs)) => (n,(EName.NTab.N.ins_fresh en ins,outs))) n2
      (* give back new edge name and new graph *)
    in Graph{nodes = nodes2, edges = edges2} end;

(* deleting node also removes all it's edges. *)
fun delete_node n g = 
    let 
      val nodes = get_nodes g
      val (nd,(ins,outs)) =
          case NnTab.lookup nodes n of 
            NONE => raise no_such_nname_exp ("delete_node", n, g)
          | SOME x => x;

      val nodes2 = nodes |> NnTab.delete n

      (* delete all edges going into or out of this node. *)
      val edges2 = 
          (get_edges g)
            |> EName.NTab.N.fold EnTab.delete ins
            |> EName.NTab.N.fold EnTab.delete outs

      (* give back new edge name and new graph *)
    in Graph{nodes = nodes2, edges = edges2} end;

(* implicit args: en g *)
val delete_edge = 
    update_edges o EnTab.delete;


(* nname = suggested name *)
(* rename all occurances of old name (1st arg) with new name (2nd arg) 
   n2 must not exist already, n1 must exist *)
fun rename_nname n1 n2 g = 
    let 
      val nodes = (get_nodes g)
      (* gets node for n1, makes ure it exists *)
      val (nd,(ins,outs)) = case NnTab.lookup nodes n1 
                             of NONE => raise no_such_nname_exp ("rename_nname", n1, g)
                              | SOME x => x;

      (* update the node entry for n1, we make sure that n2 doesn't already exist *)
      val nodes2 = 
          case (nodes |> NnTab.delete n1
                      |> NnTab.try_ins (n2,(nd,(ins,outs))))
           of NONE => raise nname_already_exists_exp (n2,g)
            | SOME nodes2 => nodes2;

      (* update any edges to use the new name n1 instead of n1, using ins and outs lists *)
      val edges2 = 
          (get_edges g)
            |> EName.NTab.N.fold (EnTab.map_entry (fn (e,(from,_)) => (e,(from,n2)))) ins
            |> EName.NTab.N.fold (EnTab.map_entry (fn (e,(_,tos)) => (e,(n2,tos)))) outs;

      (* give back new edge name and new graph *)
    in Graph{nodes = nodes2, edges = edges2} end;


fun rename_ename en1 en2 g = 
    let 
      val edges = (get_edges g)
      (* gets node for n1, makes ure it exists *)
      val (e,(from_n,to_n)) = case EnTab.lookup edges en1
                           of NONE => raise no_such_ename_exp ("rename_nname", en1, g)
                            | SOME x => x;

      (* update the node entry for n1, we make sure that n2 doesn't already exist *)
      val edges2 = 
          case (edges |> EnTab.delete en1
                      |> EnTab.try_ins (en2,(e,(from_n,to_n))))
           of NONE => raise ename_already_exists_exp (en2,g)
            | SOME edges2 => edges2;

      (* update any edges to use the new name n1 instead of n1, using ins and outs lists *)
      val nodes2 = 
          (get_nodes g)
            |> NnTab.map_entry (fn (n,(ins,outs)) => 
                                   let val outs2 = outs |> EName.NTab.N.delete en1
                                                        |> EName.NTab.N.ins_fresh en2
                                   in (n,(ins,outs2)) end) from_n
            |> NnTab.map_entry (fn (n,(ins,outs)) =>
                                   let val ins2 = ins |> EName.NTab.N.delete en1
                                                      |> EName.NTab.N.ins_fresh en2
                                   in (n,(ins2,outs)) end) to_n;

      (* give back new edge name and new graph *)
    in Graph{nodes = nodes2, edges = edges2} end;



(* rename the names in g2 to avoid those in g1 *)

(* IMPROVE: avoid conversion to lists as uses up lots of memory: have
a function in FNAMETAB to useup names from a name collection *)
fun rename_apart avoidg ing = 
		let val nnames = (get_nodes avoidg)
				val enames = (get_edges avoidg)
				val g2 = ing |> useup_nnames (NName.NTab.keys nnames)
										 |> useup_enames (EName.NTab.keys enames)
		in
			g2 |> (NName.NTab.fold 
							 (fn (n,_) => fn g3 =>
									 if NName.NTab.contains nnames n then
										 rename_nname 
											 n (NName.NTab.N.new n (NName.NTab.get_names 
																								(get_nodes g3)))
											 g3
									 else g3)
							 (get_nodes g2))
				 |> (EName.NTab.fold 
							 (fn (n,_) => fn g3 =>
									if EName.NTab.contains enames n then
										rename_ename 
											n (EName.NTab.N.new n (EName.NTab.get_names 
																							 (get_edges g3)))
											g3
									else g3)
							 (get_edges g2))
		end;

(* implicit arg: g *)
fun update_node f n = 
    update_nodes (NnTab.map_entry (fn (nd,ios) => (f nd,ios)) n);
fun update_edge f en = 
    update_edges (EnTab.map_entry (fn (e,fromto) => (f e,fromto)) en);
val set_node = update_node o K;
val set_edge = update_edge o K;

(* pretty printing *)
val pretty_nname = NName.pretty;
val pretty_ename = EName.pretty;
val pretty_node = NE.pretty_node;
val pretty_edge = NE.pretty_edge;

val print_nname = Pretty.writeln o pretty_nname;
val print_ename = Pretty.writeln o pretty_ename;
val print_node = Pretty.writeln o pretty_node;
val print_edge = Pretty.writeln o pretty_edge;

fun pretty g = 
    Pretty.chunks 
      [Pretty.str "Graph{",
       Pretty.block 
         [Pretty.str "  ", (* indent *)
          Pretty.chunks 
          [Pretty.block 
             [Pretty.str "Nodes: ",
              Pretty.chunks 
                (map 
                   (fn (n, (nd,(ins,outs))) => 
                       Pretty.block 
                         [pretty_nname n,
                          Pretty.str "; ",
                          Pretty.list "i{" "}" (map pretty_ename (EName.NTab.N.dest ins)),
                          Pretty.list "; o{" "}" (map pretty_ename (EName.NTab.N.dest outs)),
                          Pretty.str "; ",
                          pretty_node nd
                   ])
                   (NnTab.dest (get_nodes g)))],
           Pretty.block
             [Pretty.str "Edges: ",
              Pretty.chunks 
                (map 
                   (fn (en, (e, (n1, n2))) => 
                       Pretty.block 
                         [pretty_ename en,
                          Pretty.str ": ",
                          pretty_nname n1,
                          Pretty.str " - ",
                          pretty_nname n2
                   ])
                   (EnTab.dest (get_edges g)))]]],
       Pretty.str "}"
      ];
     
val print = Pretty.writeln o pretty;

end;


(* An instance of Basic Graph: strings at nodes and edges *)
structure DB_StrGraph = BasicGraphFun(StrNE);
structure StrGraph :> BASIC_GRAPH where 
type NE.node = StrNE.node 
 and type NE.edge = StrNE.edge 
= DB_StrGraph;
