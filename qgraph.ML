(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Ross' Quantum Graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graphs with given inputs/outputs *)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature QGRAPH = 
sig
structure QuNE : Q_NODES_AND_EDGES

structure PatGraph : PATTERN_GRAPH
where 
  type PatNE.node_pat = QuNE.node
  and type PatNE.edge_pat = QuNE.edge;

include BASIC_GRAPH sharing NE = QuNE;

(* where
  type NE.node = QuNE.node
  and type NE.edge = QuNE.edge; *)

val get_patgraph : T -> PatGraph.T
val get_inputs : T -> NName.Col.T
val get_outputs : T -> NName.Col.T
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* A Quantum Graph *)
functor QGraphFun(QuNE : Q_NODES_AND_EDGES)
= struct 

structure QuNE = QuNE;
structure NE = QuNE;

structure PatGraph = PatGraphFun(
  structure PatNE = NE
  structure Expr = BasicQuExpr);

type ename = EName.T;
type nname = NName.T;

type node = NE.node;
type edge = NE.edge;

structure NnTab = NName.NTab;
structure NnCol = NName.Col;
structure EnTab = EName.NTab;
structure EnCol = EName.Col;

datatype T = QGraph of 
     { graph : PatGraph.T,
       inputs : NnCol.T,
       outputs : NnCol.T
     };

exception no_such_nname_exp of string * nname * T;
exception nname_already_exists_exp of nname * T;

exception no_such_ename_exp of string * ename * T;
exception ename_already_exists_exp of ename * T;

val empty = QGraph { graph = PatGraph.empty, 
                     inputs = NnCol.empty, 
                     outputs = NnCol.empty };

fun update_patgraph f (QGraph rep) = 
    QGraph { graph = f (#graph rep), 
             inputs = #inputs rep, 
             outputs = #outputs rep }; 
fun update_inputs f (QGraph rep) = 
    QGraph { graph = #graph rep, 
             inputs = f(#inputs rep), 
             outputs = #outputs rep };
fun update_outputs f (QGraph rep) = 
    QGraph { graph = #graph rep, 
             inputs = #inputs rep, 
             outputs = f(#outputs rep) };
val set_graph = update_patgraph o K;
val set_inputs = update_inputs o K;
val add_input = update_inputs o NnCol.union1;
val add_inputs = update_inputs o NnCol.union_set;
val del_input = update_inputs o NnCol.delete;
val del_inputs = update_inputs o NnCol.minus_set;
val set_outputs = update_outputs o K;
val add_output = update_outputs o NnCol.union1;
val add_outputs = update_outputs o NnCol.union_set;
val del_output = update_outputs o NnCol.delete;
val del_outputs = update_outputs o NnCol.minus_set;
fun get_patgraph (QGraph rep) = #graph rep;
fun get_inputs (QGraph rep) = #inputs rep;
fun get_outputs (QGraph rep) = #outputs rep;

val get_nodes = PatGraph.get_nodes o get_patgraph;
val get_edges = PatGraph.get_edges o get_patgraph;
val get_node_list = NnTab.dest o get_nodes;
val get_edge_list = EnTab.dest o get_edges;

(* implicit args: q n *)
val get_node = PatGraph.get_node o get_patgraph;
val get_edge = PatGraph.get_edge o get_patgraph;
val lookup_node = PatGraph.lookup_node o get_patgraph;
val lookup_edge = PatGraph.lookup_edge o get_patgraph;

(* adding a node gives back unique name for it and the new graph *)
fun add_named_node n (nd as NE.VarNode in_or_out) q =
    let val (n2,g2) = PatGraph.add_named_node n nd (get_patgraph q)
    in (n2,
        q |> (case in_or_out of NE.InputNd => add_input n
                              | NE.OutputNd => add_output n)
          |> set_graph g2)
    end
  | add_named_node n nd q =
    let val (n2,g2) = PatGraph.add_named_node n nd (get_patgraph q)
    in (n2, set_graph g2 q) end;

fun new_named_node n (nd as NE.VarNode in_or_out) q =
    let val g2 = PatGraph.new_named_node n nd (get_patgraph q)
    in q |> (case in_or_out of NE.InputNd => add_input n
                             | NE.OutputNd => add_output n)
         |> set_graph g2
    end
  | new_named_node n nd q =
    let val g2 = PatGraph.new_named_node n nd (get_patgraph q)
    in set_graph g2 q end;

(* implicit args: nd q *)
val add_node = add_named_node NE.default_nname;
val doadd_node = snd oo add_node;

(* implicit args: en e n1 n2 q *)
fun add_named_edge en e n1 n2 q = 
    let val (en2,g2) = PatGraph.add_named_edge en e n1 n2 (get_patgraph q) 
    in (en2, set_graph g2 q) end;
(* implicit: q *)
fun new_named_edge en e n1 n2 =
    update_patgraph (PatGraph.new_named_edge en e n1 n2);

(* implicit args: e n1 n2 q *)
val add_edge = add_named_edge NE.default_ename;
val doadd_edge  = snd oooo add_edge;

(* deleting node also removes all it's edges. *)
fun delete_node n q = 
    q |> (case (get_node q n) 
           of (NE.VarNode NE.InputNd,_) => del_input n
            | (NE.VarNode NE.OutputNd,_) => del_output n
            | _ => I)
      |> update_patgraph (PatGraph.delete_node n);

(* implicit args: en q *)
val delete_edge = update_patgraph o PatGraph.delete_edge;

(* nname = suggested name *)
(* rename all occurances of old name (1st arg) with new name (2nd arg) 
   n2 must not exist already, n1 must exist *)
fun rename_nname n1 n2 q = 
    (case (get_node q n1)
      of (NE.VarNode NE.InputNd,_) => 
         q |> update_inputs (NnCol.delete n1 o NnCol.union1 n2)
           |> update_patgraph (PatGraph.rename_nname n1 n2)
       | (NE.VarNode NE.OutputNd,_) => 
         q |> update_outputs (NnCol.delete n1 o NnCol.union1 n2)
           |> update_patgraph (PatGraph.rename_nname n1 n2)                
       | _ => q |> update_patgraph (PatGraph.rename_nname n1 n2));

(* val implicit args: en1 en2 q *)
val rename_ename = update_patgraph oo PatGraph.rename_ename;
(* implicit arg: f n q *)
val update_node = update_patgraph oo PatGraph.update_node;
(* implicit arg: f en q *)
val update_edge = update_patgraph oo PatGraph.update_edge;
(* implicit arg: nd n q *)
val set_node = update_patgraph oo PatGraph.set_node;
(* implicit arg: en e q *)
val set_edge = update_patgraph oo PatGraph.set_edge;

(* useup names... *)
val useup_nnames = update_patgraph o PatGraph.useup_nnames;
val useup_enames = update_patgraph o PatGraph.useup_enames;

(* implicit args: g1 g2: rename g2 to avoid names in g1 *)
val rename_apart = 
		update_patgraph o (PatGraph.rename_apart o get_patgraph);

(* pretty printing *)
val pretty_node = NE.pretty_node;
val pretty_edge = NE.pretty_edge;
val print_node = Pretty.writeln o pretty_node;
val print_edge = Pretty.writeln o pretty_edge;

fun pretty q = 
    Pretty.chunks 
    [ Pretty.str "QGraph{",
      Pretty.block 
        [Pretty.str "  ", (* indent *)
         Pretty.chunks 
           [Pretty.block 
              [Pretty.str "InputNodes: ",
               Pretty.list "{" "}" 
                           (map NName.pretty (NnCol.dest (get_inputs q)))],
            Pretty.block 
              [Pretty.str "OutputNodes: ",
               Pretty.list "{" "}"
                           (map NName.pretty (NnCol.dest (get_outputs q)))],
            PatGraph.pretty (get_patgraph q)]],
      Pretty.str "}"
    ];

val print = Pretty.writeln o pretty;

end;


structure DB_QGraph = QGraphFun(NE);
structure QGraph : QGRAPH = DB_QGraph;

val Green = NE.Node o BasicQuNE.Znd;
val Red = NE.Node o BasicQuNE.Xnd;
val H = NE.Node BasicQuNE.Hnd;
val Edge = NE.OneEdge BasicQuNE.Edge;
val EEdges = NE.Edges BasicQuNE.Edge;
val InVar = NE.VarNode NE.InputNd;
val OutVar = NE.VarNode NE.OutputNd;
