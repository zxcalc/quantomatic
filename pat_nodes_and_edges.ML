(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Pattern Nodes and Edges *)
signature PATTERN_NODES_AND_EDGES
= sig

type nvdata (* node variable data *)

structure BasicNE : NODES_AND_EDGES

datatype node_pat = 
         Node of BasicNE.node
       | VarNode of nvdata;
datatype edge_pat = 
         OneEdge of BasicNE.edge
       | Edges of BasicNE.edge;

include NODES_AND_EDGES
where type node = node_pat and type edge = edge_pat;

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Pattern Nodes and Edges *)
functor SimplePatNodesAndEdgesFun(structure BasicNE : NODES_AND_EDGES)
: PATTERN_NODES_AND_EDGES 
where type BasicNE.node = BasicNE.node
  and type BasicNE.edge = BasicNE.edge
= struct

structure BasicNE = BasicNE;
datatype nvdata = NVData;

fun nvdata_ord (NVData, NVData) = EQUAL;
fun nvdata_nvdata NVData = Pretty.str "";

datatype node_pat = 
         Node of BasicNE.node
       | VarNode of nvdata;
datatype edge_pat = 
         OneEdge of BasicNE.edge
       | Edges of BasicNE.edge;

type node = node_pat;
type edge = edge_pat;

val default_nname = BasicNE.default_nname;
val default_ename = BasicNE.default_ename;

fun node_ord (VarNode a, VarNode b) = nvdata_ord (a,b)
  | node_ord (VarNode _, _) = LESS
  | node_ord (_, VarNode _) = GREATER
  | node_ord (Node a, Node b) = BasicNE.node_ord (a,b);

fun node_eq n1 n2 = case node_ord (n1, n2) of EQUAL => true | _ => false;

fun edge_ord (OneEdge a,OneEdge b) = BasicNE.edge_ord (a,b)
  | edge_ord (OneEdge Edge,_) = LESS
  | edge_ord (_,OneEdge Edge) = GREATER
  | edge_ord (Edges a, Edges b) = BasicNE.edge_ord (a,b);

fun edge_eq e1 e2 = case edge_ord (e1, e2) of EQUAL => true | _ => false;

fun pretty_node (Node a) = 
    Pretty.block [Pretty.str "Node(", BasicNE.pretty_node a, Pretty.str ")"]  
  | pretty_node (VarNode v) = 
    Pretty.block [Pretty.str "NodeVar(", nvdata_nvdata v, Pretty.str ")"]; 
val print_node = Pretty.writeln o pretty_node;

fun pretty_edge (OneEdge e) = 
    Pretty.block [Pretty.str "OneEdge(", BasicNE.pretty_edge e, Pretty.str ")"]  
  | pretty_edge (Edges e) = 
    Pretty.block [Pretty.str "Edges(", BasicNE.pretty_edge e, Pretty.str ")"];
val print_edge = Pretty.writeln o pretty_edge;


end;



(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Pattern Nodes and Edges with Input/Output for variable nodes *)

signature IO_PATTERN_NODES_AND_EDGES
= sig
datatype vdata = InputNd | OutputNd;
include PATTERN_NODES_AND_EDGES where type nvdata = vdata;
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  IMPROVE: This shares a lot of code with the above: could generalise 
    idea of PatNodes with some given nvdata *)
(*  Pattern Nodes and Edges *)
functor IOPatNodesAndEdgesFun(BasicNE : NODES_AND_EDGES) 
: IO_PATTERN_NODES_AND_EDGES 
where type BasicNE.node = BasicNE.node
  and type BasicNE.edge = BasicNE.edge
= struct

structure BasicNE = BasicNE;

datatype vdata = InputNd | OutputNd;
type nvdata = vdata;

fun nvdata_ord (InputNd, InputNd) = EQUAL
  | nvdata_ord (InputNd, OutputNd) = LESS
  | nvdata_ord (OutputNd, InputNd) = GREATER
  | nvdata_ord (OutputNd, OutputNd) = EQUAL;

fun nvdata_nvdata InputNd = Pretty.str "InputNd"
  | nvdata_nvdata OutputNd = Pretty.str "OutputNd";

datatype node_pat = 
         Node of BasicNE.node
       | VarNode of nvdata;
datatype edge_pat = 
         OneEdge of BasicNE.edge
       | Edges of BasicNE.edge;

type node = node_pat;
type edge = edge_pat;

val default_nname = BasicNE.default_nname;
val default_ename = BasicNE.default_ename;

fun node_ord (VarNode a, VarNode b) = nvdata_ord (a,b)
  | node_ord (VarNode _, _) = LESS
  | node_ord (_, VarNode _) = GREATER
  | node_ord (Node a, Node b) = BasicNE.node_ord (a,b);

fun node_eq n1 n2 = case node_ord (n1, n2) of EQUAL => true | _ => false;

fun edge_ord (OneEdge a,OneEdge b) = BasicNE.edge_ord (a,b)
  | edge_ord (OneEdge Edge,_) = LESS
  | edge_ord (_,OneEdge Edge) = GREATER
  | edge_ord (Edges a, Edges b) = BasicNE.edge_ord (a,b);

fun edge_eq e1 e2 = case edge_ord (e1, e2) of EQUAL => true | _ => false;

fun pretty_node (Node a) = 
    Pretty.block [Pretty.str "Node(", BasicNE.pretty_node a, Pretty.str ")"]  
  | pretty_node (VarNode v) = 
    Pretty.block [Pretty.str "NodeVar(", nvdata_nvdata v, Pretty.str ")"]; 
val print_node = Pretty.writeln o pretty_node;

fun pretty_edge (OneEdge e) = 
    Pretty.block [Pretty.str "OneEdge(", BasicNE.pretty_edge e, Pretty.str ")"]  
  | pretty_edge (Edges e) = 
    Pretty.block [Pretty.str "Edges(", BasicNE.pretty_edge e, Pretty.str ")"];
val print_edge = Pretty.writeln o pretty_edge;


end;

(* In/Out Pattern Graphs with strings at nodes and edges *)
structure PatStrNE = IOPatNodesAndEdgesFun(StrNE);


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Nodes and Edges for quantum graphs *)
signature BASIC_Q_NODES_AND_EDGES
= sig
  type angle = Expr.T;
  val angle_ord : angle * angle -> General.order
  datatype qnd = Xnd of angle (* Red: defined using H of Z *) 
               | Znd of angle (* Green *)
               | Hnd; (* Hadamard node *)
  include NODES_AND_EDGES where type node = qnd;
  val Edge : edge;
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
structure BasicQuNE 
: BASIC_Q_NODES_AND_EDGES
= struct

type angle = Expr.T;

val angle_ord = Expr.ord;

datatype qnd = Xnd of angle (* Red: defined using H of Z *) 
             | Znd of angle (* Green *)
             | Hnd; (* Hadamard node *)
type node = qnd;
datatype edge = Edge;

val default_nname = NName.mk "n_a";
val default_ename = EName.mk "e_a";

fun node_ord (Hnd, Hnd) = EQUAL
  | node_ord (Hnd, _) = LESS
  | node_ord (_, Hnd) = GREATER
  | node_ord (Znd a, Znd b) = angle_ord (a,b)
  | node_ord (Znd _, _) = LESS
  | node_ord (_, Znd _) = GREATER
  | node_ord (Xnd a, Xnd b) = angle_ord (a,b);
fun node_eq n1 n2 = case node_ord (n1, n2) of EQUAL => true | _ => false;

fun edge_ord (Edge,Edge) = EQUAL;
fun edge_eq e1 e2 = case edge_ord (e1, e2) of EQUAL => true | _ => false;

fun pretty_angle a = Pretty.str a;
val print_angle = Pretty.writeln o pretty_angle;
    
fun pretty_node (Xnd a) = 
    Pretty.block [Pretty.str "X(", pretty_angle a, Pretty.str ")"]  
  | pretty_node (Znd a) = 
    Pretty.block [Pretty.str "Z(", pretty_angle a, Pretty.str ")"]
  | pretty_node Hnd = Pretty.str "H";
val print_node = Pretty.writeln o pretty_node;

fun pretty_edge Edge = Pretty.str "<edge>"
val print_edge = Pretty.writeln o pretty_edge;

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature Q_NODES_AND_EDGES = 
  IO_PATTERN_NODES_AND_EDGES where 
  type BasicNE.node = BasicQuNE.node 
  and type BasicNE.edge = BasicQuNE.edge;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
structure QuNE : Q_NODES_AND_EDGES = IOPatNodesAndEdgesFun(BasicQuNE);

structure NE = QuNE;
