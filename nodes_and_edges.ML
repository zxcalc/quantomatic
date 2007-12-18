(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* Node and Edge names *)
structure NName :> STR_NAME = StrName;
structure EName :> STR_NAME = StrName;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* Nodes and Edges *)
signature NODES_AND_EDGES
= sig
  val default_nname : NName.T  
  val default_ename : EName.T  

  type edge
  val edge_eq : edge -> edge -> bool
  val edge_ord : edge * edge -> order

  type node 
  val node_eq : node -> node -> bool
  val node_ord : node * node -> order

  val pretty_node : node -> Pretty.T
  val print_node : node -> unit

  val pretty_edge : edge -> Pretty.T
  val print_edge : edge -> unit
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* Nodes and Edges which are tagged by strings *)
signature STR_NODES_AND_EDGES
= sig

datatype nd = Nd of string;
datatype ed = Ed of string;

include NODES_AND_EDGES
where type node = nd and type edge = ed;

end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Nodes and Edges which associated have strings *)
structure StrNE : STR_NODES_AND_EDGES
= struct

val default_nname = NName.mk "n_a";
val default_ename = EName.mk "e_a";

datatype nd = Nd of string;
datatype ed = Ed of string;

type node = nd;
type edge = ed;

fun node_ord (Nd s1, Nd s2) = String.compare (s1, s2);
fun node_eq n1 n2 = case node_ord (n1, n2) of EQUAL => true | _ => false;

fun edge_ord (Ed s1, Ed s2) = String.compare (s1, s2);
fun edge_eq e1 e2 = case edge_ord (e1, e2) of EQUAL => true | _ => false;

fun pretty_node (Nd s) = 
    Pretty.block [Pretty.str "Nd(", Pretty.str s, Pretty.str ")"];
val print_node = Pretty.writeln o pretty_node;
fun pretty_edge (Ed s) = 
    Pretty.block [Pretty.str "Ed(", Pretty.str s, Pretty.str ")"];
val print_edge = Pretty.writeln o pretty_edge;
end;
