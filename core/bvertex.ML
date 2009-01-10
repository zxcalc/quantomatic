(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  variable-verticies / boundary-verticies  *)
signature BVERTEX
= sig

type internal_data

datatype data2 = 
         IVert of internal_data (* inner node with basic vertex data *)
       | BVert; (* boundary vertex with boundary data *)

include VERTEX
sharing type data = data2

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Pattern Nodes construction *)
functor BVertexFun(
  structure IVertex : VERTEX 
)
: BVERTEX
= struct


(* open the Names Part of IVertex *)
structure INames : NAMES = IVertex
open INames; 

(* internal data *)
type internal_data = IVertex.data;

datatype data2 = 
         IVert of internal_data (* vertex with data *)
       | BVert; (* a variable vertex *)

type data = data2;

fun data_ord (BVert, BVert) = EQUAL
  | data_ord (BVert, _) = LESS
  | data_ord (_, BVert) = GREATER
  | data_ord (IVert a, IVert b) = IVertex.data_ord (a,b);

fun data_eq (n1, n2) = case data_ord (n1, n2) of EQUAL => true | _ => false;

fun pretty_data (BVert) = 
    Pretty.block [Pretty.str "BVert"]
  | pretty_data (IVert a) = 
    Pretty.block [Pretty.str "IVert(", IVertex.pretty_data a, Pretty.str ")"];

val print_data = Pretty.writeln o pretty_data;

end;
