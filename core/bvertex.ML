(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  variable-verticies / boundary-verticies  *)
signature BVERTEX
= sig

structure IData : ORD_DATA (* internal data *)

datatype data2 = 
         IVert of IData.data (* inner node with basic vertex data *)
       | BVert; (* boundary vertex with boundary data *)

include VERTEX
sharing type data = data2

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Pattern Nodes construction *)
functor BVertexFun(
  structure IData : ORD_DATA
  structure INames : SSTR_NAMES
)
: BVERTEX
= struct

structure IData : ORD_DATA = IData;

(* open the Names Part of IVertex *)
structure INames : SSTR_NAMES = INames;
open INames; 

(* internal data *)
datatype data2 = 
         IVert of IData.data (* vertex with data *)
       | BVert; (* a variable vertex *)

type data = data2;

fun data_ord (BVert, BVert) = EQUAL
  | data_ord (BVert, _) = LESS
  | data_ord (_, BVert) = GREATER
  | data_ord (IVert a, IVert b) = IData.data_ord (a,b);

fun data_eq (n1, n2) = case data_ord (n1, n2) of EQUAL => true | _ => false;

fun pretty_data (BVert) = 
    Pretty.block [Pretty.str "BVert"]
  | pretty_data (IVert a) = 
    Pretty.block [Pretty.str "IVert(", IData.pretty_data a, Pretty.str ")"];

val print_data = Pretty.writeln o pretty_data;

val default_data = BVert;

end;
