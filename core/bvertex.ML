(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  variable-verticies / boundary-verticies  *)
signature BVERTEX
= sig

structure IVertex : VERTEX
structure Boundary : ORD_DATA

datatype data2 = 
         IVert of IVertex.data (* inner node with basic vertex data *)
       | BVert of Boundary.data; (* boundary vertex with boundary data *)

include VERTEX 
sharing IVertex.Rnm = Rnm
sharing IVertex.NSet = NSet
sharing IVertex.NTab = NTab
(* sharing type name = IVertex.name implicit from NSet *)
sharing type data = data2

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Pattern Nodes construction *)
functor BVertexFun(
  structure IVertex : VERTEX 
  structure Boundary : ORD_DATA
)
: BVERTEX
= struct

structure IVertex = IVertex;
structure Boundary = Boundary;

(* open the Names Part of IVertex *)
structure INames : NAMES = IVertex
open INames; 

datatype data2 = 
         IVert of IVertex.data (* vertex with data *)
       | BVert of Boundary.data; (* a variable vertex *)

type data = data2;

fun data_ord (BVert a, BVert b) = Boundary.data_ord (a,b)
  | data_ord (BVert _, _) = LESS
  | data_ord (_, BVert _) = GREATER
  | data_ord (IVert a, IVert b) = IVertex.data_ord (a,b);

fun data_eq (n1, n2) = case data_ord (n1, n2) of EQUAL => true | _ => false;

fun pretty_data (BVert b) = 
    Pretty.block 
      [Pretty.str "BVert(", Boundary.pretty_data b, Pretty.str ")"]
  | pretty_data (IVert a) = 
    Pretty.block 
      [Pretty.str "IVert(", IVertex.pretty_data a, Pretty.str ")"];

val print_data = Pretty.writeln o pretty_data;

end;


structure StrBVertex : BVERTEX = 
BVertexFun(structure IVertex = StrVertex and Boundary = StrData);
