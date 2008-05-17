(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Pattern Nodes and Edges *)
signature PAT_VERTEX
= sig

structure BasicVert : NAME_AND_DATA

datatype pat_data = 
         Vert of BasicVert.data (* vertex with data *)
       | VarVert; (* a variable vertex *)

include NAME_AND_DATA 
sharing BasicVert.NSet = NSet
sharing BasicVert.NTab = NTab
sharing type name = BasicVert.name
sharing type data = pat_data

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Pattern Nodes construction *)
functor PatVertexFun(structure BasicVert : NAME_AND_DATA)
: PAT_VERTEX
= struct

structure BasicVert = BasicVert;

open BasicVert;

datatype pat_data = 
         Vert of BasicVert.data (* vertex with data *)
       | VarVert; (* a variable vertex *)

type data = pat_data;

fun data_ord (VarVert, VarVert) = EQUAL
  | data_ord (VarVert, _) = LESS
  | data_ord (_, VarVert) = GREATER
  | data_ord (Vert a, Vert b) = BasicVert.data_ord (a,b);

fun data_eq (n1, n2) = case data_ord (n1, n2) of EQUAL => true | _ => false;

fun pretty_data VarVert = Pretty.str "VarVert"
  | pretty_data (Vert a) = 
    Pretty.block 
      [Pretty.str "Vert(", BasicVert.pretty_data a, Pretty.str ")"];

val print_data = Pretty.writeln o pretty_data;

end;

