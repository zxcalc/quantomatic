(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  variable-verticies / boundary-verticies  *)
signature BVERTEX
= sig

structure BasicVert : VERTEX

type bdry_data

datatype bdata = 
         IVert of BasicVert.data (* inner node with basic vertex data *)
       | BVert of bdry_data; (* boundary vertex with boundary data *)

include NAME_AND_DATA 
sharing BasicVert.NSet = NSet
sharing BasicVert.NTab = NTab
sharing type name = BasicVert.name
sharing type data = bdata

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Pattern Nodes construction *)
functor BVertexFun(
  structure BasicVert : VERTEX 
  type bdry_data
  val pretty_bdry_data : bdry_data -> Pretty.T
  val bdry_ord : bdry_data * bdry_data -> General.order
)
: BVERTEX
= struct

structure BasicVert = BasicVert;
type bdry_data = bdry_data;

open BasicVert;

datatype bdata = 
         IVert of BasicVert.data (* vertex with data *)
       | BVert of bdry_data; (* a variable vertex *)

type data = bdata;

fun data_ord (BVert a, BVert b) = bdry_ord (a,b)
  | data_ord (BVert _, _) = LESS
  | data_ord (_, BVert _) = GREATER
  | data_ord (IVert a, IVert b) = BasicVert.data_ord (a,b);

fun data_eq (n1, n2) = case data_ord (n1, n2) of EQUAL => true | _ => false;

fun pretty_data (BVert b) = 
    Pretty.block 
      [Pretty.str "BVert(", pretty_bdry_data b, Pretty.str ")"]
  | pretty_data (IVert a) = 
    Pretty.block 
      [Pretty.str "IVert(", BasicVert.pretty_data a, Pretty.str ")"];

val print_data = Pretty.writeln o pretty_data;

end;

