(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  variable-verticies / boundary-verticies  *)

signature OVERTEX_SHARING
= sig
include NAME_AND_UNIFIABLE_DATA_SHARING
type idata
end;

signature OVERTEX
= sig

(* internal data *)
structure IData : UNIFIABLE_DATA 

datatype data2 = 
         IVert of IData.data (* inner node with basic vertex data *)
       | EVert; (* boundary vertex with boundary data *)

include VERTEX
sharing type data = data2

val is_internal_data : data -> bool

structure SharingWithIData : OVERTEX_SHARING where type idata = IData.data
sharing SharingWithData = SharingWithIData;

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Pattern Nodes construction *)
functor OVertexFun(
  structure IData : UNIFIABLE_DATA
  structure VertexNames : SSTR_NAMES
)
: OVERTEX
= struct

structure IData : UNIFIABLE_DATA = IData;

(* open the Names Part of IVertex *)
structure VertexNames : SSTR_NAMES = VertexNames;
open VertexNames; 

structure Map = NameMapFun(structure Dom = VertexNames and Cod = VertexNames);
structure InjEndo = NameInjEndoFun(VertexNames);


(* internal data *)
datatype data2 = 
         IVert of IData.data (* vertex with data *)
       | EVert; (* a variable vertex *)

fun is_internal_data (IVert _) = true
  | is_internal_data _ = false;

type data = data2;

fun data_ord (EVert, EVert) = EQUAL
  | data_ord (EVert, _) = LESS
  | data_ord (_, EVert) = GREATER
  | data_ord (IVert a, IVert b) = IData.data_ord (a,b);

fun data_eq (n1, n2) = case data_ord (n1, n2) of EQUAL => true | _ => false;

fun subst_in_data u EVert = EVert
  | subst_in_data u (IVert a) = IVert (IData.subst_in_data u a);

fun pretty_data (EVert) = 
    Pretty.block [Pretty.str "EVert"]
  | pretty_data (IVert a) = 
    Pretty.block [Pretty.str "IVert(", IData.pretty_data a, Pretty.str ")"];

val print_data = Pretty.writeln o pretty_data;

val default_data = EVert;

type unif = IData.unif;

val empty_unif = IData.empty_unif;

val compose_unif = IData.compose_unif;

fun unify_data ((IVert d1),(IVert d2)) u = IData.unify_data (d1,d2) u
  | unify_data (EVert,EVert) u = SOME u
  | unify_data _ _ = NONE;

val pretty_unif = IData.pretty_unif;

val print_unif = IData.print_unif;

structure SharingWithData = struct 
  open Sharing; type data = data; type unif = unif;
  structure Map = Map.Sharing;
  structure InjEndo = InjEndo.Sharing;
end;
structure SharingWithIData = struct 
  open SharingWithData; type idata = IData.data; type unif = unif;
end;

end;
