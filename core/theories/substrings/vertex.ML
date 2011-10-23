(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Open Vertices with string data: normal open-vertex  structure 
    (OVERTEX) with internal data set to string. *)
signature STR_VERTEX
= sig
  include OVERTEX
  sharing type IData.data = string;
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  A structure that defines vertices with strings as data and 
    prefix-substring matching. *)
structure PrefixStringVertexData
: UNIFIABLE_DATA
= struct

  type data = string;
  
  (* ordering is used for building tables, sorting, etc *)
  fun data_ord (s1,s2) = String.compare;
  (* equality check (should agree with order):
     data_eq(x,y) <=> data_ord (x,y) = EQUAL *)
  fun data_eq (xy as (x, y)) = 
      (case data_ord n1n2 of EQUAL => true
          | _ => false);
 
  (* pretty printing code *)
  fun pretty_data = Pretty.str;
  val print_data = Pretty.writeln o pretty_data;
  
  (* no variables in our vertex data, so we have dummy unit subst *)
  type subst = unit;
  val empty_subst = ();
  fun compose_subst (u1,u2) = ();
  
  (* matching is string prefix "abc" matches "abcd" and "abc.d" but not 
     "ab.cd" or "aabc" *)
  fun match_data (ss as (s1, s2)) subst = 
      if String.isPrefix ss then SOME subst else NONE; 
  (* unification: if x matches y, and y matches x (with the same subst) *) 
  (* for string prefix, x and y must be the same string *)
  fun unify_data xy subst = if data_eq xy then SOME subst else NONE;
  (* there is not data in a vertex, subst is just the id on the data *)
  fun subst_in_data subst x = x;
  
  (* pretty printing for subst *)
  val pretty_subst = Pretty.str "vertex subst: ()";
  val print_subst = Pretty.writeln o pretty_subst;  

  val default_data = "";
  
  structure Sharing = struct type data = data; type subst = subst; end;  
end;
