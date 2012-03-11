(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  A structure that defines vertices with strings as data and 
    prefix-substring matching. *)
structure DB_RTechn_VertexData
= struct

  datatype data = GoalNode of string  (* goal type *) 
                              * SStrName.NSet.T (* goal names of this type *)
                | RTechnNode of string;  (* technique name *)
  
  (* ordering is used for building tables, sorting, etc *)
  fun data_ord (GoalNode _, RTechnNode _) = LESS
    | data_ord (RTechnNode _, GoalNode _) = GREATER
    | data_ord (GoalNode tss1, GoalNode tss2) = 
      prod_ord String.compare SStrName.NSet.ord (tss1,tss2)
    | data_ord (RTechnNode s1, RTechnNode s2) = String.compare (s1,s2);

  (* equality check (should agree with order):
     data_eq(x,y) <=> data_ord (x,y) = EQUAL *)
  fun data_eq (xy as (x, y)) = 
      (case data_ord xy of EQUAL => true
          | _ => false);
 
  (* pretty printing code *)
  fun pretty_data (RTechnNode s) = 
      Pretty.block [ Pretty.str "RTechnNode(", Pretty.str s, Pretty.str ")"]
    | pretty_data (GoalNode (t,ss)) =  
      Pretty.block [ Pretty.str "GoalNode(", Pretty.str t, Pretty.str ", ",
                     SStrName.NSet.pretty ss, Pretty.str ")"];
  val print_data = Pretty.writeln o pretty_data;
  
  (* no variables in our vertex data, so we have dummy unit subst *)
  type subst = unit;
  val empty_subst = ();
  fun compose_subst (u1,u2) = ();
  
  (* matching is string prefix "abc" matches "abcd" and "abc.d" but not 
     "ab.cd" or "aabc" *)
  fun match_data (ss as (GoalNode _, RTechnNode _)) subst = NONE 
    | match_data (ss as (RTechnNode _, GoalNode _)) subst = NONE 
    | match_data (ss as (RTechnNode s1, RTechnNode s2)) subst =  
      if String.isPrefix s1 s2 then SOME subst else NONE
    | match_data (ss as (GoalNode (s1,_), GoalNode (s2,_))) subst =  
      if String.isPrefix s1 s2 then SOME subst else NONE; 
  (* unification: if x matches y, and y matches x (with the same subst) *) 
  (* for string prefix, x and y must be the same string *)
  fun unify_data xy subst = if data_eq xy then SOME subst else NONE;
  (* there is not data in a vertex, subst is just the id on the data *)
  fun subst_in_data subst x = x;
  
  (* pretty printing for subst *)
  fun pretty_subst () = Pretty.str "vertex subst: ()";
  val print_subst = Pretty.writeln o pretty_subst;  

  val default_data = RTechnNode "";
  
  structure Sharing = struct type data = data; type subst = subst; end;  
end;

structure RTechn_VertexData : UNIFIABLE_DATA = DB_RTechn_VertexData;

(*  *)
structure RTechn_VertexComponentData_Param
: ELEMENT_COMPONENT_DATA_PARAM where type data = RTechn_VertexData.data
= struct 
  type data = RTechn_VertexData.data;
  exception unknown_type_exp of string;
  exception unexpected_data_exp;
  fun type_of s = "String";
  fun cdata_of d = 
    (* TODO: do someting more beautiful; this just uses the pretty function to set the string *)
    ComponentData.String (Pretty.string_of (RTechn_VertexData.pretty_data d));
  
  (* New nodes are by default RTechnNodes *)
  fun default_data s = DB_RTechn_VertexData.RTechnNode s; 

  (* TODO: currently edts to the string are simply ignored. *)
  fun update f x = x; 
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  A structure that defines vertices with strings as data and 
    prefix-substring matching. *)
structure StringData
: UNIFIABLE_DATA
= struct

  type data = string;
  
  (* ordering is used for building tables, sorting, etc *)
  val data_ord = String.compare;
  (* equality check (should agree with order):
     data_eq(x,y) <=> data_ord (x,y) = EQUAL *)
  fun data_eq (xy as (x, y)) = 
      (case data_ord xy of EQUAL => true
          | _ => false);
 
  (* pretty printing code *)
  val pretty_data = Pretty.str;
  val print_data = Pretty.writeln o pretty_data;
  
  (* no variables in our vertex data, so we have dummy unit subst *)
  type subst = unit;
  val empty_subst = ();
  fun compose_subst (u1,u2) = ();
  
  (* matching is string prefix "abc" matches "abcd" and "abc.d" but not 
     "ab.cd" or "aabc" *)
  fun match_data (ss as (s1, s2)) subst = 
      if data_eq (s1, s2) then SOME subst else NONE; 
  (* unification: if x matches y, and y matches x (with the same subst) *) 
  (* for string prefix, x and y must be the same string *)
  fun unify_data xy subst = if data_eq xy then SOME subst else NONE;
  (* there is not data in a vertex, subst is just the id on the data *)
  fun subst_in_data subst x = x;
  
  (* pretty printing for subst *)
  fun pretty_subst () = Pretty.str "string subst: ()";
  val print_subst = Pretty.writeln o pretty_subst;  

  val default_data = "";
  
  structure Sharing = struct type data = data; type subst = subst; end;  
end;


(*  *)
structure Str_ComponentData_Param
: ELEMENT_COMPONENT_DATA_PARAM where type data = StringData.data
= struct 
  type data = StringData.data;
  exception unknown_type_exp of string;
  exception unexpected_data_exp;
  fun type_of s = "String";
  fun cdata_of s = ComponentData.String s;
  fun default_data s = s;

  fun unwrapUpd upd a =
    let
      fun unwrapString (ComponentData.String e) = e
        | unwrapString _ = raise unexpected_data_exp
    in
      unwrapString (upd (ComponentData.String a))
    end;
  fun update f x = unwrapUpd f x;
end;


