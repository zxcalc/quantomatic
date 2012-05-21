(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  A structure that defines vertices with strings as data and 
    prefix-substring matching. *)
structure RTechnKey : NAME where type name = string = StrName;

structure DB_RTechn_VertexData
= struct

  datatype data =  RTechnNode of RTechnKey.name
                 | GoalNode of  StrName.name  (* goal/wire type *)
                              * StrIntName.NSet.T; (* goal names of this type *)
                  (* was GoalNode.T  *)

  (* ordering is used for building tables, sorting, etc *)
  fun data_ord (GoalNode _, RTechnNode _) = LESS
    | data_ord (RTechnNode _, GoalNode _) = GREATER
    | data_ord (GoalNode tss1, GoalNode tss2) = 
      prod_ord StrName.name_ord StrIntName.NSet.ord (tss1,tss2)
    | data_ord (RTechnNode s1, RTechnNode s2) = StrName.name_ord (s1,s2);

  (* equality check (should agree with order):
     data_eq(x,y) <=> data_ord (x,y) = EQUAL *)
  fun data_eq (xy as (x, y)) = 
      (case data_ord xy of EQUAL => true
          | _ => false);
 
  (* pretty printing code *)
  fun pretty_data (RTechnNode n) = 
      Pretty.block [ Pretty.str "RTechnNode(", RTechnKey.pretty_name n, 
                     Pretty.str ")"]
    | pretty_data (GoalNode (t,nset)) =  
      Pretty.block [ Pretty.str "GoalNode(", 
                     StrName.pretty_name t, Pretty.str ", ",
                     StrIntName.NSet.pretty nset, Pretty.str ")"];
  val print_data = Pretty.writeln o pretty_data;
  
  (* pretty printing code *)
  fun pretty_node (RTechnNode n) = RTechnKey.pretty_name n
    | pretty_node (GoalNode (t,nset)) = StrName.pretty_name t;
  val print_node = Pretty.writeln o pretty_data;

  
  (* no variables in our vertex data, so we have dummy unit subst *)
  type subst = unit;
  val empty_subst = ();
  fun compose_subst (u1,u2) = ();
  
  (* matching is string prefix "abc" matches "abcd" and "abc.d" but not 
     "ab.cd" or "aabc"; the number index is ignored. *)
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
  type data = DB_RTechn_VertexData.data;
  exception unknown_type_exp of string;
  exception unexpected_data_exp;
  fun type_of (DB_RTechn_VertexData.RTechnNode _) = "RT"
    | type_of (DB_RTechn_VertexData.GoalNode _) = "Gs";
  
  (* TODO: do someting more beautiful; this just uses the pretty function to set the string; 
     also: printing and parsing should be symmetric. *)
  fun cdata_of d = 
    ComponentData.String (Pretty.string_of (DB_RTechn_VertexData.pretty_node d));
  
  (* New nodes are by default RTechnNodes *)
  fun default_data "RT" = DB_RTechn_VertexData.RTechnNode "?"
    | default_data "Gs" = DB_RTechn_VertexData.GoalNode 
        ("?", StrIntName.NSet.empty)
    | default_data s = raise unknown_type_exp s;

  (* TODO: currently edits to the string are simply ignored. *)
  fun update f x = x; 

  fun unwrapUpdRT upd d =
    let
      fun unwrapRTechnNode (ComponentData.String s) = DB_RTechn_VertexData.RTechnNode s
        | unwrapRTechnNode _ = raise unexpected_data_exp
    in
      unwrapRTechnNode (upd (cdata_of d))
    end;

  fun unwrapUpdGs upd d =
    let
      fun unwrapGs (ComponentData.String s) = 
          DB_RTechn_VertexData.GoalNode 
            (s, StrIntName.NSet.empty)
        | unwrapGs _ = raise unexpected_data_exp
    in
      unwrapGs (upd (cdata_of d))
    end;

  fun update upd (d as DB_RTechn_VertexData.RTechnNode _) = unwrapUpdRT upd d
    | update upd (d as DB_RTechn_VertexData.GoalNode _) = unwrapUpdGs upd d;  
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
  fun default_data "String" = "undefined"
    | default_data n = raise unknown_type_exp n;
  fun update f x = x;
end;
