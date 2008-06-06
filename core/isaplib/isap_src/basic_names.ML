(*  Title:      basic_names.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                30 May 2008   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

Basic things you want from names...

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

(* names with an ordering, sets of names, and tables from names to objects *)
signature BASIC_NAMES
= sig
  type name
  structure NSet : FNAMER sharing type NSet.name = name;
  structure NTab : FNAME_TAB sharing NTab.N = NSet;
  val default_name : name
  val pretty_name : name -> Pretty.T
  val print_name : name -> unit
  val name_eq : name * name -> bool
  val name_ord : name * name -> General.order
end;


(* strings as names *)
structure BasicStrName : BASIC_NAMES where type name = string
= struct
  type name = string
  structure NTab = StrNameTab;
  structure NSet = NTab.N;
  val default_name = "Va";
  val pretty_name = Pretty.str;
  val print_name = Pretty.writeln o pretty_name;
  val name_ord = String.compare;
  fun name_eq (ab as (a,b)) = case (name_ord ab) of EQUAL => true | _ => false;
end;
