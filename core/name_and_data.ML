signature ORD_NAMES
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

signature ORD_DATA
= sig
  type data
  val pretty_data : data -> Pretty.T
  val print_data : data -> unit
  val data_ord : data * data -> General.order
  val data_eq : data * data -> bool
end;

signature NAME_AND_DATA
= sig
include ORD_NAMES
include ORD_DATA
end;


structure StrData : ORD_DATA 
= struct 
type data = string
val pretty_data = Pretty.str;
val print_data = Pretty.writeln o pretty_data;
val data_ord = String.compare;
fun data_eq (a,b) = (a = (b : string));
end;


structure UnitData : ORD_DATA 
= struct 
type data = unit
val pretty_data = K (Pretty.str "");
val print_data = Pretty.writeln o pretty_data;
val data_ord = K EQUAL;
val data_eq = K true;
end;


structure StrVertex : NAME_AND_DATA
= struct
type name = NName.T
structure NSet = NName.NSet;
structure NTab = NName.NTab;
val default_name = "v_a";
val pretty_name = NName.pretty;
val print_name = NName.print;
val name_eq = uncurry NName.eq;
val name_ord = NName.ord;
open StrData;
end;

structure UnitEdge : NAME_AND_DATA
= struct
type name = EName.T
structure NSet = EName.NSet;
structure NTab = EName.NTab;
val default_name = "e_a";
val pretty_name = EName.pretty;
val print_name = EName.print;
val name_eq = uncurry EName.eq;
val name_ord = EName.ord;
open UnitData;
end;

structure StrEdge : NAME_AND_DATA
= struct
type name = EName.T
type data = string
structure NSet = EName.NSet;
structure NTab = EName.NTab;
val default_name = "v_a";
val pretty_name = EName.pretty;
val print_name = EName.print;
val name_eq = uncurry EName.eq;
val name_ord = EName.ord;
open StrData;
end;
