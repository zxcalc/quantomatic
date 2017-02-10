(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  DESCRIPTION:
    Basic things you want from names... name tables, name sets,
    and simple tables, default names, ordering and pretty printing
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

(* names with an ordering, sets of names, and tables from names to objects *)

(* NOTE: this signature is unused, its just for building NAME which
   is used, see names.ML *)

signature BASIC_NAME
= sig
  type name

  structure Namer : NAMER
  sharing type Namer.name = name

  structure NTab : BASIC_NAME_TAB
  sharing type NTab.name = name;

  structure NSet : BASIC_NAME_SET
  sharing NTab.NSet = NSet;

  structure Tab : TABLE
  sharing type NTab.Tab.table = Tab.table;
  sharing type NTab.Tab.key = Tab.key;

(*   type nmap = name NTab.T *)
  structure Rnm : BASIC_RENAMING
  where type 'a nmap = 'a NTab.T
  and type name = name
  and type nset = NSet.T

  val default_name : name
  val pretty_name : name -> Pretty.T
  val string_of_name : name -> string
  val print_name : name -> unit
  val name_eq : name * name -> bool
  val name_ord : name * name -> General.order
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
functor BasicNameFun(structure Namer : NAMER
                     val default_name : Namer.name)
: BASIC_NAME
= struct
type name = Namer.name;
val pretty_name = Namer.pretty;
val string_of_name = Pretty.string_of o Namer.pretty;
val print_name = Pretty.writeln o pretty_name;
val name_ord = Namer.ord;
fun name_eq ab = case (name_ord ab) of EQUAL => true | _ => false;
val default_name = default_name;
structure Namer = Namer;
structure NSet = BasicNameSetFun(Namer);
structure NTab = BasicNameTabFun(structure NSet = NSet and Namer = Namer);
structure Tab = NTab.Tab;
structure Rnm = BasicRenamingFun(structure NTab = NTab and Namer = Namer);
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(* basic names: strings, integers, and pairs of strings and ints     *)
(*
structure BasicStrName : BASIC_NAME where type name = string
= BasicNameFun(structure Namer = StrNamer val default_name = "a");

structure BasicIntName : BASIC_NAME where type name = int
= BasicNameFun(structure Namer = IntNamer val default_name = 0);

structure BasicStrIntName : BASIC_NAME where type name = string * int
= BasicNameFun(structure Namer = StrIntNamer val default_name = ("a",0));
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  PolyML pretty printing *)
(*
install_pp (make_pp ["BasicStrName", "NSet", "T"] (Pretty.pprint o BasicIntName.NSet.pretty));
install_pp (make_pp ["BasicStrName", "NSet", "T"] (Pretty.pprint o BasicStrName.NSet.pretty));
install_pp (make_pp ["BasicStrIntName", "NSet", "T"] (Pretty.pprint o BasicStrIntName.NSet.pretty));
*)
