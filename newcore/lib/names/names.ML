(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  Title:      names.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                30 May 2008                                            *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  DESCRIPTION:

Names + renaming for them

Avoids very common error: see "n" is not in avoid set, so you don't
change it. But! you also don't want another renaming, call it "m"
*not* to result in renaming "m" to "n". So you need both an avoid set
as well as a ignore set. "n" gets added to the ignore set as well as
the avoid set, thus "m" doesn't clobber "n" and all future occurances
of "n" are ignored.

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

(* Types within a NAME structure, to ease writing of sharing constraints *)
signature NAME_SHARING =
sig
  type name
  type 'a table
  type nset
  type 'a ntab
  type renaming
end;

(* names + renaming *)
signature NAME
= sig
  type name

  structure Namer : NAMER
  sharing type Namer.name = name;

  structure NTab : NAME_TAB;
  sharing type NTab.name = name;

  structure NSet : NAME_SET;
  sharing NTab.NSet = NSet;

  structure Tab : TABLE;
  sharing type NTab.Tab.table = Tab.table;
  sharing type NTab.Tab.key = Tab.key;

  structure Rnm : RENAMING
  where type 'a nmap = 'a NTab.T
  and type name = name
  and type nset = NSet.T

  sharing type NTab.renaming = Rnm.T
  sharing type NSet.renaming = Rnm.T;

  val default_name : name
  val pretty_name : name -> Pretty.T
  val string_of_name : name -> string
  val print_name : name -> unit
  val name_eq : name * name -> bool
  val name_ord : name * name -> General.order

  type renaming = Rnm.T
  (* sharing type Rnm.T = renaming *)

  val empty_renaming : renaming (* avoid these names *)
  val mk_renaming : NSet.T  (* ignore these *)
                    -> NSet.T (* avoid these names *)
                    -> name NTab.T (* Use this renaming map *)
                    -> renaming
  val renaming_of_avoidset
      : NSet.T (* avoid these names *)
        -> renaming

  (* for renaming *)
  val rename1 : name (* old name *)
                -> renaming (* renaming so far *)
                -> (name option) (* new name *)
                   * renaming (* extended renaming *)

  (* for alpha conversion *)
  val try_compose_rename1 : name (* old name *)
                    -> name (* new name *)
                    -> renaming (* renaming so far *)
                    -> renaming option (* (optional) extended renaming *)

  (* add more stuff to renaming *)
  val union_avoids_to_renaming : NSet.T -> renaming -> renaming
  (* ignores are auto also added to avoids *)
  val union_ignores_to_renaming : NSet.T -> renaming -> renaming

  (* gets from renamings *)
  val newnames_of_renaming : renaming -> NSet.T
  val ignored_of_renaming : renaming -> NSet.T
  val avoids_of_renaming : renaming -> NSet.T
  val nmap_of_renaming : renaming -> name NTab.T

  val pretty_renaming : renaming -> Pretty.T
  val print_renaming : renaming -> unit


  (** types which want to be shared **)
  structure Sharing : NAME_SHARING
  where type name = name
    and type 'a table = 'a Tab.table
    and type nset = NSet.T
    and type 'a ntab = 'a NTab.T
    and type renaming = Rnm.T

  (* where type nmap = nmap *) (* note compound type; cannot use sharing *)

(*   sharing type Rnm.name = name
  sharing type Rnm.nset = NSet.T
*)

end;

(* Safe String Names; isomorphic strings, named hidden under a
constructor so the underlying string type is not normally revealed,
thus harder to confuse name types. *)

signature SNAME
= sig
include NAME
type src
val mk : src -> name
val dest : name -> src
end;

functor NameFun(
  structure Namer : NAMER
  val default_name : Namer.name
) : NAME
= struct
  (* create basic names *)
  structure Nm : BASIC_NAME =
    BasicNameFun(
      structure Namer = Namer
      val default_name = default_name);

  open Nm;

  structure Rnm = RenamingFun(Nm);
  structure NSet = NameSetFun(Nm);
  structure NTab = NameTabFun(Nm);

  type renaming = Rnm.T
  val empty_renaming = Rnm.empty;
  val mk_renaming = Rnm.mk;
  val renaming_of_avoidset = Rnm.mk_from_avoids;

  val rename1 = Rnm.rename1;
  val try_compose_rename1 = Rnm.try_compose_rename1;

  val union_ignores_to_renaming = Rnm.union_ignores;
  val union_avoids_to_renaming = Rnm.union_avoids;

  val newnames_of_renaming = Rnm.get_newnames;
  val ignored_of_renaming = Rnm.get_ignored;
  val avoids_of_renaming = Rnm.get_avoids;
  val nmap_of_renaming = Rnm.get_nmap;

  val pretty_renaming = Rnm.pretty;
  val print_renaming = Rnm.print;

  (** types which want to be shared **)
  structure Sharing = struct
    type name = name
    type 'a table = 'a Tab.table
    type nset = NSet.T
    type 'a ntab = 'a NTab.T
    type renaming = Rnm.T
  end


(* FIXME: respect depth properly; don't print out all things in set, just depth number of them. *)
  (* PolyML pretty print names *)
  val _ = PolyML.addPrettyPrinter (fn depth => fn printArgTypes => fn value =>
            ml_pretty (Pretty.to_ML (Nm.pretty_name value)));

  (* pretty print name sets *)
  val _ = PolyML.addPrettyPrinter (fn depth => fn printArgTypes => fn value =>
            ml_pretty (Pretty.to_ML (NSet.pretty value)));

  (* pretty name tables *)
  val _ = PolyML.addPrettyPrinter (fn depth => fn printArgTypes => fn value =>
          let fun pretty_f a = (Pretty.from_ML o pretty_ml o printArgTypes)
                                  (a,depth - 1)
          in
            value |> NTab.list_of
                  |> map (fn (n,a) =>
                     Pretty.block [Nm.pretty_name n,
                                   Pretty.str "=>", pretty_f a])
                  |> Pretty.list "{" "}"
                  |> Pretty.to_ML |> ml_pretty
          end);

end;

(* add data to a names type - makes new names type with extra data *)
functor AddDataNameFun(
  structure Nm : NAME
  type data
  val ord_data : data * data -> order
  val pretty : data * Nm.name -> Pretty.T
  val default_name : data * Nm.name
)
: NAME
= struct
  structure Namer2 =
    AddDataNamerFun(
      structure N = Nm.Namer;
      type data = data
      val ord_data = ord_data
      val pretty = pretty);
  structure Names = NameFun(
    structure Namer = Namer2
    val default_name = default_name);
  open Names;
end;
