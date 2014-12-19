(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  Title:      isaplib/isap_src/nametab.ML (sym link in src/generic/)
    Author:     Lucas Dixon, University of Edinburgh
                lucas.dixon@ed.ac.uk
                Initially written Oct 2005, Updated 27 Dec 2008        *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  DESCRIPTION:
    Table with names as keys.
    for holding things associated with nice fresh namers.

    Each entry has a name - but we allow some names not to have
    entries: you can use up names without providing entries.

    Note: Currently
*)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature NAME_TAB_SHARING =
sig
  type name
  type nset
  type renaming
  type 'a T (* name table *)
  type 'a table (* a basic lookup table, not a name table *)
end;

signature NAME_TAB =
sig
  include BASIC_NAME_TAB
  type renaming
  val rename : renaming -> 'a T -> renaming * 'a T
  val mk_renaming : 'a T -> renaming -> renaming
  val do_renaming : renaming -> 'a T -> 'a T
  structure Sharing : NAME_TAB_SHARING
  sharing type Sharing.name = name
  sharing type Sharing.table = Tab.table
  sharing type Sharing.nset = NSet.T
  sharing type Sharing.renaming = renaming
end;

(* see comments below: we end up with a second copy of the same code
   as in compound_renaming: very crappy, better if we could share
   ground function with free type *)
functor NameTabFun(Nm : BASIC_NAME)
: NAME_TAB
= struct
  structure Rnm = Nm.Rnm;
  type renaming = Rnm.T;
  open Nm.NTab;

  fun do_renaming rn x =
      Nm.NSet.fold
        (fn n => fn x2 =>
            case Rnm.lookup rn n of
              NONE => x2
            | SOME n2 => (case try_rename1 n n2 x2
                           of NONE => raise Rnm.invalid_renaming_exp "do_renaming"
                            | SOME x3 => x3 ))
        (get_nameset x) x;

  fun mk_renaming x rn =
      Nm.NSet.fold (snd oo Rnm.rename1) (get_nameset x) rn;

(* note: takes time of nset * ln(nset),
   could be inverted to take time of avoids(rn) * ln(nset) *)

  fun rename rn x =
      Nm.NSet.fold
        (fn n => fn (rn2,x2) =>
            case Rnm.rename1 n rn2 of
              (NONE, rn3) => (rn3,x2)
            | (SOME n2, rn3) =>
              (case try_rename1 n n2 x2
                of NONE => raise Rnm.invalid_renaming_exp "rename"
                 | SOME x3 => (rn3, x3)))
        (get_nameset x) (rn,x);

  structure Sharing = struct
    type name = name;
    type 'a table = 'a Tab.table;
    type 'a T = 'a T;
    type renaming = renaming;
    type nset = Nm.NSet.T;
  end;
end;




(* Cannot do the nice and obvious because of ML's broken functor and
signature mechanism *)
(*
signature NAME_TAB =
sig
  include BASIC_NAME_TAB
  include COMPOUND_RENAMING where type 'a obj = 'a T
end;

functor NameTabFun(Rnm : RENAMING) =
struct
  open Nm.NTab;

  structure CR = CompoundRenamingFun(
                 struct
                 structure Nm = Nm;
                 type obj = 'a Nm.NTab.T
                 val get_nameset = Nm.NTab.get_nameset
                 val try_rename1 = Nm.NTab.try_rename1
                 end)

  open CR;
end;
*)

