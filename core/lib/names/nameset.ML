(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  Title:      nameset.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                Initially written Oct 2005, Last Updated 29 Jan 2008   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  DESCRIPTION:

    Used to get new nice fresh names (quickly). With a slight
    adaptation, it can also be thought of as a memory management where
    names are memory spaces.

    A descrete interval table (for fresh names): a specialised, AVL
    tree inspired, representation for holding intervals and getting
    quick lookup/add/delete, worst case O(Log(n)^2); but when adjacent
    names are used it is much faster (often constant time).

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature NAME_SET_SHARING =
sig
  type name
  type T
  type renaming
end;

signature NAME_SET =
sig
  include BASIC_NAME_SET
  include COMPOUND_RENAMING where type obj = T
  structure Sharing : NAME_SET_SHARING
  sharing type Sharing.name = name
  sharing type Sharing.T = T
  sharing type Sharing.renaming = renaming
end;

functor NameSetFun(Nm : BASIC_NAME) =
struct
  open Nm.NSet;

  structure CR = CompoundRenamingFun(
                 struct
                 structure Nm = Nm;
                 type obj = Nm.NSet.T
                 val get_nameset = I
                 val try_rename1 = Nm.NSet.try_rename1
                 end)

  open CR;

  structure Sharing = struct
    type name = name
    type T = T
    type renaming = renaming
  end;
end;


