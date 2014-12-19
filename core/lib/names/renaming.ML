(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      renaming.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                30 May 2008   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:
 *
 * See BASIC_RENAMING documentation.
 *
 * This adds a compound renaming that updates a renaming to account for the
 * precomposition of another renaming.
 *)

(* Renamings which can themselves be renamed *)
signature RENAMING 
= sig
include BASIC_RENAMING;

structure Rnm : COMPOUND_RENAMING
sharing type Rnm.renaming = T
sharing type Rnm.obj = T

(* first renaming is added after second renaming; extra renamings are added *)
(* val compose_renaming : T -> T -> T *)
end;


(* *)
functor RenamingFun(Nm : BASIC_NAME)
: RENAMING
= struct
  open Nm.Rnm;
  
  structure Rnm = CompoundRenamingFun(
    struct
      structure Nm = Nm
      type obj = T
      val get_nameset = get_avoids
      val try_rename1 = try_rename1_renaming 
    end)

end;
