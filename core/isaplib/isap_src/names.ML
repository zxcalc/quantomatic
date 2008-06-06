(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      names.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                30 May 2008   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

Names + renaming for them

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

(* names + renaming *)
signature NAMES
= sig
  include BASIC_NAMES
  type renaming  
  val init_renaming : NSet.T (* avoid these names *)
                      -> renaming
  val rename1 : name (* old name *)
                -> renaming (* renaming so far *)
                -> (name option) (* new name *)
                   * renaming (* extended renaming *)
                   
  val ignored_of_renaming : renaming -> NSet.T
  val avoids_of_renaming : renaming -> NSet.T
  val nmap_of_renaming : renaming -> name NTab.T
end;


(* strings as names *)
structure StrName :> NAMES where type name = string
= struct
  open BasicStrName
  structure Renaming = RenamingFun(BasicStrName);
  type renaming = Renaming.T
  val init_renaming = Renaming.init;
  val rename1 = Renaming.rename1;
  val ignored_of_renaming = Renaming.get_ignored;
  val avoids_of_renaming = Renaming.get_avoids;
  val nmap_of_renaming = Renaming.get_nmap;
end;
