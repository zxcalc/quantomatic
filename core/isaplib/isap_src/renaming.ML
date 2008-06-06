(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      renaming.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                30 May 2008   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

Generic renaming with a set of ignored names, a set of names to be
avoided, and a table of renamings. 

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature RENAMING 
= sig

structure N : BASIC_NAMES

type T

val init : N.NSet.T (* avoid these *)
           -> T

val rename1 : N.name (* old name *)
             -> T (* renaming so far *)
             -> (N.name option) * T (* new name and extended renaming *)

val get_ignored : T -> N.NSet.T
val get_avoids : T -> N.NSet.T
val get_nmap : T -> N.name N.NTab.T

end;


functor RenamingFun(N : BASIC_NAMES) 
: RENAMING
= struct

structure N = N;

(* Invarients: 
   ignore <= avoids - (dom(rn) + range(rn)); 
   dom(rn) <= avoids; 
   range(rn) <= avoids 
*)
datatype T = Renaming of 
         {ignore : N.NSet.T, (* These names that are not renamed *)
          avoids : N.NSet.T, (* Nothing is allowed to be renamed to these *)
          nmap : N.name N.NTab.T}; (* mapping from old to new names *)

(* make a renaming *)
fun init avoids =
    Renaming {ignore = N.NSet.empty,
              avoids = avoids,
              nmap = N.NTab.empty};

(* get info/status of renaming *)
fun get_avoids (Renaming rep) = (#avoids rep);
fun get_nmap (Renaming rep) = (#nmap rep);
fun get_ignored (Renaming rep) = (#ignore rep);

(* rename an old name *)
fun rename1 oldn (rn as Renaming rep) = 
    let val ignore = (#ignore rep) in
      if N.NSet.contains ignore oldn then (NONE, rn) (* ignore it *)
      else 
        let val nmap = (#nmap rep) in 
          (case N.NTab.lookup nmap oldn of
             NONE => 
             let val avoids = (#avoids rep) in
               if N.NSet.contains avoids oldn then (* rename it! *)
                 let val (n2,avoids2) = (N.NSet.add_new oldn avoids)
                 in (SOME n2, 
                     Renaming {ignore = ignore, avoids = avoids2,
                               nmap = N.NTab.ins (oldn,n2) nmap}) 
                 end
               else (NONE, 
                     Renaming {ignore = N.NSet.ins_fresh oldn ignore, 
                               avoids = N.NSet.ins_fresh oldn avoids, 
                               nmap = nmap})
             end
           | SOME n2 => (SOME n2, rn))
        end
    end;

end;
