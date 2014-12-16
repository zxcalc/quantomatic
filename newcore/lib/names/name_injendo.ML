(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

Note: Injective Endomorphisms on Names  (1-1) functions with same type of 
      dom and cod. 

(usually used for representing isomorphism from one name kind to itself)

 name_injendo = CHANGE_NAME where
 translate_name = id; Unlike renaming, we don't keep cache of newnames.

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(*  sharing substuff issue: would like to make a subtype of INJ for 
    injective endomorphism that adds the Dom=Cod constraint. 
*)

(* FIXME: I'd like to be able to further restrict existing signature of sharing
   structure in NAME_INJ to being NAME_INJENDO_SHARING, e.g. where structure
   Sharing : NAME_INJENDO_SHARING *)
signature NAME_INJENDO_SHARING = sig 
  include NAME_INJ_SHARING
  sharing Dom = Cod
end;

signature NAME_INJENDO = 
sig 
  include NAME_INJ
  sharing Sharing.Dom = Sharing.Cod

  (* renamings are trivially name injective endo *)
  val of_rnm : Dom.Rnm.T -> T
  val rnm_of : T -> Dom.Rnm.T
  
  (* amorph composition *)
  val compose : T -> T -> T
end;

(* Name endomorphisms *)
functor NameInjEndoFun(Nm : NAME) 
: NAME_INJENDO
= struct 
  structure Nm = Nm;
  structure NameInj = NameInjFun(
    structure Dom = Nm 
    structure Cod = Nm 
    val translate_name = I
    ) : NAME_INJ;
  open NameInj;

  fun of_rnm rn = 
      let val rep = (Dom.Rnm.destr rn)
      in
        constr {ignore = #ignore rep,
            avoids = #avoids rep,
            domf = #nmap rep,
            codf = 
              Dom.NTab.fold (fn (n1,n2) => Cod.NTab.doadd (n2,n1)) 
                (#nmap rep) Cod.NTab.empty } 
      end;

  fun rnm_of a = 
      let val rep = (destr a)
      in
        Dom.Rnm.constr {ignore = #ignore rep,
                    avoids = #avoids rep,
                    new = Dom.NSet.empty,
                    nmap = #domf rep}
      end;

  (* HERE BE DRAGONS: assumptions about ignore and avoid: assumes dom of one matches cod of the other *)
  fun compose a b =
      let val repa = (destr a)
          val repb = (destr b)
      in
        constr {ignore = #ignore repa,
            avoids = #avoids repa,
            domf = Dom.NTab.map_all (K (domf b)) (#domf repa),
            codf = Cod.NTab.map_all (K (codf a)) (#codf repb) } 
      end;
      
end;