(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graph unification morphisms. *) 
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

signature GRAPH_UMORPH
= sig
  include NAME_AMORPH
  
  (* Entity: internal names and their data *)
  structure Entity : NAME_AND_UNIFIABLE_DATA
  (* Note: Dom = Cod = name part of Entity (Dom and Cod have no data), 
     typically this is the Edge or Vertex
  *)
  sharing Dom.Sharing = Entity.Sharing 

  (* get unification (vertex & edge) data associated with graph automorphism *)
  val get_unif : T -> Entity.unif

  (* morphism of variables in internal data *)
  val get_amorph : T -> Entity.Amorph.T
  
  (* for alpha-equivalance/isomorphism checking *)
  (* try to add mapping,  *)
  val try_unify : Entity.name * Entity.data
                    -> Entity.name * Entity.data
                    -> T (* renaming so far *)
                    -> T option (* new name and extended renaming *)

  (* as above, but raises exception *)
  val unify : Entity.name * Entity.data
              -> Entity.name * Entity.data 
              -> T -> T

end;

functor GraphUmorphFun(Entity : NAME_AND_UNIFIABLE_DATA) : GRAPH_UMORPH
= struct
  (* create name amorph *)
  structure Am : NAME_AMORPH =
    NameAmorphWithDataFun(
      structure Nm = Entity
      type data = Entity.unif
      val empty_data = Entity.unif_empty);
  
  open Am;
  
  structure Entity = Entity;
  
  val get_unif = get_data;
  fun get_amorph y = let val x = destr y in 
                     Entity.Amorph.constr {ignore = #ignore x,
                                           avoids = #avoids x,
                                           domf = #domf x,
                                           codf = #codf x,
                                           data = Entity.Amorph.empty_data} end;
  
  fun try_unify (n1,d1) (n2,d2) cn = case try_change1 n1 n2 cn of
      NONE    => NONE
    | SOME cn'=> case Entity.data_unify 
                                        (d1,d2)
                                        (get_unif cn') of
                   NONE    => NONE
                 | SOME un => let val rep = destr cn' in
                                     SOME (constr {ignore = #ignore rep,
                                           avoids = #avoids rep,
                                           domf = #domf rep,
                                           codf = #codf rep,
                                           data = un}) end;

  fun unify (nd1 as (n1,d1)) (nd2 as (n2,d2)) cn =
    (case try_unify nd1 nd2 cn of 
       NONE => raise add_exp (n1, n2, cn)
     | SOME cn2 => cn2);
                    
end;