(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Graph unification morphisms. *) 
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 

signature GRAPH_UMORPH
= sig
  include NAME_AMORPH
  structure Entity : NAME_AND_UNIFIABLE_DATA
  structure Graph : BASIC_GRAPH
  sharing Dom.Sharing = Entity.Sharing 

  val get_unif : T -> Entity.unif
  val get_amorph : T -> Entity.Amorph.T
  
  (* for alpha-equivalance/isomorphism checking *)
  val try_change2 : Graph.T -> Graph.T (* old and new graphs *)
                    -> Dom.name (* old name *)
                    -> Cod.name (* new name *)
                    -> T (* renaming so far *)
                    -> T option (* new name and extended renaming *)
  (* as above, but raises exception *)
  val add2 : Graph.T -> Graph.T -> Dom.name -> Cod.name -> T -> T
  
end;

functor GraphUmorphFun( structure Graph : BASIC_GRAPH
  and Entity : NAME_AND_UNIFIABLE_DATA
  val retrieve_data : Graph.T -> Entity.name -> Entity.data ) : GRAPH_UMORPH
= struct
  (* create name amorph *)
  structure Am : NAME_AMORPH =
    NameAmorphWithDataFun(
      structure Nm = Entity
      type data = Entity.unif
      val empty_data = Entity.unif_empty);
  
  open Am;
  
  structure Entity = Entity;
  structure Graph = Graph;
  
  val get_unif = get_data;
  fun get_amorph y = let val x = destr y in 
                     Entity.Amorph.constr {ignore = #ignore x,
                                           avoids = #avoids x,
                                           domf = #domf x,
                                           codf = #codf x,
                                           data = Entity.Amorph.empty_data} end;
  
  fun try_change2 g h n m cn = case try_change1 n m cn of
      NONE    => NONE
    | SOME cn'=> case Entity.data_unify 
                                        (retrieve_data g n,retrieve_data h m)
                                        (get_unif cn') of
                   NONE    => NONE
                 | SOME un => let val rep = destr cn' in
                                     SOME (constr {ignore = #ignore rep,
                                           avoids = #avoids rep,
                                           domf = #domf rep,
                                           codf = #codf rep,
                                           data = un}) end;

  fun add2 g h oldn newn cn =
    (case try_change2 g h oldn newn cn of 
       NONE => raise add_exp (oldn, newn, cn)
     | SOME cn2 => cn2);
                                           
end;