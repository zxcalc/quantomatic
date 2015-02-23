(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      isaplib/maps/name_relation.ML
    Author:     Aleks Kissinger
                aleks0@gmail.com
                Initially written 31 Mar 2013                          *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:
    A generic many-to-many relation for names. All other name-to-name
    mappings inherit this signature.
    
    While NAME_TABLE does not directly inherit this signature, it uses
    the same naming conventions where possible.
*)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

signature NAME_RELATION_SHARING =
sig
  type dom
  type cod
  type rep
  structure DomSet : NAME_SET_SHARING sharing type DomSet.name = dom
  structure CodSet : NAME_SET_SHARING sharing type CodSet.name = cod
end

signature NAME_RELATION =
sig
  type dom
  type cod
  type rep (* the internal representation *)
  structure DomSet : NAME_SET
  structure CodSet : NAME_SET
  type T = (dom, cod, rep) Map.amap (* the main type *)
  
  structure Sharing : NAME_RELATION_SHARING
  sharing type Sharing.dom = dom
  sharing type Sharing.cod = cod
  sharing type Sharing.rep = rep
  sharing Sharing.DomSet = DomSet.Sharing
  sharing Sharing.CodSet = CodSet.Sharing
  
  
  val empty: T
  val is_empty : T -> bool
  
  val get_dom_set : T -> DomSet.T (* set of elements in domain *)
  val get_cod_set : T -> CodSet.T (* set of elements in codomain *)
  
  val dom_size : T -> int (* cardinality(dom_set) *)
  val cod_size : T -> int (* cardinality(cod_set) *)
  
  val dom_contains : T -> dom -> bool
  val cod_contains : T -> cod -> bool
  
  (*** FORWARD LOOKUP FUNCTIONS ***)
  
  (* these will both throw multiple_values_exp if multiple values returned *)
  val get : T -> dom -> cod (* throws no_such_elem_exp if not found *)
  val get_opt : T -> dom -> cod option (* returns NONE if not found *)
  
  (* these will always succeed, but may return an empty set *)
  val img : T -> dom -> CodSet.T
  val img_of_set : T -> DomSet.T -> CodSet.T
  
  (*** INVERSE LOOKUP FUNCTIONS ***)
  
  (* these will both throw multiple_values_exp if multiple values returned *)
  val inv_get : T -> cod -> dom (* throws no_such_elem_exp if not found *)
  val inv_get_opt : T -> cod -> dom option (* returns NONE if not found *)
  
  (* these will always succeed, but may return an empty set *)
  val inv_img : T -> cod -> DomSet.T
  val inv_img_of_set : T -> CodSet.T -> DomSet.T
  
  (*** RELATION LOOKUP ***)
  
  val is_mapped : T -> (dom * cod) -> bool
  
  (*** UPDATERS ***)
  
  (* NOTE: for relations, set = add, but add is preferred *)
  
  (* for single-valued functions, overwrite if dom exists *)
  val set : (dom * cod) -> T -> T
  
  (* for single-valued functions, raise duplicate_exp dom exists *) 
  val add : (dom * cod) -> T -> T
  
  (* delete an element of the domain. no-op if not found *)
  val delete : dom -> T -> T
  
  (* delete an element of the codomain. no-op if not found *)
  val inv_delete : cod -> T -> T
  
  (* remove a single pair from the relation. for single-valued functions, use
     of delete or inv_delete is preferred *)
  val unmap : dom * cod -> T -> T
  
  (*** ABSTRACT MAP FUNCTIONS ***)
  
  (* inherited from Map *)
  val fold : (dom * cod -> 'a -> 'a) -> T -> 'a -> 'a
  val exists : (dom * cod -> bool) -> T -> bool
  val forall : (dom * cod -> bool) -> T -> bool
  val find : (dom * cod -> bool) -> T -> (dom * cod) option
  
  (* composition with other abstract maps *)
  val compose  : ('a,cod,'y) Map.amap * (dom,'a,'x) Map.amap -> T
  
  (* shorthand for compose o (I X compose) *)
  val compose3 : ('b,cod,'z) Map.amap * ('a,'b,'y) Map.amap * (dom,'a,'x) Map.amap -> T
  
  (* try to convert an amap to the given type of relation, exception if unsuccessful. *)
  val coerce : (dom,cod,'x) Map.amap -> T
  
  val pretty : T -> Pretty.T

end


functor NameRelation(
  structure Dom : NAME
  structure Cod : NAME
) : NAME_RELATION =
struct
  type dom = Dom.name
  type cod = Cod.name
  structure DomSet = Dom.NSet
  structure CodSet = Cod.NSet
  structure DomTab = NameTable(structure Dom = Dom)
  structure CodTab = NameTable(structure Dom = Cod)
  
  (*structure Tab = Table(type key = dom val ord = Dom.name_ord)
  structure ITab = Table(type key = cod val ord = Cod.name_ord)*)
  
  (* do gets directly on a table, throw exception if not found *)
  (*fun get_from_tab tab k =
    case Tab.lookup tab k of SOME v => v
       | NONE => raise Map.no_such_elem_exp ()
  fun get_from_itab tab k =
    case ITab.lookup tab k of SOME v => v
       | NONE => raise Map.no_such_elem_exp ()*)
  
  (* the internal representation *)
  type rep = {
    tab : CodSet.T DomTab.T,
    itab : DomSet.T CodTab.T
  }
  
  type T = (dom, cod, rep) Map.amap
  
  (* record accessors *)
  local
    fun update_rep_tab     f (r:rep) = {tab=f(#tab r),itab= #itab r}
    fun update_rep_itab    f (r:rep) = {tab= #tab r,itab=f(#itab r)}
  in
    fun update_tab  f (mp:T) = Map.update_rep (update_rep_tab  f) mp
    fun update_itab f (mp:T) = Map.update_rep (update_rep_itab f) mp
    fun get_tab       (mp:T) = #tab     (Map.get_rep mp)
    fun get_itab      (mp:T) = #itab    (Map.get_rep mp)
    fun set_tab  x           = update_tab     (K x)
    fun set_itab x           = update_itab    (K x)
  end

  fun get_dom_set mp = DomTab.get_dom_set (get_tab mp)
  fun get_cod_set mp = CodTab.get_dom_set (get_itab mp)
  
  fun graph_of_rep (r:rep) = let
    fun di x = case DomTab.get_opt (#tab r) x of SOME ys => CodSet.list_of ys | NONE => []
    fun dom () = DomSet.list_of (DomTab.get_dom_set (#tab r))
  in Map.mk_graph (dom, di)
  end
  
  val is_empty = DomTab.is_empty o get_tab
  val dom_size = DomTab.dom_size o get_tab
  val cod_size = CodTab.dom_size o get_itab
  val dom_contains = DomSet.contains o get_dom_set
  val cod_contains = CodSet.contains o get_cod_set
  
  fun get_opt mp x =
    case DomTab.get_opt (get_tab mp) x
      of SOME ys => (case CodSet.tryget_singleton ys of SOME y => SOME y
                        | NONE => raise Map.multiple_values_exp ())
       | NONE => NONE
  
  fun get mp x = case get_opt mp x of SOME y => y
                    | NONE => raise Map.no_such_elem_exp ()
  
  fun img mp x = case DomTab.get_opt (get_tab mp) x of SOME ys => ys | NONE => CodSet.empty
  fun img_of_set mp xs = DomSet.fold (CodSet.union_merge o (img mp)) xs CodSet.empty
  
  fun inv_get_opt mp x =
    case CodTab.get_opt (get_itab mp) x
      of SOME ys => (case DomSet.tryget_singleton ys of SOME y => SOME y
                        | NONE => raise Map.multiple_values_exp ())
       | NONE => NONE
    
  fun inv_get mp y = case inv_get_opt mp y of SOME x => x
                        | NONE => raise Map.no_such_elem_exp ()
  
  fun inv_img mp y = case CodTab.get_opt (get_itab mp) y of SOME xs => xs | NONE => DomSet.empty
  fun inv_img_of_set mp ys = CodSet.fold (DomSet.union_merge o (inv_img mp)) ys DomSet.empty
  
  fun is_mapped mp (x,y) = CodSet.contains (img mp x) y
  
  fun add (x,y) mp =
    mp |> update_tab (DomTab.set (x,CodSet.add y (img mp x)))
       |> update_itab (CodTab.set (y,DomSet.add x (inv_img mp y)))
  
  val set = add
  
  fun unmap (x,y) mp = let
    val ix = img mp x |> CodSet.delete y
    val iy = inv_img mp y |> DomSet.delete x
  in mp |> (if CodSet.is_empty ix
            then update_tab (DomTab.delete x)
            else update_tab (DomTab.set (x,ix)))
        |> (if DomSet.is_empty iy
            then update_itab (CodTab.delete y)
            else update_itab (CodTab.set (y,iy)))
  end
  
  fun delete x mp = CodSet.fold (fn y => unmap (x,y)) (img mp x) mp
  fun inv_delete y mp = DomSet.fold (fn x => unmap (x,y)) (inv_img mp y) mp
  
  val empty_rep : rep = { tab = DomTab.empty, itab = CodTab.empty }
  val empty = Map.MAP (empty_rep, graph_of_rep)
  
  fun of_graph gr = Map.fold_graph add gr empty
  
  val fold = Map.fold
  val forall = Map.forall
  val exists = Map.exists
  val find = Map.find
  
  fun coerce abs_map = fold add abs_map empty
  handle Map.duplicate_exp () => raise Map.bad_coercion_exp ()
  
  fun compose ms = coerce (Map.compose ms)
  fun compose3 ms = coerce (Map.compose3 ms)
  
  val pretty = Map.pretty "NameFunction" DomTab.pretty_key CodTab.pretty_key
  
  structure Sharing =
  struct
    type dom = dom
    type cod = cod
    type rep = rep
    structure DomSet = DomSet.Sharing
    structure CodSet = CodSet.Sharing
  end
end




