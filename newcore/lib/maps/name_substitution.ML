(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      isaplib/maps/name_relation.ML
    Author:     Aleks Kissinger
                aleks0@gmail.com
                Initially written 31 Mar 2013                          *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:
    An injective endofunction with fast inverse-lookup and constant-time
    map inversion. These are used for renaming.
    
    Note there is a great deal of code replication with name injections.
    This seems to be difficult to get around without two-level functors
    since Tab != Tab. As substition is BY FAR the most used case,
    it seems wasteful to obfuscate the code to share more with injections.
    
    NOTE: 'set (x,y)' will overwrite a value if x is already in the
    domain, and raise name_clash_exp if mapping x to y would break
    injectivity.
*)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

signature NAME_SUBSTITUTION =
sig
  include NAME_RELATION
  sharing type dom = cod
  sharing DomSet = CodSet

  (*
   * All updaters maintain two invariants:
   *  1. the underlying map is injective
   *  2. cod_set (intersect) cod_avoids = empty
   * 
   * If an updater would break this invariants, name_clash_exp is thrown. This happens in
   * three cases:
   *  1. (SOME a, SOME b, c) : client wants to add a mapping [b |-> c], but there already exists
   *                           a mapping [a |-> c].
   *  2. (SOME a, NONE, c)   : client wants to add c to "avoids" set, but there already exists a
   *                           mapping [a |-> c].
   *  3. (NONE, SOME b)      : client wants to add a mapping [b |-> c], but c is already in the
   *                           "avoids" set
   *)
  exception name_clash_exp of dom option * dom option * cod
  
  (* CAREFUL: inverse_of o inverse_of != I, because avoids info gets lost. This is not a problem
   * in practice, because typically inverses are created local to a renaming function then thrown
   * away. *)
  val inverse_of : T -> T

  (* TODO: maybe these should be in RELATION signature, and implemented everywhere *)
  val restrict_dom : DomSet.T -> T -> T
  val filter_dom : (dom -> bool) -> T -> T
  (*val is_monotone : T -> bool*)

  (*
   * Typically substitutions are created by setting an "avoids" set, then extending the domain of
   * the substitution with "extend_fixed" and "extend_fresh"
   *)
  
  (* try to extend the domain to include every element of this set, where unmapped elements
   * go to themselves. This can fail with name_clash_exp. *)
  val extend_fixed : DomSet.T -> T -> T

  (* extend the domain to the given set, where unmapped elements are mapped to new fresh names.
   * This will never fail. *)
  val extend_fresh : DomSet.T -> T -> T

  val get_cod_avoids : T -> CodSet.T
  val avoid_in_cod : cod -> T -> T
  val avoid_set_in_cod : CodSet.T -> T -> T
  val clear_avoids : T -> T

  val mk_from_avoids : CodSet.T -> T
  
  val add_fresh : dom -> T -> (cod * T)
  val add_fresh_anon : dom -> T -> T
end

functor NameSubstitution(
  structure Name : NAME
) : NAME_SUBSTITUTION =
struct

  type dom = Name.name
  type cod = Name.name
  structure DomSet = Name.NSet
  structure CodSet = Name.NSet
  
  structure Tab = Table(type key = dom val ord = Name.name_ord)

  exception name_clash_exp of dom option * dom option * cod
  
  (* do gets directly on a table, throw exception if not found *)
  fun get_from_tab tab k =
    case Tab.lookup tab k of SOME v => v
       | NONE => raise Map.no_such_elem_exp ()
  
  (* the internal representation *)
  type rep = {
    tab : cod Tab.table,
    itab : dom Tab.table,
    dom_set : DomSet.T,
    cod_set : CodSet.T,
    cod_avoids : CodSet.T
  }
  
  type T = (dom, cod, rep) Map.amap
  
  (* record accessors *)
  local
    fun update_rep_tab f (r : rep) = {tab=f(#tab r),itab= #itab r,dom_set= #dom_set r,cod_set= #cod_set r,cod_avoids= #cod_avoids r}
    fun update_rep_itab f (r : rep) = {tab= #tab r,itab=f(#itab r),dom_set= #dom_set r,cod_set= #cod_set r,cod_avoids= #cod_avoids r}
    fun update_rep_dom_set f (r : rep) = {tab= #tab r,itab= #itab r,dom_set=f(#dom_set r),cod_set= #cod_set r,cod_avoids= #cod_avoids r}
    fun update_rep_cod_set f (r : rep) = {tab= #tab r,itab= #itab r,dom_set= #dom_set r,cod_set=f(#cod_set r),cod_avoids= #cod_avoids r}
    fun update_rep_cod_avoids f (r : rep) = {tab= #tab r,itab= #itab r,dom_set= #dom_set r,cod_set= #cod_set r,cod_avoids=f(#cod_avoids r)}
  in
    fun update_tab        f (mp:T) = Map.update_rep (update_rep_tab        f) mp
    fun update_itab       f (mp:T) = Map.update_rep (update_rep_itab       f) mp
    fun update_dom_set    f (mp:T) = Map.update_rep (update_rep_dom_set    f) mp
    fun update_cod_set    f (mp:T) = Map.update_rep (update_rep_cod_set    f) mp
    fun update_cod_avoids f (mp:T) = Map.update_rep (update_rep_cod_avoids f) mp
    
    fun get_tab        (mp:T) = #tab        (Map.get_rep mp)
    fun get_itab       (mp:T) = #itab       (Map.get_rep mp)
    fun get_dom_set    (mp:T) = #dom_set    (Map.get_rep mp)
    fun get_cod_set    (mp:T) = #cod_set    (Map.get_rep mp)
    fun get_cod_avoids (mp:T) = #cod_avoids (Map.get_rep mp)
    
    fun set_tab        x = update_tab        (K x)
    fun set_itab       x = update_itab       (K x)
    fun set_dom_set    x = update_dom_set    (K x)
    fun set_cod_set    x = update_cod_set    (K x)
    fun set_cod_avoids x = update_cod_avoids (K x)
  end
  
  fun graph_of_rep (r:rep) = let
    fun di x = case Tab.lookup (#tab r) x of SOME y => [y] | NONE => []
    fun dom () = DomSet.list_of (#dom_set r)
  in Map.mk_graph (dom, di)
  end
  
  val is_empty = DomSet.is_empty o get_dom_set
  val dom_size = DomSet.cardinality o get_dom_set
  val cod_size = CodSet.cardinality o get_cod_set (* should always = dom_size *)
  val dom_contains = DomSet.contains o get_dom_set
  val cod_contains = CodSet.contains o get_cod_set
  
  val get = get_from_tab o get_tab
  val get_opt = Tab.lookup o get_tab
  fun img mp x = case get_opt mp x of SOME y => CodSet.single y | NONE => CodSet.empty
  fun img_of_set mp xs = DomSet.fold (CodSet.union_merge o (img mp)) xs CodSet.empty
  val inv_get = get_from_tab o get_itab
  val inv_get_opt = Tab.lookup o get_itab
  fun inv_img mp x = case inv_get_opt mp x of SOME y => DomSet.single y | NONE => DomSet.empty
  fun inv_img_of_set mp xs = CodSet.fold (DomSet.union_merge o (inv_img mp)) xs DomSet.empty
  
  fun is_mapped mp (x,y) =
    case get_opt mp x of SOME y' => Name.name_eq(y,y') | NONE => false
  
  (* raises an exception if setting value would make map no longer injective or break
   * avoids constraint *)
  fun set (x,y) mp = let
    val oldy_opt = get_opt mp x
  in
    if (CodSet.contains (get_cod_set mp) y)
    then
      let
        val x' = inv_get mp y
      in if Name.name_eq (x,x') then mp
         else raise name_clash_exp (SOME x', SOME x, y)
      end
    else
      if (CodSet.contains (get_cod_avoids mp) y)
      then raise name_clash_exp (NONE, SOME x, y)
      else mp |> update_dom_set (DomSet.add x)
              |> update_cod_set (case oldy_opt of SOME oldy => CodSet.delete oldy | NONE => I)
              |> update_cod_set (CodSet.add y)
              |> update_tab (Tab.update (x,y))
              |> update_itab (case oldy_opt of SOME oldy => Tab.delete oldy | NONE => I)
              |> update_itab (Tab.update (y,x))
  end
  
  fun add (x,y) mp = 
    if (DomSet.contains (get_dom_set mp) x) then raise Map.duplicate_exp ()
    else set (x,y) mp
  
  local
    fun delete_pair (x,y) mp =
      mp |> update_dom_set (DomSet.delete x)
         |> update_tab (Tab.delete x)
         |> update_cod_set (CodSet.delete y)
         |> update_itab (Tab.delete y)
  in
    fun delete x mp = case get_opt mp x of NONE => mp | SOME y => delete_pair (x,y) mp
    fun inv_delete y mp = case inv_get_opt mp y of NONE => mp | SOME x => delete_pair (x,y) mp
  end
  
  fun unmap (x,y) mp = case get_opt mp x 
                         of SOME y1 => if Name.name_eq (y,y1) then delete x mp else mp
                          | NONE => mp
  
  (*fun is_monotone mp =
    let
      fun mono dset cset =
        case (DomSet.get_min dset, CodSet.get_min cset)
          of (SOME d, SOME c) => Name.name_eq (get mp d,c) andalso
                                 mono (DomSet.delete d dset) (CodSet.delete c cset)
           | (NONE,NONE) => true
           | _ => false
    in mono (get_dom_set mp) (get_cod_set mp)
    end*)



  (***** avoids and freshness *****)
  
  fun avoid_in_cod n mp =
    if CodSet.contains (get_cod_set mp) n
    then raise name_clash_exp (SOME (inv_get mp n), NONE, n)
    else update_cod_avoids (CodSet.add n) mp


  fun avoid_set_in_cod nset = CodSet.fold avoid_in_cod nset
  fun clear_avoids mp = mp |> set_cod_avoids CodSet.empty

  fun add_fresh old_n mp = let
    val avoid = CodSet.union_merge (get_cod_avoids mp) (get_cod_set mp)
    val new_n = CodSet.new avoid old_n
  in (new_n, mp |> add (old_n, new_n))
  end
  
  val add_fresh_anon = snd oo add_fresh
  
  fun extend_fresh nset mp =
    DomSet.fold (fn n => if dom_contains mp n then I else add_fresh_anon n) nset mp

  
  val empty_rep : rep = { tab = Tab.empty, itab = Tab.empty,
                          dom_set = DomSet.empty, cod_set = CodSet.empty,
                          cod_avoids = CodSet.empty }
  val empty = Map.MAP (empty_rep, graph_of_rep)
  
  fun mk_from_avoids nset = empty |> set_cod_avoids nset

  fun of_graph gr = Map.fold_graph add gr empty
  
  val fold = Map.fold
  val forall = Map.forall
  val exists = Map.exists
  val find = Map.find
  
  fun coerce abs_map = fold add abs_map empty
  handle _ => raise Map.bad_coercion_exp ()
  
  fun compose ms = coerce (Map.compose ms)
  fun compose3 ms = coerce (Map.compose3 ms)
  
  val pretty = Map.pretty "NameSubstitution" Name.pretty_name Name.pretty_name
  
  fun restrict_dom set mp =
    DomSet.fold (fn x => add (x, get mp x)) set empty
  fun filter_dom f mp = 
    DomSet.fold (fn x => if f x then add (x, get mp x) else I) (get_dom_set mp) empty


  (*type irep = {
    tab : dom Tab.table,
    itab : cod Tab.table,
    dom_set : CodSet.T,
    cod_set : DomSet.T
  }
  
  type iT = (cod,dom,irep) Map.amap*)
  
  (*
   Constant-time inversion.
   *)
  fun inverse_of mp =
    mp |> set_cod_set (get_dom_set mp)
       |> set_dom_set (get_cod_set mp)
       |> set_tab (get_itab mp)
       |> set_itab (get_tab mp)
  
  (* extend domain to include new_dom, mapping items to themselves if not mapped *)
  fun extend_fixed new_dom mp = DomSet.fold
    (fn x => if (DomSet.contains (get_dom_set mp) x) then I else add (x, x)) new_dom mp
  
  structure Sharing =
  struct
    type dom = dom
    type cod = cod
    type rep = rep
    structure DomSet = DomSet.Sharing
    structure CodSet = CodSet.Sharing
  end
end
