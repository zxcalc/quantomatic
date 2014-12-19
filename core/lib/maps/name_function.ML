signature NAME_FUNCTION =
sig
  include NAME_RELATION
end

functor NameFunction(
  structure Dom : NAME
  structure Cod : NAME
) : NAME_FUNCTION =
struct

  type dom = Dom.name
  type cod = Cod.name
  structure DomSet = Dom.NSet
  structure CodSet = Cod.NSet
  
  structure Tab = Table(type key = dom val ord = Dom.name_ord)
  structure ITab = Table(type key = cod val ord = Cod.name_ord)
  
  (* do gets directly on a table, throw exception if not found *)
  fun get_from_tab tab k =
    case Tab.lookup tab k of SOME v => v
       | NONE => raise Map.no_such_elem_exp ()
  fun get_from_itab tab k =
    case ITab.lookup tab k of SOME v => v
       | NONE => raise Map.no_such_elem_exp ()
  
  (* the internal representation *)
  type rep = {
    tab : cod Tab.table,
    itab : DomSet.T ITab.table,
    dom_set : DomSet.T,
    cod_set : CodSet.T
  }
  
  type T = (dom, cod, rep) Map.amap
  
  (* record accessors *)
  local
    fun update_rep_tab     f (r:rep) = {tab=f(#tab r),itab= #itab r,dom_set= #dom_set r,cod_set= #cod_set r}
    fun update_rep_itab    f (r:rep) = {tab= #tab r,itab=f(#itab r),dom_set= #dom_set r,cod_set= #cod_set r}
    fun update_rep_dom_set f (r:rep) = {tab= #tab r,itab= #itab r,dom_set=f(#dom_set r),cod_set= #cod_set r}
    fun update_rep_cod_set f (r:rep) = {tab= #tab r,itab= #itab r,dom_set= #dom_set r,cod_set=f(#cod_set r)}
  in
    fun update_tab     f (mp:T) = Map.update_rep (update_rep_tab     f) mp
    fun update_itab    f (mp:T) = Map.update_rep (update_rep_itab    f) mp
    fun update_dom_set f (mp:T) = Map.update_rep (update_rep_dom_set f) mp
    fun update_cod_set f (mp:T) = Map.update_rep (update_rep_cod_set f) mp
    
    fun get_tab     (mp:T) = #tab     (Map.get_rep mp)
    fun get_itab    (mp:T) = #itab    (Map.get_rep mp)
    fun get_dom_set (mp:T) = #dom_set (Map.get_rep mp)
    fun get_cod_set (mp:T) = #cod_set (Map.get_rep mp)
    
    fun set_tab     x = update_tab     (K x)
    fun set_itab    x = update_itab    (K x)
    fun set_dom_set x = update_dom_set (K x)
    fun set_cod_set x = update_cod_set (K x)
  end
  
  fun graph_of_rep (r:rep) = let
    fun di x = case Tab.lookup (#tab r) x of SOME y => [y] | NONE => []
    fun dom () = DomSet.list_of (#dom_set r)
  in Map.mk_graph (dom, di)
  end
  
  val is_empty = DomSet.is_empty o get_dom_set
  val dom_size = DomSet.cardinality o get_dom_set
  val cod_size = CodSet.cardinality o get_cod_set
  val dom_contains = DomSet.contains o get_dom_set
  val cod_contains = CodSet.contains o get_cod_set
  
  val get = get_from_tab o get_tab
  val get_opt = Tab.lookup o get_tab
  fun img mp x = case get_opt mp x of SOME y => CodSet.single y | NONE => CodSet.empty
  fun img_of_set mp xs = DomSet.fold (CodSet.union_merge o (img mp)) xs CodSet.empty
  
  fun inv_get_opt mp x =
    case ITab.lookup (get_itab mp) x
      of SOME ys => (case DomSet.tryget_singleton ys of SOME y => SOME y
                        | NONE => raise Map.multiple_values_exp ())
       | NONE => NONE
    
  fun inv_get mp x = case inv_get_opt mp x of SOME y => y
                        | NONE => raise Map.no_such_elem_exp ()
  
  fun inv_img mp y = case ITab.lookup (get_itab mp) y of SOME xs => xs | NONE => DomSet.empty
  fun inv_img_of_set mp ys = CodSet.fold (DomSet.union_merge o (inv_img mp)) ys DomSet.empty
  
  fun is_mapped mp (x,y) =
    case get_opt mp x of SOME y' => Cod.name_eq(y,y') | NONE => false
  
  
  fun set (x,y) mp = let
    (* if x had previously occurred in an inverse image, clear it *)
    val mp' =
      case get_opt mp x
        of SOME oldy => let
             val inv = DomSet.delete x (inv_img mp oldy)
           in if DomSet.is_empty inv
              then mp |> update_cod_set (CodSet.delete oldy)
                      |> update_itab (ITab.delete oldy)
              else mp |> update_itab (ITab.update (oldy,inv))
           end
         | NONE => mp
  in
    mp' |> update_dom_set (DomSet.add x)
        |> update_cod_set (CodSet.add y)
        |> update_tab (Tab.update (x,y))
        |> update_itab (ITab.update (y,DomSet.add x (inv_img mp' y)))
  end
  
  fun add (x,y) mp = 
    if (DomSet.contains (get_dom_set mp) x) then raise Map.duplicate_exp ()
    else set (x,y) mp
  
  local
    fun delete_pair (x,y) mp = let
      val inv = DomSet.delete x (inv_img mp y)
      val mp' = if DomSet.is_empty inv
                then mp |> update_cod_set (CodSet.delete y)
                        |> update_itab (ITab.delete y)
                else mp |> update_itab (ITab.update (y,inv))
    in
      mp' |> update_dom_set (DomSet.delete x)
          |> update_tab (Tab.delete x)
    end
  in
    fun delete x mp = case get_opt mp x of NONE => mp | SOME y => delete_pair (x,y) mp
    fun inv_delete y mp = case inv_get_opt mp y of NONE => mp | SOME x => delete_pair (x,y) mp
  end
  
  fun unmap (x,y) mp = case get_opt mp x 
                         of SOME y1 => if Cod.name_eq (y,y1) then delete x mp else mp
                          | NONE => mp
  
  val empty_rep : rep = { tab = Tab.empty, itab = ITab.empty,
                          dom_set = DomSet.empty, cod_set = CodSet.empty }
  val empty = Map.MAP (empty_rep, graph_of_rep)
  
  fun of_graph gr = Map.fold_graph add gr empty
  
  val fold = Map.fold
  val forall = Map.forall
  val exists = Map.exists
  val find = Map.find
  
  fun coerce abs_map = fold add abs_map empty
  handle _ => raise Map.bad_coercion_exp ()
  
  fun compose ms = coerce (Map.compose ms)
  fun compose3 ms = coerce (Map.compose3 ms)
  
  val pretty = Map.pretty "NameFunction" Dom.pretty_name Cod.pretty_name
  
  structure Sharing =
  struct
    type dom = dom
    type cod = cod
    type rep = rep
    structure DomSet = DomSet.Sharing
    structure CodSet = CodSet.Sharing
  end
end

