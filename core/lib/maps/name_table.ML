(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      isaplib/maps/name_table.ML
    Author:     Aleks Kissinger, Lucas Dixon
                aleks0@gmail.com, lucas.dixon@ed.ac.uk
                Initially written Oct 2005, Updated 27 Mar 2013        *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:
    Table with names as keys and polymorphic value types. Note we don't
    even require values to have a notion of equality, so things like
    inverse image don't make sense.
    
    The underlying type is an ABS_MAP, so it supports coercion and
    composition with other map types.
*)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

signature NAME_TABLE =
sig
  type name
  structure NSet : NAME_SET sharing type NSet.name = name
  
  type 'a rep
  type 'a T = (name, 'a, 'a rep) Map.amap
  val empty: 'a T
  
  val get_dom_set : 'a T -> NSet.T
  
  val dom_size : 'a T -> int (* cardinality(dom_set) *)
  val size : 'a T -> int (* alias for dom_size *)
  
  val dom_contains : 'a T -> name -> bool

  (* given the comparison function for values, return true if tables are identical *)
  val tab_eq : ('a * 'a -> bool) -> 'a T * 'a T -> bool
  
  val is_empty : 'a T -> bool
  
  (* forward lookup *)
  val get : 'a T -> name -> 'a
  val get_opt : 'a T -> name -> 'a option
  
  (* updaters *)
  val set : (name * 'a) -> 'a T -> 'a T
  val add : (name * 'a) -> 'a T -> 'a T
  val delete : name -> 'a T -> 'a T
  
  (* table-specific updaters *)
  val store : 'a -> 'a T -> (name * 'a T)
  val store_anon : 'a -> 'a T -> 'a T
  val map_entry : ('a -> 'a) -> name -> 'a T -> 'a T
  val map_default : ('a -> 'a) -> 'a -> name -> 'a T -> 'a T
  val map_all : ('a -> 'b) -> 'a T -> 'b T

  val of_list : (name * 'a) list -> 'a T
  val list_of : 'a T -> (name * 'a) list
  
  (* inherited from Map.Util *)
  val fold : (name * 'a -> 'b -> 'b) -> 'a T -> 'b -> 'b
  val exists : (name * 'a -> bool) -> 'a T -> bool
  val forall : (name * 'a -> bool) -> 'a T -> bool
  val find : (name * 'a -> bool) -> 'a T -> (name * 'a) option
  
  (* composition with other abstract maps *)
  val compose  : ('a,'b,'y) Map.amap * (name,'a,'x) Map.amap -> 'b T
  val compose3 : ('b,'c,'z) Map.amap * ('a,'b,'y) Map.amap * (name,'a,'x) Map.amap -> 'c T
  
  (* try to convert an amap to a name table. Throws an exception if unsuccessful. *)
  val coerce : (name,'a,'b) Map.amap -> 'a T
  
  val pretty_key : name -> Pretty.T
  val pretty : ('a -> Pretty.T) -> 'a T -> Pretty.T
end

functor NameTable(
  structure Dom : NAME
) : NAME_TABLE =
struct

  structure NSet = Dom.NSet
  type name = Dom.name
  fun ord (a, b) = Dom.name_ord (a,b)
  fun name_eq (a, b) = (ord (a,b) = EQUAL)

  structure Tab = Table(type key = name val ord = ord);
  
  (* do gets directly on a table, throw exception if not found *)
  fun get_from_tab tab k =
    case Tab.lookup tab k of SOME v => v
       | NONE => raise Map.no_such_elem_exp ()

  datatype 'a rep = Rep of {names : NSet.T, tab : 'a Tab.table}
  
  local
    fun update_rep_names f (Rep r) = Rep {names=f(#names r),tab= #tab r}
    fun update_rep_tab f (Rep r) = Rep {names= #names r,tab=f(#tab r)}
    fun get_rep_names (Rep r) = #names r
    fun get_rep_tab   (Rep r) = #tab r
  in
  
    fun update_names f = Map.update_rep (update_rep_names f)
    fun update_tab f   = Map.update_rep (update_rep_tab f)
    fun set_names x    = update_names (K x)
    fun set_tab x      = update_tab (K x)
    fun get_names m    = get_rep_names (Map.get_rep m)
    fun get_tab m      = get_rep_tab (Map.get_rep m)
         
    fun graph_of_rep rep = let
      fun di x = case Tab.lookup (get_rep_tab rep) x of SOME y => [y] | NONE => []
      fun dom () = NSet.list_of (get_rep_names rep)
    in Map.mk_graph (dom, di)
    end
  
  end
  

  type 'a T = (name, 'a, 'a rep) Map.amap
  
  val get_dom_set = get_names
  
  fun dom_size mp = NSet.cardinality (get_names mp)
  val size = dom_size
  fun dom_contains mp = NSet.contains (get_dom_set mp)
  
  fun is_empty mp = NSet.is_empty (get_names mp)
  
  fun get_opt mp = Tab.lookup (get_tab mp)
  fun get mp = get_from_tab (get_tab mp)

  fun set (k,v) mp =
    mp |> update_names (NSet.add k)
       |> update_tab (Tab.update (k,v))
  
  fun add (k,v) m =
    if (NSet.contains (get_names m) k) then raise Map.duplicate_exp ()
    else set (k,v) m

  fun map_entry f k m = update_tab (Tab.map_entry k f) m
                        handle Tab.SAME => m
  fun map_default f x k = update_tab (Tab.map_default (k,x) f)
  fun map_all f (Map.MAP (Rep {names=names,tab=tab}, _)) =
    Map.MAP (Rep { names = names, tab = Tab.map (K f) tab }, graph_of_rep)
  
  fun store v mp = let
    val (fresh,names) = NSet.add_new Dom.default_name (get_names mp)
  in (fresh, mp |> set_names names |> update_tab (Tab.update (fresh,v)))
  end
  
  fun store_anon v mp = snd (store v mp)
  
  fun delete n mp =
    mp |> update_names (NSet.delete n)
       |> update_tab (Tab.delete_safe n)
  
  val empty_rep = Rep { names=NSet.empty, tab=Tab.empty }
  val empty = Map.MAP (empty_rep, graph_of_rep)
  
  val fold = Map.fold
  val forall = Map.forall
  val exists = Map.exists
  val find = Map.find

  fun of_graph gr = Map.fold_graph add gr empty
  fun of_list lst = Basics.fold add lst empty
  fun list_of m = fold cons m []


  fun tab_eq val_eq (tab1,tab2) =
    if dom_size tab1 = dom_size tab2
    then fold (fn (k,v) => fn sofar => (sofar andalso
      case get_opt tab2 k
        of SOME v' => val_eq (v,v')
         | NONE => false)) tab1 true
    else false
  
  fun coerce abs_map = fold add abs_map empty
  handle _ => raise Map.bad_coercion_exp ()
  
  fun compose ms = coerce (Map.compose ms)
  fun compose3 ms = coerce (Map.compose3 ms)
  
  val pretty_key = Dom.pretty_name
  fun pretty pretty_val = Map.pretty "NameTable" Dom.pretty_name pretty_val
  
end

