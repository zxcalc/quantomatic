signature GRAPH_ENTRY =
sig
  structure Graph : BANG_GRAPH
  structure Equiv : GRAPH_EQUIV
  sharing Equiv.Graph.SharingOGraph = Graph.SharingOGraph
  
  type T
  type data
  val update_graph : (Equiv.Graph.T -> Equiv.Graph.T) -> T -> T
  val update_data  : (data -> data) -> T -> T
  val update_edata : (Equiv.T -> Equiv.T) -> T -> T
  val update_dirty : (bool -> bool) -> T -> T
  val get_graph    : T -> Equiv.Graph.T
  val get_data     : T -> data
  val get_edata    : T -> Equiv.T
  val get_dirty    : T -> bool
  val set_graph    : Equiv.Graph.T -> T -> T
  val set_data     : data -> T -> T
  val set_edata    : Equiv.T -> T -> T
  val set_dirty    : bool -> T -> T
  val mk  : Graph.T * data -> T
  val mk' : (Graph.T * Equiv.T) * data -> T
end

signature EQ_CLASS =
sig
  type T
  structure GraphEntry : GRAPH_ENTRY
  val update_rep     : (GraphEntry.T -> GraphEntry.T) -> T -> T
  val update_congs   : (GraphEntry.T list -> GraphEntry.T list) -> T -> T
  val update_redexes : (GraphEntry.T list -> GraphEntry.T list) -> T -> T
  val get_rep        : T -> GraphEntry.T
  val get_congs      : T -> GraphEntry.T list
  val get_redexes    : T -> GraphEntry.T list
  val set_rep        : GraphEntry.T -> T -> T
  val set_congs      : GraphEntry.T list -> T -> T
  val set_redexes    : GraphEntry.T list -> T -> T
  val mk : GraphEntry.T -> T
end

signature EQ_CLASS_TAB =
sig
  type T
  structure GraphEntry : GRAPH_ENTRY
  
  val fold_redexes : (GraphEntry.T -> 'a -> 'a) -> T -> 'a -> 'a
  val fold_irredexes : (GraphEntry.T -> 'a -> 'a) -> T -> 'a -> 'a
  
  (* folds over a function which gets args "(graph, rep) is_redex" *)
  val fold_rule_pairs :
    (GraphEntry.Graph.T * GraphEntry.Graph.T -> bool -> 'a -> 'a) ->
    T -> 'a -> 'a
  
  val exists_redexes : (GraphEntry.T -> bool) -> T -> bool
  val exists_irredexes : (GraphEntry.T -> bool) -> T -> bool
  val get_irredexes : T -> GraphEntry.T list
  val get_redexes : T -> GraphEntry.T list
  val update_one : GraphEntry.T -> T -> GraphEntry.T option * T
  val update : GraphEntry.T list -> T -> GraphEntry.T list * T
  
  (* manually add an eqclass. All the elements in the list are
     assumed redexes *)
  val add_eq_class : GraphEntry.T * GraphEntry.T list -> T -> T
  
  val empty : T
end

functor EqClass(
  structure Equiv : GRAPH_EQUIV
  type data
) : EQ_CLASS =
struct

  structure GraphEntry : GRAPH_ENTRY =
  struct
    structure Graph = Equiv.Graph
    structure Equiv = Equiv
    type data = data
    datatype T = GE of { graph: Equiv.Graph.T, data: data, edata: Equiv.T, dirty: bool }
    fun update_graph f (GE r) = GE {graph = f(#graph r), data  = #data r, edata = #edata r, dirty = #dirty r}
    fun update_data f (GE r) = GE {graph = #graph r, data  = f(#data r), edata = #edata r, dirty = #dirty r}
    fun update_edata f (GE r) = GE {graph = #graph r, data  = #data r, edata = f(#edata r), dirty = #dirty r}
    fun update_dirty f (GE r) = GE {graph = #graph r, data  = #data r, edata = #edata r, dirty = f(#dirty r)}
    fun get_graph (GE r) = #graph r
    fun get_data  (GE r) = #data r
    fun get_edata (GE r) = #edata r
    fun get_dirty (GE r) = #dirty r
    val set_graph    = update_graph o K
    val set_data     = update_data  o K
    val set_edata    = update_edata o K
    val set_dirty    = update_dirty o K
    
    fun mk' ((gr, ed), d) =
      GE {
        graph = gr,
        data = d,
        edata = ed,
        dirty = true
      }
    
    fun mk (gr, d) = mk' (Equiv.compute_equiv_data gr, d)
  end

  datatype T = EQCLASS of { rep: GraphEntry.T, congs: GraphEntry.T list, redexes: GraphEntry.T list }
  fun update_rep f (EQCLASS r)     = EQCLASS {rep= f(#rep r),congs= #congs r,redexes= #redexes r}
  fun update_congs f (EQCLASS r)   = EQCLASS {rep= #rep r,congs= f(#congs r),redexes= #redexes r}
  fun update_redexes f (EQCLASS r) = EQCLASS {rep= #rep r,congs= #congs r,redexes= f(#redexes r)}
  fun get_rep     (EQCLASS r) = #rep r
  fun get_congs   (EQCLASS r) = #congs r
  fun get_redexes (EQCLASS r) = #redexes r
  val set_rep     = update_rep     o K
  val set_congs   = update_congs   o K
  val set_redexes = update_redexes o K  
  fun mk ge = EQCLASS { rep = ge, congs = [], redexes = [] }
end


functor EqClassTab(
  structure Theory : GRAPHICAL_THEORY
  structure Metric : GRAPH_METRIC
  structure Equiv : GRAPH_EQUIV
  sharing Theory.Graph.SharingOGraph =
          Metric.Graph.SharingOGraph =
          Equiv.Graph.SharingOGraph
  type data
) : EQ_CLASS_TAB =
struct
  structure G = Theory.Graph
  structure EqClass = EqClass(
    structure Equiv = Equiv
    type data = data)
  structure GraphEntry = EqClass.GraphEntry
  open GraphEntry
  
  structure ClassIndex = Table(
    type key = int * int
    val ord = prod_ord int_ord int_ord
  )
  
  type T = (EqClass.T list) ClassIndex.table
  
  (* convenience functions for pulling out redexes and irredexes *)
  local
    exception found_exn of unit
    fun fold_to_exists fldf f tab = 
      fldf (fn x => fn _ =>
        (if f x then raise found_exn () else false)) tab false
      handle found_exn () => true
  in
    fun fold_redexes f =
      ClassIndex.fold (fn (_, classlist) =>
        fold (fn class =>
          fold f (EqClass.get_redexes class)
        ) classlist
      )
      
    fun fold_irredexes f =
      ClassIndex.fold (fn (_, classlist) =>
        fold (fn class =>
          fold f (EqClass.get_rep class :: EqClass.get_congs class)
        ) classlist
      )
    
    fun fold_rule_pairs f =
      let
        fun class_fld class x =
          let
            val rep = GraphEntry.get_graph (EqClass.get_rep class)
          in
            fold
               (fn redex => f (GraphEntry.get_graph redex, rep) true)
               (EqClass.get_redexes class)
            (fold
               (fn cong => f (GraphEntry.get_graph cong, rep) false)
               (EqClass.get_congs class) x)
          end
      in
        ClassIndex.fold (fn (_, classlist) =>
          fold class_fld classlist)
      end
    
    fun get_redexes tab = fold_redexes cons tab []
    fun get_irredexes tab = fold_irredexes cons tab []
    val exists_redexes = fold_to_exists fold_redexes
    val exists_irredexes = fold_to_exists fold_irredexes
  end
  
  
  
  fun has_match tgt pat =
    is_some (Seq.pull (Theory.MatchSearch.match
      (get_graph pat) (get_graph tgt)
    ))
  
  (* returns (SOME new_class) if graph is equivalent to the representative,
     and NONE otherwise *)
  fun tryadd_to_class cls ge =
    if Equiv.eq (get_edata (EqClass.get_rep cls), get_edata ge)
    then SOME (
      case Metric.ord_graph (get_graph (EqClass.get_rep cls), get_graph ge)
      of LESS    => cls |> EqClass.update_redexes (cons ge)
       | GREATER => cls |> EqClass.update_redexes (fn reds => EqClass.get_rep cls :: EqClass.get_congs cls @ reds)
                        |> EqClass.set_congs []
                        |> EqClass.set_rep ge
       | EQUAL   => cls |> EqClass.update_congs (cons ge))
    else NONE
  
  fun find_and_add ge classlist = let
    fun tryaddf cls (cls_found, rest) =
      if cls_found then (cls_found, cls :: rest)
      else (case tryadd_to_class cls ge
              of SOME cls' => (true, cls' :: rest)
               | NONE      => (false, cls :: rest))
  in case fold tryaddf classlist (false, [])
       of (true, cs) => cs
        | (false, cs) => (EqClass.mk ge :: cs)
  end
  
  fun update_one ge tab =
    if exists_redexes (has_match ge) tab then (NONE, tab)
    else let
      val gr = get_graph ge
      val arity = (V.NSet.cardinality (G.get_inputs gr),
                   V.NSet.cardinality (G.get_inputs gr))
    in (SOME ge , tab |> ClassIndex.map_default (arity, []) (find_and_add ge))
    end
  
  fun update ge_list tab = let
    fun update' ge (ges, tab') = let
       val (newge, tab') = update_one ge tab'
    in case newge
        of SOME ge => (ge::ges, tab')
         | NONE    => (ges, tab') 
    end 
  in fold update' ge_list ([],tab)
  end

  fun add_eq_class (rep, redexes) =
    ClassIndex.map_default ((~1,~1), [])
      (cons (EqClass.mk rep |> EqClass.set_redexes redexes))
  
  val empty = ClassIndex.empty
end
