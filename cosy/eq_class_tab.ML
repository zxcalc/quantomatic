signature GRAPH_ENTRY =
sig
  structure Graph : BANG_GRAPH
  type T
  type data
  val update_graph : (Graph.T -> Graph.T) -> T -> T
  val update_dirty : (bool -> bool) -> T -> T
  val update_data  : (data -> data) -> T -> T
  val get_graph    : T -> Graph.T
  val get_dirty    : T -> bool
  val get_data     : T -> data
  val set_graph    : Graph.T -> T -> T
  val set_dirty    : bool -> T -> T
  val set_data     : data -> T -> T
  val mk : Graph.T * data -> T
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

functor EqClass(
  structure Graph : BANG_GRAPH
  type data
) : EQ_CLASS =
struct
  structure GraphEntry =
  struct
    structure Graph = Graph
    type data = data
    datatype T = GE of { graph: Graph.T, data: data, dirty: bool }
    fun update_graph f (GE r) = GE {graph= f(#graph r),dirty= #dirty r,data= #data r}
    fun update_dirty f (GE r) = GE {graph= #graph r,dirty= f(#dirty r),data= #data r}
    fun update_data f (GE r)  = GE {graph= #graph r,dirty= #dirty r,data= f(#data r)}
    fun get_graph (GE r) = #graph r
    fun get_dirty (GE r) = #dirty r
    fun get_data  (GE r) = #data r
    val set_graph = update_graph o K
    val set_dirty = update_dirty o K
    val set_data  = update_data  o K
    fun mk (gr, d) = GE { graph = gr, data = d, dirty = true }
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

signature EQ_CLASS_TAB =
sig
  type T
  structure GraphEntry : GRAPH_ENTRY
  val fold_redexes : (GraphEntry.T -> 'a -> 'a) -> T -> 'a -> 'a
  val fold_irredexes : (GraphEntry.T -> 'a -> 'a) -> T -> 'a -> 'a
  val exists_redexes : (GraphEntry.T -> bool) -> T -> bool
  val exists_irredexes : (GraphEntry.T -> bool) -> T -> bool
  val get_irredexes : T -> GraphEntry.T list
  val get_redexes : T -> GraphEntry.T list
  val update_one : GraphEntry.T -> T -> T
  val update : GraphEntry.T list -> T -> T
  val empty : T
end


functor EqClassTab(
  structure Theory : GRAPHICAL_THEORY
  structure Metric : GRAPH_METRIC
  val eq : Theory.Graph.T * Theory.Graph.T -> bool
  sharing Theory.Graph.SharingOGraph = Metric.Graph.SharingOGraph
  type data
) : EQ_CLASS_TAB =
struct
  structure G = Theory.Graph
  structure EqClass = EqClass(
    structure Graph = G
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
      
    fun get_redexes tab = fold_redexes cons tab []
    fun get_irredexes tab = fold_irredexes cons tab []
    val exists_redexes = fold_to_exists fold_redexes
    val exists_irredexes = fold_to_exists fold_irredexes
  end
  
  
  
  fun has_match tgt pat =
    is_some (Seq.pull (Theory.MatchSearch.match
      (get_graph pat) (get_graph tgt)
    ))
  
  fun tryadd_to_class cls ge =
    if eq (get_graph (EqClass.get_rep cls), get_graph ge)
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
    if exists_redexes (has_match ge) tab then tab
    else let
      val gr = get_graph ge
      val arity = (V.NSet.cardinality (G.get_inputs gr),
                   V.NSet.cardinality (G.get_inputs gr))
    in tab |> ClassIndex.map_default (arity, []) (find_and_add ge)
    end
  
  fun update ge_list tab = fold update_one ge_list tab
  
  val empty = ClassIndex.empty
end
