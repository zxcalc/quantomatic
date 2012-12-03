signature GRAPH_ENTRY =
sig
  structure Graph : BANG_GRAPH
  structure Equiv : GRAPH_EQUIV
  sharing Equiv.Graph.SharingOGraph = Graph.SharingOGraph
  
  type T
  type data
  val update_graph : (Graph.T -> Graph.T) -> T -> T
  val update_data  : (data -> data) -> T -> T
  val update_edata : (Equiv.T option -> Equiv.T option) -> T -> T
  val update_dirty : (bool -> bool) -> T -> T
  val get_graph    : T -> Graph.T
  val get_data     : T -> data
  val get_edata    : T -> Equiv.T option
  val get_dirty    : T -> bool
  val set_graph    : Graph.T -> T -> T
  val set_data     : data -> T -> T
  val set_edata    : Equiv.T option -> T -> T
  val set_dirty    : bool -> T -> T
  val mk  : Graph.T * data -> T
  val mk' : (Graph.T * Equiv.T option) * data -> T
end

signature EQ_CLASS =
sig
  type T
  val update_rep     : (GraphName.name -> GraphName.name) -> T -> T
  val update_congs   : (GraphName.name list -> GraphName.name list) -> T -> T
  val update_redexes : (GraphName.name list -> GraphName.name list) -> T -> T
  val get_rep        : T -> GraphName.name
  val get_congs      : T -> GraphName.name list
  val get_redexes    : T -> GraphName.name list
  val set_rep        : GraphName.name -> T -> T
  val set_congs      : GraphName.name list -> T -> T
  val set_redexes    : GraphName.name list -> T -> T
  val mk : GraphName.name -> T
end

signature EQ_CLASS_TAB =
sig
  type T
  structure Theory : GRAPHICAL_THEORY
  structure EqClass : EQ_CLASS
  structure GraphEntry : GRAPH_ENTRY
  structure DNet : TOP_DNET
  sharing GraphEntry.Graph.SharingOGraph
    = Theory.Graph.SharingOGraph
    = DNet.G.SharingOGraph
  
  
  val update_initial_rs : (Theory.Ruleset.T -> Theory.Ruleset.T) -> T -> T
  val get_initial_rs    : T -> Theory.Ruleset.T
  val set_initial_rs    : Theory.Ruleset.T -> T -> T

  val get_ruleset : T -> Theory.Ruleset.T
  
  val get_graph_entry : T -> GraphName.name -> GraphEntry.T
  val get_graph_tab : T -> GraphEntry.T GraphName.NTab.T
  
  val fold_eqclasses : (EqClass.T -> 'a -> 'a) -> T -> 'a -> 'a
  val fold_redexes : (GraphEntry.T -> 'a -> 'a) -> T -> 'a -> 'a
  val fold_irredexes : (GraphEntry.T -> 'a -> 'a) -> T -> 'a -> 'a
  
  (* folds over a function which gets args "(graph, rep) is_redex" *)
  (*val fold_rule_pairs :
    (GraphEntry.Graph.T * GraphEntry.Graph.T -> bool -> 'a -> 'a) ->
    T -> 'a -> 'a*)
  
  val exists_redexes : (GraphEntry.T -> bool) -> T -> bool
  val exists_irredexes : (GraphEntry.T -> bool) -> T -> bool
  val get_irredexes : T -> GraphEntry.T list
  val get_redexes : T -> GraphEntry.T list
  
  
  (* add one graph entry to table. Only return the table if it has actually changed. *)
  val update_one : GraphEntry.T -> T -> T option
  val update : GraphEntry.T list -> T -> GraphEntry.T list * T
  
  val empty : T
  val mk : Theory.Ruleset.T -> T
end

functor GraphEntry(
  structure Equiv : GRAPH_EQUIV
  type data
) : GRAPH_ENTRY =
struct
  structure Graph = Equiv.Graph
  structure Equiv = Equiv
  type data = data
  
  exception stub_data_access of unit
  datatype T = GE of { graph: Graph.T, data: data, edata: Equiv.T option, dirty: bool }
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
  
  fun mk (gr, d) = let
    val (gr' ,ed) = Equiv.compute_equiv_data gr
  in mk' ((gr', SOME ed), d)
  end
end

structure EqClass =
struct
  datatype T = EQC of { rep: GraphName.name, congs: GraphName.name list, redexes: GraphName.name list }
  fun update_rep f (EQC r)     = EQC {rep= f(#rep r),congs= #congs r,redexes= #redexes r}
  fun update_congs f (EQC r)   = EQC {rep= #rep r,congs= f(#congs r),redexes= #redexes r}
  fun update_redexes f (EQC r) = EQC {rep= #rep r,congs= #congs r,redexes= f(#redexes r)}
  fun get_rep     (EQC r) = #rep r
  fun get_congs   (EQC r) = #congs r
  fun get_redexes (EQC r) = #redexes r
  val set_rep     = update_rep     o K
  val set_congs   = update_congs   o K
  val set_redexes = update_redexes o K  
  fun mk g = EQC { rep = g, congs = [], redexes = [] }
end


functor EqClassTab(
  structure Theory : GRAPHICAL_THEORY
  structure Metric : GRAPH_METRIC
  structure Equiv : GRAPH_EQUIV
  sharing Theory.Graph.SharingOGraph =
          Metric.Graph.SharingOGraph =
          Equiv.Graph.SharingOGraph
  type data
  val default_data : data
) : EQ_CLASS_TAB =
struct
  structure Theory = Theory
  structure Ruleset = Theory.Ruleset
  structure EqClass = EqClass
  structure G = Theory.Graph
  structure GraphEntry = GraphEntry(
    structure Equiv = Equiv
    type data = data)
  structure GE = GraphEntry
  structure DNet = Top_DNet(G)
  
  structure ClassIndex = Table(
    type key = int * int
    val ord = prod_ord int_ord int_ord
  )
  
  datatype T = EQT of {
    tab : (EqClass.T list) ClassIndex.table,
    initial_rs : Theory.Ruleset.T,
    dnet : DNet.T,
    graph_tab : GE.T GraphName.NTab.T
  }
  
  val empty = EQT {
    tab=ClassIndex.empty,
    initial_rs=Theory.Ruleset.empty,
    dnet=DNet.empty,
    graph_tab=GraphName.NTab.empty
  }
  
  fun update_tab f (EQT r) = EQT {tab=f(#tab r),initial_rs= #initial_rs r,dnet= #dnet r,graph_tab= #graph_tab r}
  fun update_initial_rs f (EQT r) = EQT {tab= #tab r,initial_rs=f(#initial_rs r),dnet= #dnet r,graph_tab= #graph_tab r}
  fun update_dnet f (EQT r) = EQT {tab= #tab r,initial_rs= #initial_rs r,dnet=f(#dnet r),graph_tab= #graph_tab r}
  fun update_graph_tab f (EQT r) = EQT {tab= #tab r,initial_rs= #initial_rs r,dnet= #dnet r,graph_tab=f(#graph_tab r)}

  fun get_tab         (EQT r) = #tab r
  fun get_initial_rs  (EQT r) = #initial_rs r
  fun get_dnet        (EQT r) = #dnet r
  fun get_graph_tab   (EQT r) = #graph_tab r

  val set_tab            = update_tab         o K
  val set_initial_rs     = update_initial_rs  o K
  val set_dnet           = update_dnet        o K
  val set_graph_tab      = update_graph_tab   o K

  
  fun get_graph_entry eqt g =
    GraphName.NTab.get (get_graph_tab eqt) g
    
  fun set_as_redex gn eqt =
    eqt |> update_dnet (DNet.add_graph (gn, GE.get_graph (get_graph_entry eqt gn)))

  
  (* convenience functions for pulling out redexes and irredexes *)
  local
    exception found_exn of unit
    (*fldf = fold function.
      eqt = equivalence class table
      f = the filter function
      false is the identity. Note that this function basically ignores the
      second argument (fold f [1,2,3] = f 1 (fold f [2,3]) ignores later result!
      And just raises an exception to stop instead.
      
      This might affect futures, and is pretty unnecessary.*)
    fun fold_to_exists fldf f eqt = 
      fldf (fn x => fn _ =>
        (if f x then raise found_exn () else false)) eqt false
      handle found_exn () => true
  in
    (*Applies a given function over every list of classes (so all hashed arities)
    Class index to fold I think just combines all of the results into a list...
    Note: second argument is the equivalence class table*)
    fun fold_eqclasses f = ClassIndex.fold (fn (_, classlist) => (*Apply f to every reducible expression listed in every class!*)
        fold f classlist
      ) o get_tab
    (*Given a function f to apply between elements during the fold
    (i.e: fold f [1,2,3] = f 1 (fold f [2,3])
                         = f 1 (f 2 (fold f [3]))
                         = f 1 (f 2 (f 3 id)) maybe
    apply it to the list of reducible expressions within each equivalence class.
    Second argument is the equivalence class table itself!*)
    fun fold_redexes f eqt =
      fold_eqclasses (fn class =>
        fold (fn gn => f (GraphName.NTab.get (get_graph_tab eqt) gn))
             (EqClass.get_redexes class)
      ) eqt
    (*Similar to fold_redexes to an extent. It will get the class representative,
    and then all other members of the equivalence class of the same side, and return those.
    Anything larger than both the congruences and the representative can be reduced to the
    representative by a rewrite rule.*)
    fun fold_irredexes f eqt =
      fold_eqclasses (fn class =>
        fold (fn gn => f (GraphName.NTab.get (get_graph_tab eqt) gn))
             (EqClass.get_rep class :: EqClass.get_congs class)
      ) eqt
    
    (*fun fold_rule_pairs f =
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
          fold class_fld classlist) o get_tab
      end*)
    
    (*Return all of the reducible expressions.
    cons is exactly what you think it is
    eqt = the equivalence class table*)
    fun get_redexes eqt = fold_redexes cons eqt []
    (*Return all of the irreducible expressions*)
    fun get_irredexes eqt = fold_irredexes cons eqt []
    (*Checks to see if there exists some reducible expression under which the
    given function (that needs to be passed in as the first argument here!)
    returns true on.*)
    val exists_redexes = fold_to_exists fold_redexes
    (*Similar to exists_redexes, but for irreducible graphs*)
    val exists_irredexes = fold_to_exists fold_irredexes
  end
  
  fun get_ruleset eqt =
  let
    fun add_rule nm tag rhs lhs_n rs =
    let
      val lhs = GE.get_graph (GraphName.NTab.get (get_graph_tab eqt) lhs_n)
      val r = Theory.Rule.mk (lhs, rhs)
      val (rname, rs) = Theory.Ruleset.add_fresh_rule (R.mk nm, r) rs
    in rs |> Theory.Ruleset.tag_rule rname (TagName.mk tag)
    end
    
    fun add_class class rs =
      let
        val rep = GE.get_graph (GraphName.NTab.get (get_graph_tab eqt) (EqClass.get_rep class))
        val add_redex = add_rule "r_0" "redex" rep
        val add_cong = add_rule "c_0" "cong" rep
      in
        fold add_redex (EqClass.get_redexes class)
        (fold add_cong (EqClass.get_congs class) rs)
      end
  in
    ClassIndex.fold (fn (_, classlist) =>
      fold add_class classlist) (get_tab eqt) (get_initial_rs eqt)
  end
  (*Mathing algorithm is used here as before. is_some effectively just looks
  to see if there is a single match! Note that this only needs the first match,
  so laziness is very important. In other words, either use a special case
  in the matching algorithm for the first element, or just use the sequential algorithm!!*)
  fun has_match tgt pat =
  (*Mathing algorithm is used here as before. is_some effectively just looks
  to see if there is a single match! Note that this only needs the first match,
  so laziness is very important. In other words, either use a special case
  in the matching algorithm for the first element, or just use the sequential algorithm!!*)
    is_some (Seq.pull (Theory.MatchSearch.match
      (get_graph pat) (get_graph tgt)
    ))
  
  (*rs = a rule set. tgt = the existing graph.
  This checks all the left hand sides of all known rules. A rule is of the form
  L -> R where L can be rewritten to R I think. Hence, if L is spotted as a subgraph (match)
  within a graph G, its ocurrence within G can be replaced by R. Assuming all kept rules
  reduce graphs in "size", then if such a rule with a match exists, this graph can be
  considered reducible to a smaller graph. If all smaller graphs were already evaluated,
  this returning true means we can effectively ignore this graph, since we already
  know we can rewrite it.*)
  fun rs_can_reduce rs tgt =
  let (*Also uses the matching algorithm!! Also requires only the first element*)
    fun r_matches r_name = (*r_matches  = rule matches*)
      is_some (Seq.pull (Theory.MatchSearch.match
        (*Left hand side of a rule is some L -> R
        L exists within G. Hence, the left hand sides form pattern graphs.*)
        (Theory.Rule.get_lhs (Ruleset.get_rule rs r_name)) (get_graph tgt)
      ))
  in R.NSet.exists r_matches
     (Theory.Ruleset.get_rules_in_tag rs (TagName.mk "redex"))
  end*)
  
  (* returns (SOME new_class) if graph is equivalent to the representative,
     and NONE otherwise *)
     
(*  fun find_and_add eqt gn = let
    val ge = GraphName.NTab.get (get_graph_tab eqt) gn

  *)
  
  (*fun update_one ge eqt =
    if (*rs_can_reduce (get_initial_rs eqt) ge orelse
       exists_redexes (has_match ge) eqt*) false
    then NONE
    else let
      val (gn, gt) = get_graph_tab eqt |> GraphName.NTab.add (GraphName.mk "g0", ge)
      val eqt = eqt |> set_graph_tab gt
      val gr = GE.get_graph ge
      val arity = (V.NSet.cardinality (G.get_inputs gr),
                   V.NSet.cardinality (G.get_outputs gr))
    in SOME (eqt |>
      update_tab (ClassIndex.map_default (arity, [])
        (find_and_add eqt gn)
      ))
    end*)
  
  fun has_match_in_dnet eqt tgt = let
     (*checks all with the same arity and checks matching
     Hence, this specifically checks to see if, for all reducible expressions, whether or not
     a reducible expression in the equivalence class table exists within the existing graph.
     If so, it again means this graph can be reduced. This is sort of redundant if the DNET
     using rs_can_reduce already has all the rewrite rules from with the equivalence class
     table, but while the table is formed this is not the case, so the above check is worth while.
     Also, it seems odd that we check ALL graphs, since the paper suggests you need only
     check graphs in the same equivalence class...
     
     Is it bad that the irreducible graphs are not looked for? No - if you found the irreducible
     graph within your graph, what do you plan to do with it anyway...*)
    fun has_match pat_n = let
      val pat = get_graph_entry eqt pat_n
    in is_some (Seq.pull (Theory.MatchSearch.match
         (GE.get_graph pat) (GE.get_graph tgt)
       ))
    end
    (*Firstly, see if it exists in the equivalence table already...*)
  in GraphName.NSet.exists has_match
      (DNet.get_match_candidates (get_dnet eqt) (GE.get_graph tgt))
  end
  
  fun update_one ge eqt =
    (*This is where the DNET comes in
    See if the LHS of any existing rule within the rule set (rule set can reduce) appears in the graph. That would
    mean that the ocurrence of the LHS in the graph can be replaced by the RHS, and assuming
    this reduces the size of the graph, that indicates we can reduce it to a graph we're already
    found.*)
    if has_match_in_dnet eqt ge then NONE (*No need to add this*)
    else let (*Add this to the equivalence class*)
      val (gn, gt) = get_graph_tab eqt |> GraphName.NTab.add (GraphName.mk "g0", ge)
      val eqt' = eqt |> set_graph_tab gt
      val gr = GE.get_graph ge (*Get the graph from the graph entry*)
      val arity = (V.NSet.cardinality (G.get_inputs gr), (*For hashing*)
                   V.NSet.cardinality (G.get_outputs gr))
      val classlist = case ClassIndex.lookup (get_tab eqt) arity
                      of SOME cs => cs | NONE => []
      (*
      cls = equivalence class to try and add the graph too
      ge = graph entry to be added
      
      Checks first that the graph and the minimal graph are equivalent by the equivalence metric.
      If so, it sees by the ordering metric whether this graph is any smaller or not.
      The Congruences are remembered explicity to derive all of the cycle rewrite rules.
      Hence, if it is the same size, then it is added to the set of congruences.
      
      If C and C' are in the set of congruences, then rewrite rules exist such that
      C -> C' and C' -> C
      
      If G is in the equivalence class but not minimal, then rewrite rules exist such that
      for all C in the set of congruences, and R the representive,
      G -> R and G -> C
      
      Returns: NONE     - to indicate it couldn't be added
               SOME cls - to indicate it was added, and cls is the modified class
      *)
      fun tryadd_to_class cls eqt = let (*Does not guarantee that they are equivalent*)
        val cls_ge = GraphName.NTab.get (get_graph_tab eqt) (EqClass.get_rep cls)
        val inclass = (*Create a new class, since it is distinct from the rest.*)
          case (GE.get_edata cls_ge, GE.get_edata ge)
            (*Equivalence metric*)
            of (SOME d1, SOME d2) => Equiv.eq (d1, d2) (*Succeeded, so remember modified version*)
             | _ => false (*Create a new class, since it is distinct from the rest.*)
      in
        if inclass
        then
        SOME (
          case Metric.ord_graph (GE.get_graph cls_ge, GE.get_graph ge)
          (*representative stays (minimal)*)
          of LESS    => (cls |> EqClass.update_redexes (cons gn), eqt |> set_as_redex gn)
           | GREATER =>
              let
                val new_reds = EqClass.get_rep cls :: EqClass.get_congs cls
              in (cls |> EqClass.update_redexes (fn reds => new_reds @ reds)
                      |> EqClass.set_congs [] (*new graph should be new representative*)
                      |> EqClass.set_rep gn,
                  fold set_as_redex new_reds eqt)
              end
              (*Congs contains equivalent graphs*)
           | EQUAL   => (cls |> EqClass.update_congs (cons gn), eqt)
        )
        else NONE
      end
      
      fun tryaddf cls (found, rest, eqt) =
        if found then (true, cls :: rest, eqt)
        else case tryadd_to_class cls eqt
               of SOME (cls', eqt') => (true, cls' :: rest, eqt')
                | NONE => (false, cls :: rest, eqt)
      
      val (found, classlist', eqt') = fold tryaddf classlist (false, [], eqt')
    in
    (*Find the appropriate row for this graph in the table, if it exists, and then add it.*)
    (*The arity forms the key. The [] will be used if no entry for the given key exists, in which
    case find_and_add ge [] will be used. Otherwise,
    find_and_add ge ls will be used where ls is the associated list. (See table.ML)
    Hence, if the arity fails to exist then we add a new row in the physical table,
    and a new equivalence class. If the arity does exist, then we will add
    the graph entry either to an existing equivalence class with that arity, or create
    a new class with the same arity.*)
      SOME (
        eqt' |> update_tab (ClassIndex.update (arity,
                  if found then classlist'
                  else EqClass.mk gn :: classlist'))
      )
    end
  (*Arity works like hashcode - we know only graphs with equivalent arity could belong to the same class*)
  
  (*Add multiple graph entries at once
  Note: eqt = equivalence table
  
  See update one for details. It does all the work. The fold
  just remembers the equivalence table as it is formed.*)
  fun update ge_list eqtab = let
    fun updatef ge (ges, eqt) =
      case update_one ge eqt
        of SOME eqt' => (ge::ges, eqt') (*We only add the generators that are identical (could return boolean)*)
         | NONE      => (ges, eqt)
  in fold updatef ge_list ([],eqtab) (*BRANCH POINT*)
  end
  
  (*
  IMPORTANT:
  Unsurprisingly, it is the equivalence metric which determines whether or not
  two graphs belong in the same equivalence class. You would think this is where most
  of the work is done (and I had believed this was where the matching was important, but alas
  I was wrong). The equivalence checking is done typically with tensors and their contraction
  to other forms. I know nothing about this at the moment... Maybe I will find out more
  from the recommended book?
  
  Apparently, this is "fast" so perhaps it is not even NP Complete? Hopefully, it will not
  turn out to take up the significant use of the CPU time, since I will be focussing
  on parallelising across the matching. I can probably parallelise across this too though.
  
  Whenever a graph (entry) is added to the equivalence table, it first:
  1. Goes through the DNET with its existing set of rules (currently using a default, so not really
  active but you should assume it will be)
  2. Next, with every reducible graph in the table, see if any match (are subgraphs of) the graph
  being added. If so, ignore it. This runs the matching algorithm MANY times if
  there are many reducible expressions (graphs)
  3. Finally, try adding the graph to each equivalence class. The Equiv metric will identify
  when the right class has been found, and then the ordering metric will trigger the appropriate
  update. Apparently this is fast, and there won't be many classes to try with hashing anyway.
  
  So, certainly parallelising across 1 and 2 is a good idea. In the merging of equivalence
  tables, the number of reducible expressions will be initially very small. Hence, at first
  there will be many tables all of which are very fine grained, and later there will be few
  which offer very coarse grain concurrency. Hence, a dynamic model to adjust for the times
  taken is EXTREMELY useful (WOOO!). With such a model, I can just parallelise straight across.
  
  Note that the tree might have trouble with this. Initially, it will build up huge numbers
  of tables it needs to merge at once. However, it does so by considering nodes in order.
  It would be wise to supply all the small tables first, and then the larger ones later,
  so the formation of the nodes it would parallelise across is relevant.
  
  An alternative is just to set off a new compactor on each row. This is less risky in the sense
  of accidentally eating a whole row above and struggling to climb down in time exponentially
  as the grain becomes coarser (don't want to wind up eating the whole tree at every step)
  
  In fact, it might even be worth adding a guard against this, to prevent the whole
  tree being consumed at each step. Otherwise, only one core will ever be allowed to run...
  
  See project journal for suggestions
  *)
  fun mk rs = let
    val initial_redexes = Ruleset.get_rules_in_tag rs (TagName.mk "redex")
    fun add_rule rn eqt = let
      val ge = GE.mk' ((Theory.Rule.get_lhs (Ruleset.get_rule rs rn), NONE), default_data)
      val (gn, tab) = (get_graph_tab eqt) |> GraphName.NTab.add (GraphName.mk "r0", ge)
    in eqt |> set_graph_tab tab |> set_as_redex gn
    end (*initial_rs includes many common reductions*)
  in R.NSet.fold add_rule initial_redexes (empty |> set_initial_rs rs)
  end
end










