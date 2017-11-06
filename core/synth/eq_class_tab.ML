signature GRAPH_ENTRY =
sig
  structure Graph : BANG_GRAPH
  structure Equiv : GRAPH_EQUIV
  sharing Equiv.Graph.Sharing = Graph.Sharing
  
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
  sharing GraphEntry.Graph.Sharing
    = Theory.Graph.Sharing
    = DNet.G.Sharing
  
  
  val update_initial_rs : (Theory.Ruleset.T -> Theory.Ruleset.T) -> T -> T
  val get_initial_rs    : T -> Theory.Ruleset.T
  val set_initial_rs    : Theory.Ruleset.T -> T -> T

  (* also adds to DNET *)
  val set_initial_ruleset    : Theory.Ruleset.T -> T -> T

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
  sharing Theory.Graph.Sharing =
          Metric.Graph.Sharing =
          Equiv.Graph.Sharing
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
    graph_tab : GE.T GraphName.NTab.T,
    graph_tab' : GE.Graph.T GraphName.NTab.T
  }
  
  val empty = EQT {
    tab=ClassIndex.empty,
    initial_rs=Theory.Ruleset.empty,
    dnet=DNet.empty,
    graph_tab=GraphName.NTab.empty,
    graph_tab'=GraphName.NTab.empty
  }

  fun update_tab f (EQT r) = EQT {tab=f(#tab r),initial_rs= #initial_rs r,dnet= #dnet r,graph_tab= #graph_tab r,graph_tab'= #graph_tab' r}
  fun update_initial_rs f (EQT r) = EQT {tab= #tab r,initial_rs=f(#initial_rs r),dnet= #dnet r,graph_tab= #graph_tab r,graph_tab'= #graph_tab' r}
  fun update_dnet f (EQT r) = EQT {tab= #tab r,initial_rs= #initial_rs r,dnet=f(#dnet r),graph_tab= #graph_tab r,graph_tab'= #graph_tab' r}
  fun update_graph_tab f (EQT r) = EQT {tab= #tab r,initial_rs= #initial_rs r,dnet= #dnet r,graph_tab=f(#graph_tab r),graph_tab'= #graph_tab' r}
  fun update_graph_tab' f (EQT r) = EQT {tab= #tab r,initial_rs= #initial_rs r,dnet= #dnet r,graph_tab= #graph_tab r,graph_tab'=f(#graph_tab' r)}

  fun get_tab        (EQT r) = #tab r
  fun get_initial_rs (EQT r) = #initial_rs r
  fun get_dnet       (EQT r) = #dnet r
  fun get_graph_tab  (EQT r) = #graph_tab r
  fun get_graph_tab' (EQT r) = #graph_tab' r

  fun set_tab        x  = update_tab        (fn _ => x)
  fun set_initial_rs x  = update_initial_rs (fn _ => x)
  fun set_dnet       x  = update_dnet       (fn _ => x)
  fun set_graph_tab  x  = update_graph_tab  (fn _ => x)
  fun set_graph_tab' x  = update_graph_tab' (fn _ => x)

  
  (*fun update_tab f (EQT r) = EQT {tab=f(#tab r),initial_rs= #initial_rs r,dnet= #dnet r,graph_tab= #graph_tab r}
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
  val set_graph_tab      = update_graph_tab   o K*)


  fun set_initial_ruleset rs eqt = 
    let
      val eqt =  eqt |> set_initial_rs rs
      fun addr (rn,rule) eqt =
      let
        val gn = GraphName.mk ("g-" ^ R.dest rn)
        val g = Ruleset.Rule.get_lhs rule
      in
        eqt |> update_graph_tab' (GraphName.NTab.doadd (gn,g))
            |> update_dnet (DNet.add_graph (gn,g))
      end
    in
      RTab.fold addr (Ruleset.get_allrules rs) eqt
    end
  
  
  fun get_graph_entry eqt g =
    GraphName.NTab.get (get_graph_tab eqt) g

  fun get_graph eqt g =
    case GraphName.NTab.lookup (get_graph_tab eqt) g
      of SOME ge => (GraphEntry.get_graph ge)
       | NONE => (GraphName.NTab.get (get_graph_tab' eqt) g)
    
  fun set_as_redex gn eqt =
    eqt |> update_dnet (DNet.add_graph (gn, GE.get_graph (get_graph_entry eqt gn)))

  
  (* convenience functions for pulling out redexes and irredexes *)
  local
    exception found_exn of unit
    fun fold_to_exists fldf f eqt = 
      fldf (fn x => fn _ =>
        (if f x then raise found_exn () else false)) eqt false
      handle found_exn () => true
  in
    fun fold_eqclasses f = ClassIndex.fold (fn (_, classlist) =>
        fold f classlist
      ) o get_tab
  
    fun fold_redexes f eqt =
      fold_eqclasses (fn class =>
        fold (fn gn => f (GraphName.NTab.get (get_graph_tab eqt) gn))
             (EqClass.get_redexes class)
      ) eqt
      
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
    
    fun get_redexes eqt = fold_redexes cons eqt []
    fun get_irredexes eqt = fold_irredexes cons eqt []
    val exists_redexes = fold_to_exists fold_redexes
    val exists_irredexes = fold_to_exists fold_irredexes
  end
  
  
  fun get_ruleset eqt =
  let
    fun add_rule nm tag rhs lhs_n rs =
    let
      val lhs = GE.get_graph (GraphName.NTab.get (get_graph_tab eqt) lhs_n)
      val r = Theory.Rule.mk (lhs, rhs)
      val (rname, rs) = rs |> Theory.Ruleset.add_fresh_rule r
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
  
  
  (*fun has_match tgt pat =
    is_some (Seq.pull (Theory.MatchSearch.match
      (get_graph pat) (get_graph tgt)
    ))
  
  fun rs_can_reduce rs tgt =
  let
    fun r_matches r_name =
      is_some (Seq.pull (Theory.MatchSearch.match
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
    fun has_match pat_n = let
      val pat = get_graph eqt pat_n
    in is_some (Seq.pull (
         Theory.MatchSearch.match pat (GE.get_graph tgt)
       ))
    end
  in GraphName.NSet.exists has_match
      (DNet.get_match_candidates (get_dnet eqt) (GE.get_graph tgt))
  end
  
  fun update_one ge eqt =
    if has_match_in_dnet eqt ge then NONE
    else let
      val (gn, gt) = get_graph_tab eqt |> GraphName.NTab.add (GraphName.mk "g0", ge)
      val eqt' = eqt |> set_graph_tab gt
      val gr = GE.get_graph ge
      val arity = (V.NSet.cardinality (G.get_inputs gr),
                   V.NSet.cardinality (G.get_outputs gr))
      val classlist = case ClassIndex.lookup (get_tab eqt) arity
                      of SOME cs => cs | NONE => []
      
      fun tryadd_to_class cls eqt = let
        val cls_ge = GraphName.NTab.get (get_graph_tab eqt) (EqClass.get_rep cls)
        val inclass =
          case (GE.get_edata cls_ge, GE.get_edata ge)
            of (SOME d1, SOME d2) => Equiv.eq (d1, d2)
             | _ => false
      in
        if inclass
        then
        SOME (
          case Metric.ord_graph (GE.get_graph cls_ge, GE.get_graph ge)
          of LESS    => (cls |> EqClass.update_redexes (cons gn), eqt |> set_as_redex gn)
           | GREATER =>
              let
                val new_reds = EqClass.get_rep cls :: EqClass.get_congs cls
              in (cls |> EqClass.update_redexes (fn reds => new_reds @ reds)
                      |> EqClass.set_congs []
                      |> EqClass.set_rep gn,
                  fold set_as_redex new_reds eqt)
              end
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
      SOME (
        eqt' |> update_tab (ClassIndex.update (arity,
                  if found then classlist'
                  else EqClass.mk gn :: classlist'))
      )
    end
  
  fun update ge_list eqtab = let
    fun updatef ge (ges, eqt) =
      case update_one ge eqt
        of SOME eqt' => (ge::ges, eqt')
         | NONE      => (ges, eqt)
  in fold updatef ge_list ([],eqtab)
  end
  
  
  fun mk rs = let
    val initial_redexes = Ruleset.get_rules_in_tag rs (TagName.mk "redex")
    fun add_rule rn eqt = let
      val ge = GE.mk' ((Theory.Rule.get_lhs (Ruleset.get_rule rs rn), NONE), default_data)
      val (gn, tab) = (get_graph_tab eqt) |> GraphName.NTab.add (GraphName.mk "r0", ge)
    in eqt |> set_graph_tab tab |> set_as_redex gn
    end
  in R.NSet.fold add_rule initial_redexes (empty |> set_initial_rs rs)
  end
end










