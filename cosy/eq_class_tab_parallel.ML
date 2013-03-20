(* QUESTIONS

1. set_as_redex does not make a lot of sense to me. Do graph names relate to how they are reduced? (I.e: a graph has the same
name as another graph iff one can be reduced to the other?)

2. get_ruleset appears not to include the reverse rewrite rules Rep -> C for each congruence at the moment, but the paper
"Synthesising Graphical Theories" suggests that these should be included.

3. has_match_in_dnet relies on the DNET returning graphs which are reducible as pattern graphs. One of these graphs must match the
supplied target graph for a match to be identified. Presumably this behaviour is desired. (The whole "doesn't necessarily match" thing
is what was worrying me)

4. in tryadd_to_class if either graph lacks equivalence data then they are considered unique. If they both don't have edge data presumably
this is still true (?). Presumably it is correct to say that two graphs are equivalent so long as they have the same edge data.

5. mk does not supply any equivalence data to any lhs of a rule... Assume congruencies are not important by this point.

*)

(*The entire equivalence class table
Each equivalence class table has its own DNET and its own table of equivalence classes
(which uses a hash by arity I think for efficiency)
It can also have its own initial rule set*)
signature PARALLEL_EQ_CLASS_TAB =
sig
  type T
  structure Theory : GRAPHICAL_THEORY
  structure EqClass : EQ_CLASS
  structure GraphEntry : GRAPH_ENTRY
  structure DNet : TOP_DNET (*The decision network*)
  sharing GraphEntry.Graph.SharingOGraph
    = Theory.Graph.SharingOGraph
    = DNet.G.SharingOGraph
  
  (*The initial rule set is what forms the axioms of any theory. They need to be added in at first
  to ensure that not everything winds up in its own class! Generally, you won't (shouldn't) be changing this during
  computation!*)
  val update_initial_rs : (Theory.Ruleset.T -> Theory.Ruleset.T) -> T -> T
  val get_initial_rs    : T -> Theory.Ruleset.T
  val set_initial_rs    : Theory.Ruleset.T -> T -> T
  (*Get the ruleset that has been generated within this table*)
  val get_ruleset : T -> Theory.Ruleset.T
  
  (*Return the graph entry corresponding to a particular graph name (since only graph names are held in
  equivalence classes)*)
  val get_graph_entry : T -> GraphName.name -> GraphEntry.T
  (*Get the entire table of graph entries...*)
  val get_graph_tab : T -> GraphEntry.T GraphName.NTab.T
  
  (*Apply a function over every single equivalence class. You supply the identity element 'a and then it should
  accumulate its own results*)
  val fold_eqclasses : (EqClass.T -> 'a -> 'a) -> T -> 'a -> 'a
  (*Apply a function, accumulating its results, over all reducible expressions in the ENTIRE table. Fairly inefficient
  if you just want to apply the function to a single EQClass*)
  val fold_redexes : (GraphEntry.T -> 'a -> 'a) -> T -> 'a -> 'a
  (*An irreducible expression is any Congruence or Representative (checked code - this is TRUE)*)
  val fold_irredexes : (GraphEntry.T -> 'a -> 'a) -> T -> 'a -> 'a
  
  (*Check to see if there exists a reducible expression which meets the specified predicate*)
  val exists_redexes : (GraphEntry.T -> bool) -> T -> bool
  (*Check to see if there exists an irreducible expression which meets the specified predicate*)
  val exists_irredexes : (GraphEntry.T -> bool) -> T -> bool
  (*Get all of the irreducible expressions (Congs + Reps - THEY ARE DISJOINT)*)
  val get_irredexes : T -> GraphEntry.T list
  (*Get all of the reducible expressions*)
  val get_redexes : T -> GraphEntry.T list
  
  
  (*Add a single graph entry to the table. NONE is returned if the table does not change, which will be due to the DNET
  catching the graph. (AND ONLY due to this. Otherwise it will be added)*)
  val update_one : GraphEntry.T -> T -> T option
  (*Add a lot of graph entries at once. Literally just calls update_one many times.
  The list of graph entries returned represents those which were actually added. Any graph entry not in that
  list was caught by the DNET*)
  val update : GraphEntry.T list -> T -> GraphEntry.T list * T
  
  (*These functions mimic update_one and update, but they perform a hard add, in that the DNET is not first checked before
  adding the graphs. You should only use this if you are certain that the graph will pass through the DNET already*)
  val hard_update_one : GraphEntry.T -> T -> T option
  
  (*Apply a hard update to many graphs in a row*)
  val hard_update : GraphEntry.T list -> T -> GraphEntry.T list * T
  
  (*Check for a match in the DNET of this equivalence class table*)
  val has_match_in_dnet : T -> GraphEntry.T -> bool
  
  (*Create a new empty equivalence class table!*)
  val empty : T
  (*Make an equivalence class table with the initial rule set*)
  val mk : Theory.Ruleset.T -> T
  
  (*For the parallel version of the equivalence class table, we have some additional methods...*)
  
  (*State that all of the graphs in the equivalence class table can be declared as old.
  This means that the set of new graphs held by the table will now be thrown away!*)
  val clean : T -> T
  
  (*Performs the dnet check between only new graphs*)
  val has_match_in_dnet_with_new : T -> GraphEntry.T -> bool
  
  (*Whenever graphs are added to the equivalence class table (in any way), they are added as new graphs
  Calling the merge function between two equivalence class tables will cause all of the new graphs, and only the new graphs,
  in one table to be added to the other. I think it is safe to bypass the DNET at this point...
  
  The list of graph entries are those entries that were added from the first table to those in the second table
  
  Note that graphs remain new after the merge*)
  val merge : T -> T -> GraphEntry.T list *T
  
end

(*The equivalence class table itself! WOOO!*)
functor ParallelEqClassTab(
  structure Theory : GRAPHICAL_THEORY
  (*This will tell the EqTab whether one graph is larger, smaller or equal to another graph in size.
  Don't worry about how the metric actually works - you don't need to know (tensors?)*)
  structure Metric : GRAPH_METRIC
  (*The equivalence metric thing. This is NOT the same as the actual Metric above. If Metric says they're the same size, this does not
  mean that they are necessarily equal. Hence, you use Equiv to determine the right class, and then Metric to determine
  the position in that class.
  
  Two graphs are equivalence if they have the same EDGE DATA*)
  structure Equiv : GRAPH_EQUIV
  (*Random stuff*)
  sharing Theory.Graph.SharingOGraph =
          Metric.Graph.SharingOGraph =
          Equiv.Graph.SharingOGraph
  type data
  val default_data : data
) : PARALLEL_EQ_CLASS_TAB =
struct
  (*Set lots of stuff*)
  structure Theory = Theory
  structure Ruleset = Theory.Ruleset
  structure EqClass = EqClass
  structure G = Theory.Graph
  structure GraphEntry = GraphEntry(
    structure Equiv = Equiv
    type data = data)
  structure GE = GraphEntry
  structure DNet = Top_DNet(G)
  (*HASHING IN TABLE*)
  (*This is how the hashing is done. The left integer is the number of inputs, and the right integer
  is the number of outputs. I'm pretty sure what is below is the product order, which means:
  (x,y) <= (x2,y2) iff x<=x2 && y<=y2
  *)
  structure ClassIndex = Table(
    type key = int * int
    val ord = prod_ord int_ord int_ord
  )
  
  (*The type of an equivalence class table!*)
  datatype T = EQT of {
    tab : (EqClass.T list) ClassIndex.table, (*Holds the equivalence classes, hashed by input output arity *)
    initial_rs : Theory.Ruleset.T, (*Holds an initial rule set. Not sure why this is held separately, but there you go...*)
    dnet : DNet.T, (*The DNET. This decides if a graph should be added to the table or not*)
    graph_tab : GE.T GraphName.NTab.T, (*The table holding all of the graph entries. Points from graph entries to their internal names*)
    new_graphs : GraphName.NSet.T (*A set of the graphs which are considered "newly" added*)
  }
  
  (*A new empty equivalence class table*)
  val empty = EQT {
    tab=ClassIndex.empty,
    initial_rs=Theory.Ruleset.empty,
    dnet=DNet.empty,
    graph_tab=GraphName.NTab.empty,
    new_graphs=GraphName.NSet.empty
  }
  
  (*Dealing with the record structure. So many lines...*)
  fun update_tab f (EQT r) = EQT {tab=f(#tab r),initial_rs= #initial_rs r,dnet= #dnet r,graph_tab= #graph_tab r,new_graphs= #new_graphs r}
  fun update_initial_rs f (EQT r) = EQT {tab= #tab r,initial_rs=f(#initial_rs r),dnet= #dnet r,graph_tab= #graph_tab r,new_graphs= #new_graphs r}
  fun update_dnet f (EQT r) = EQT {tab= #tab r,initial_rs= #initial_rs r,dnet=f(#dnet r),graph_tab= #graph_tab r,new_graphs= #new_graphs r}
  fun update_graph_tab f (EQT r) = EQT {tab= #tab r,initial_rs= #initial_rs r,dnet= #dnet r,graph_tab=f(#graph_tab r),new_graphs= #new_graphs r}
  fun update_new_graphs f (EQT r) = EQT {tab= #tab r,initial_rs= #initial_rs r,dnet= #dnet r,graph_tab= #graph_tab r,new_graphs=f(#new_graphs r)}

  fun get_tab         (EQT r) = #tab r
  fun get_initial_rs  (EQT r) = #initial_rs r
  fun get_dnet        (EQT r) = #dnet r
  fun get_graph_tab   (EQT r) = #graph_tab r
  fun get_new_graphs  (EQT r) = #new_graphs r

  val set_tab            = update_tab         o K
  val set_initial_rs     = update_initial_rs  o K
  val set_dnet           = update_dnet        o K
  val set_graph_tab      = update_graph_tab   o K
  val set_new_graphs     = update_new_graphs  o K
  
  (*Add a "new" graph to the set of new graphs*)
  fun add_new_graph eqt g = update_new_graphs (GraphName.NSet.add g) eqt

  (*Find the graph entry g in this equivalence class table eqt
  Returns the graph name that corresponds...*)
  fun get_graph_entry eqt g =
    GraphName.NTab.get (get_graph_tab eqt) g
    
  (*Suppose this sets the given graph as reducible... Not quite sure how the DNET is supposed to use this...
  Not in the signature*)
  fun set_as_redex gn eqt = (*Retrieve the graph itself according to its name. Set the name to reduce to this graph
  Presumably then the names have some relationship to whether or not graphs can be reduced.*)
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
    (*Apply the function f to all the reducible expressions in the equivalence class table.
    f should accumulate its results, and you need to pass in an initial identity element*)
    fun fold_redexes f eqt =
      fold_eqclasses (fn class =>
        fold (fn gn => f (GraphName.NTab.get (get_graph_tab eqt) gn))
             (EqClass.get_redexes class)
      ) eqt
    (*Similar to fold_redexes, but for the representatives and congruencies*)
    fun fold_irredexes f eqt =
      fold_eqclasses (fn class =>
        fold (fn gn => f (GraphName.NTab.get (get_graph_tab eqt) gn))
             (EqClass.get_rep class :: EqClass.get_congs class)
      ) eqt
    
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
  (*LOCAL DEFINITIONS FINISHED *)
  
  (*This function automatically deduces all of the rules derived from the equivalence class table.
  Note that it will do this each time it is called ATTOW. The EQT does not derive these iteratively since
  they may change.*)
  fun get_ruleset eqt =
  let
    fun add_rule nm tag rhs lhs_n rs =
    let
      (*Find whatever graph entry we actually wanted*)
      val lhs = GE.get_graph (GraphName.NTab.get (get_graph_tab eqt) lhs_n)
      (*Make a new rule!*)
      val r = Theory.Rule.mk (lhs, rhs)
      (*Derive some kind of name for this rule (which is unique I think). rs is the existing rule set.
      We add the rule to the ruleset.*)
      val (rname, rs) = Theory.Ruleset.add_fresh_rule (R.mk nm, r) rs
    in rs |> Theory.Ruleset.tag_rule rname (TagName.mk tag) (*Tag it for reasons I am unsure of at the moment*)
    end
    (*Given a specific class and its ruleset, add all of the rules which can be derived from that class*)
    fun add_class class rs =
      let
        val rep = GE.get_graph (GraphName.NTab.get (get_graph_tab eqt) (EqClass.get_rep class))
        (*Don't ask me what the point of the name is... the tag says what kind of graph it was.
        rep should be an actual graph. The function expects a graph NAME to be supplied beyond this.
        If you pass R, it will add the rewrite R -> rep*)
        val add_redex = add_rule "r_0" "redex" rep
        (*Similar to add_redex except for the different name and tag. Should be used when handling the congruencies*)
        val add_cong = add_rule "c_0" "cong" rep
      in
        (*Add all of the relevant rules in. Appears to violate paper!!!!
        Synthesising Graphical Theories says we should also add rewrite rule C -> rep and rep -> C for each congruence.
        We only add C -> rep it seems*)
        fold add_redex (EqClass.get_redexes class)
        (fold add_cong (EqClass.get_congs class) rs)
      end
  in
    (*Add the relevant rules for every single class. The ignored element is the arity I think.
    Each arity holds a list of classes.*)
    ClassIndex.fold (fn (_, classlist) =>
      fold add_class classlist) (get_tab eqt) (get_initial_rs eqt)
  end
  
  (*Check to see whether the supplied target graph (tgt is a Graph Entry) matches a
  reducible expression already found within the DNET.
  g_set is the set of match candidates that should be considered*)
  fun has_match_in_dnet' eqt tgt graph_name_set = let
    (*val _ = PolyML.print("Check DNET");*)
    (*Check to see if the pattern graph (with the specified name) matches the supplied graph*)
    fun has_match pat_n = let
      val pat = get_graph_entry eqt pat_n;
      val sw = Stopwatch.start Stopwatch.new;
      (*val _ = PolyML.print("Beginning match search in DNET");*)
      (*Only need to check for a single match here! Parallelising is overkill as the first
      graph entry is found almost entirely sequentially*)
      val ans = is_some (Seq.pull (Theory.MatchSearch.match
                   (GE.get_graph pat) (GE.get_graph tgt)
                 ))
      (*Time it!*)
      val t = Time.toMicroseconds (Stopwatch.check sw)
      (*val _ = if (t>1000000) then (PolyML.print("Done check in DNET. Took " ^ (LargeInt.toString(t)) ^ " microseconds");()) else ();*)
    in
      ans
    end
    (*val _ = PolyML.print("Checking for match");*)
    val ans = GraphName.NSet.exists has_match graph_name_set;
    (*val _ = PolyML.print("Finished checking for match");*)
    (*Use the DNET to determine which graphs could potentially match the supplied graph.
    Then, for each pattern graph returned by the DNET, see if it actually does match the supplied graph*)
  in
    ans
  end
  (*Check against all of the graphs in the DNET whether or not the target graph tgt has a match*)
  fun has_match_in_dnet eqt tgt = let
    (*val _ = PolyML.print("Retrieving candidates");*)
    val ans = DNet.get_match_candidates (get_dnet eqt) (GE.get_graph tgt);
    (*val _ = PolyML.print("Got candidates")*)
  in
    has_match_in_dnet' eqt tgt ans
  end
  (*Check only against newly added graphs with the DNET. Note that the DNET is still required to return
  all of the possible candidates, but then the matching algorithms is only computed against some of the new versions of these graphs*)
  fun has_match_in_dnet_with_new eqt tgt = let
    (*Return all of the candidates*)
    (*val _ = PolyML.print("Retrieving candidates+");*)
    val all_candidates = DNet.get_match_candidates (get_dnet eqt) (GE.get_graph tgt);
    (*val _ = PolyML.print("Got candidates+");*)
    (*Filter function for removing the old graphs*)
    fun is_new_graph graph_name = GraphName.NSet.contains (get_new_graphs eqt) graph_name;
    (*All of the new graphs that were match candidates*)
    val new_only = GraphName.NSet.filter is_new_graph all_candidates
  in
    has_match_in_dnet' eqt tgt new_only
  end
  
  (*Add a single graph entry to the equivalence class table. If it changes the table, SOME eqt is returned. Otherwise,
  NONE is returned.
  The hard flag indicates whether or not the DNET should be checked (true meaning the DNET should not be checked)*)
  fun update_one' ge eqt hard=
    (*Sees if the supplied graph should be added to the class or not.*)
    if ((not hard) andalso (has_match_in_dnet eqt ge)) then NONE (*No need to add this- can't derive anything interesting*)
    else let (*Add this to the equivalence class*)
      (*gn is the corresponding name assigned to the graph (which will be a unique derivation
      from g0) and gt is the new table.*)
      val (gn, gt) = get_graph_tab eqt |> GraphName.NTab.add (GraphName.mk "g0", ge)
      (*Update the equivalence class table...*)
      val eqt' = eqt |> set_graph_tab gt
      val gr = GE.get_graph ge (*Get the graph from the graph entry*)
      val arity = (V.NSet.cardinality (G.get_inputs gr), (*For hashing*)
                   V.NSet.cardinality (G.get_outputs gr))
      (*Find out if this hash already containts some classes or not.*)
      val classlist = case ClassIndex.lookup (get_tab eqt) arity
                      of SOME cs => cs | NONE => []
      (*
      cls = equivalence class to try and add the graph too
      ge = graph entry to be added
      
      Checks first that the graph and the minimal graph are equivalent by the equivalence metric.
      If so, it sees by the ordering metric whether this graph is any smaller or not.
      The Congruencies are remembered explicity to derive all of the cycle rewrite rules.
      Hence, if it is the same size, then it is added to the set of congruences.
      
      If R is the representative and C is a congruence, then rewrite rules exist such that
      R -> C and C -> R
      
      If G is in the equivalence class but not minimal, then rewrite rule G -> R exists
      
      Returns: NONE               - to indicate it couldn't be added
               SOME (cls,new_eqt) - to indicate it was added, and cls is the modified class
      *)
      fun tryadd_to_class cls eqt = let
        (*Get the graph entry of the class representative*)
        val cls_ge = GraphName.NTab.get (get_graph_tab eqt) (EqClass.get_rep cls)
        val inclass = 
          (*Retrieve the equivalence data for each graph entry*)
          case (GE.get_edata cls_ge, GE.get_edata ge)
            (*Equivalence metric*)
            of (SOME d1, SOME d2) => Equiv.eq (d1, d2) (*True if they have the same equivalence data, and false otherwise*)
             | _ => false (*Create a new class, since it is distinct from the rest. No equivalence data indicates
             that the two must be different. (?)*)
      in
        if inclass (*Does ge belong to class cls?*)
        then (*YES*)
        SOME (
          (*Find out where it should go. cls_ge is the class representative's graph entry*)
          case Metric.ord_graph (GE.get_graph cls_ge, GE.get_graph ge)
          (*The existing representative is smaller, so add this to the list of redexes.*)
          of LESS    => (cls |> EqClass.update_redexes (cons gn), eqt |> set_as_redex gn)
          (*The existing representative is bigger! Hence, ge should be the new representative.
          Flush the existing congruencies (they are all redexes now) and set this to be a lone
          representative.*)
           | GREATER =>
              let
                (*Congs + old rep*)
                val new_reds = EqClass.get_rep cls :: EqClass.get_congs cls
              in (cls |> EqClass.update_redexes (fn reds => new_reds @ reds) (*All redexes now...Muhahahaha*)
                      |> EqClass.set_congs []
                      |> EqClass.set_rep gn, (*ge is the new representative! gn is its name, which is what equivalence classes deal in*)
                  fold set_as_redex new_reds eqt)
              end
              (*Congs contains equivalent graphs*)
           | EQUAL   => (cls |> EqClass.update_congs (cons gn), eqt)
        )
        (*Not in this class*)
        else NONE
      end
      (*Try add... fold?*)
      fun tryaddf cls (found, rest, eqt) =
        if found then (true, cls :: rest, eqt)
        else case tryadd_to_class cls eqt (*Try add to the supplied class*)
               (*Success - it belonged to this class*)
               of SOME (cls', eqt') => (true, cls' :: rest, eqt')
               (*Failure. Try again...*)
                | NONE => (false, cls :: rest, eqt)
      (*Try adding the graph to every class in the class list*)
      val (found, classlist', eqt') = fold tryaddf classlist (false, [], eqt')
    in
      (*The new equivalence class table!*)
      SOME (add_new_graph ( (*Ensure this graph is marked as new!*)
        eqt' |> update_tab (ClassIndex.update (arity,
                  if found then classlist' (*classlist' will have the graph already added*)
                  (*Forms a new graph! Make gn the representative, and add it to the list of classes*)
                  else EqClass.mk gn :: classlist'))) gn
      )
    end
  (*Arity works like hashcode - we know only graphs with equivalent arity could belong to the same class*)
  
  (*Add multiple graph entries at once
  Note: eqt = equivalence table
  
  See update one for details. It does all the work. The fold
  just remembers the equivalence table as it is formed.
  The final boolean flag only indicates whether or not this is a hard update*)
  fun update' ge_list eqtab hard = let
    fun updatef ge (ges, eqt) = (*update fold*)
      case update_one' ge eqt hard
        (*This was added to the equivalence class table, so use the new table and add this graph to the list of added graphs*)
        of SOME eqt' => (ge::ges, eqt')
        (*Not added, so forget this graph entry*)
         | NONE      => (ges, eqt)
  (*Add them all. [] says no graphs have been successfully added yet.*)
  in fold updatef ge_list ([],eqtab)
  end
  
  fun update ge_list eqtab = update' ge_list eqtab false
  fun update_one ge eqt = update_one' ge eqt false
  fun hard_update ge_list eqtab = update' ge_list eqtab true
  fun hard_update_one ge eqt = update_one' ge eqt true
  
  (*
  Make a new equivalence class table, which will include in all of the reducible expressions from the given rule set,
  and will set the graph table to include the supplied names.
  *)
  fun mk rs = let
    (*Get all of the rules derived from reducible expressions in the rule set*)
    val initial_redexes = Ruleset.get_rules_in_tag rs (TagName.mk "redex")
    (*Add a rule, given its name (rn) to the equivalence class table*)
    fun add_rule rn eqt = let
      (*Supplies no equivlance data I guess?
      Surely that would cause problems, since then no graphs would appear equivalent...*)
      val ge = GE.mk' ((Theory.Rule.get_lhs (Ruleset.get_rule rs rn), NONE), default_data)
      (*Add the reducible graph to the graph entry table.
      gn forms a new name for this graph entry*)
      val (gn, tab) = (get_graph_tab eqt) |> GraphName.NTab.add (GraphName.mk "r0", ge)
      (*Make it a reducible expression and update the graph table and DNET!*)
    in eqt |> set_graph_tab tab |> set_as_redex gn
    end
    (*Add everything*)
  in R.NSet.fold add_rule initial_redexes (empty |> set_initial_rs rs)
  end
  
  (*Declare all of the new graphs as officially old*)
  fun clean eqt = set_new_graphs GraphName.NSet.empty eqt
  
  (*Merge two equivalence class tables together!*)
  fun merge eqt1 eqt2 = let
    (*Firstly, retrieve all of the new graphs from table 1*)
    val new_gns = GraphName.NSet.list_of (get_new_graphs eqt1);
    (*Retrieve the actual graph entries...*)
    val new_ges = map (fn gn => get_graph_entry eqt1 gn) new_gns;
    (*Now pass them all through the DNET of the second table, against the new graphs only*)
    (*It is possible here to form an equivalence class table with only the old graphs, and then check bidirectionally the DNETS
    in both of the old equivalence class tables if that is necessary. However, it isn't clear to me how to remove a graph
    from the equivalence class table though...*)
    val new_ges_through = new_ges(*filter (fn ge => not (has_match_in_dnet_with_new eqt2 ge)) new_ges;*)
  in
    (*Now perform a hard add with all of the new graphs from the first table...*)
    hard_update new_ges_through eqt2
  end
end









