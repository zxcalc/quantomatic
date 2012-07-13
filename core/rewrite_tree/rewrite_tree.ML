(* For a given graph, explore the rewrite graph using a DFS and following a
 * given metric.
 * Since the whole process can be very time consuming we want to give updates
 * as often as possible : keep a list untried rules, keep a list of unvisited
 * vertices and update the rewrite graph.
 *)

signature REWRITE_TREE =
  sig
    structure GTheory : GRAPHICAL_THEORY
    structure Ruleset : RULESET
    structure Rule : RULE
    structure RSetRewrites : RULESET_REWRITES
    structure RewriteGraph : RGRAPH
    structure G : BANG_GRAPH
    structure M : METRICS

    type T

    val init : G.T -> Ruleset.T -> T;
    val next : T -> (bool * T);

    val get_rgraph : T -> RewriteGraph.T;
    val get_agenda : T -> GraphName.name list;
    val get_mc : T -> M.T;

    val set_rset : Ruleset.T -> T -> T;
    val set_mc : M.T -> T -> T;
  end;

functor RewriteTree(
  GTheory : GRAPHICAL_THEORY
) : REWRITE_TREE = struct
  
  structure GTheory = GTheory;
  structure Ruleset = GTheory.Ruleset;
  structure Rule = GTheory.Rule;
  structure RSetRewrites = GTheory.RulesetRewrites;
  structure RewriteGraph = RGraph(structure RSetRewrites = RSetRewrites);
  structure G = Rule.Graph;
  structure Match = RSetRewrites.Match;
  structure GraphSubst = GraphSubstFun(Match);
  structure M = MetricsController(structure Graph = G);

  datatype T = RewriteTree of {
    (* Ruleset, only active rules are used *)
    rset : Ruleset.T,
    (* Holds the rewrite graph: Graphs as nodes, rewrites as edges *)
    rgraph : RewriteGraph.T,
    (* List used to know where we are in the exploration of the rewrites
     * of a single graph. *)
    untried_rules : R.name list,
    (* Nested lists of graph names: Agenda uses the metric to assign priorities *)
    agenda : GraphName.name list,
    (* When a vertex is 'current' it is not in the agenda anymore but we may
     * have more work to do on it as there may still be unexplored rules *)
    current_vertex : GraphName.name option,
    (* Metrics *)
    mc : M.T
  }

  fun update_rset f (RewriteTree rt) =
    RewriteTree{rset = f (#rset rt), rgraph = #rgraph rt, untried_rules =
      #untried_rules rt, agenda = #agenda rt, current_vertex = #current_vertex
        rt, mc = #mc rt} 
  fun update_rgraph f (RewriteTree rt) =
    RewriteTree{rset = #rset rt, rgraph = f (#rgraph rt), untried_rules =
      #untried_rules rt, agenda = #agenda rt, current_vertex = #current_vertex
        rt, mc = #mc rt} 
  fun update_untried_rules f (RewriteTree rt) =
    RewriteTree{rset = #rset rt, rgraph = #rgraph rt, untried_rules =
      f (#untried_rules rt), agenda = #agenda rt, current_vertex = #current_vertex
        rt, mc = #mc rt} 
  fun update_agenda f (RewriteTree rt) =
    RewriteTree{rset = #rset rt, rgraph = #rgraph rt, untried_rules =
      #untried_rules rt, agenda = f (#agenda rt), current_vertex = #current_vertex
        rt, mc = #mc rt} 
  fun update_current_vertex f (RewriteTree rt) =
    RewriteTree{rset = #rset rt, rgraph = #rgraph rt, untried_rules =
      #untried_rules rt, agenda = #agenda rt, current_vertex = f (#current_vertex
        rt), mc = #mc rt} 
  fun update_mc f (RewriteTree rt) =
    RewriteTree{rset = #rset rt, rgraph = #rgraph rt, untried_rules =
      #untried_rules rt, agenda = #agenda rt, current_vertex = #current_vertex
        rt, mc = f (#mc rt)} 

  val set_rset = update_rset o K;
  val set_rgraph = update_rgraph o K;
  val set_untried_rules = update_untried_rules o K;
  val set_agenda = update_agenda o K;
  val set_current_vertex = update_current_vertex o K;
  val set_mc = update_mc o K;

  fun get_rset (RewriteTree {rset = x, ...}) = x;
  fun get_rgraph (RewriteTree {rgraph = x, ...}) = x;
  fun get_untried_rules (RewriteTree {untried_rules = x, ...}) = x;
  fun get_agenda (RewriteTree {agenda = x, ...}) = x;
  fun get_current_vertex (RewriteTree {current_vertex = x, ...}) = x;
  fun get_mc (RewriteTree {mc = x, ...}) = x;

  (* Init the RTree (which may actually be a graph, once unified) with the root 
   * and the ruleset.*)
  fun init root ruleset =
    let
      val (rn, rg) = RewriteGraph.init |> RewriteGraph.add_vertex root
      val rg' = rg |> RewriteGraph.set_root (SOME rn)
    in
      RewriteTree {
        rset = ruleset,
        (* Rewrite graph has only vertex and no edges *)
        rgraph = rg',
        (* Only the active rules are taken into account *)
        untried_rules = (R.NSet.list_of o Ruleset.get_active) ruleset,
        (* Empty agenda because the current vertex has not been explored yet*)
        agenda = [],
        (* Current vertex is the root *)
        current_vertex = SOME rn,
        mc = M.init
      }
    end;


  (* Helpers *)

  fun flip f x y = f y x

  fun pull_agenda rt = 
    let
      val agenda = get_agenda rt
      fun get_next_vertex a = SOME (List.hd a)
    in
      if List.null agenda then
        rt |> set_current_vertex NONE 
      else
        rt |> set_current_vertex (get_next_vertex agenda)
           |> set_agenda (List.tl agenda)
    end;

  fun explore_rewrites rulename graphname rt =
    let
      (* The agenda is built according to some metric*)
      fun compare g1 (_,g2) = 
        case (M.get_current_ord_graph (get_mc rt)) (g1,g2) of
          SOME GREATER => false
          | SOME LESS  => true
          | SOME EQUAL => true
          | NONE => true 
      (* Get the ruleset because we need the rule *)
      val ruleset = get_rset rt
      (* Get the actual rule *)
      val rule = Ruleset.get_rule ruleset rulename
      (* Get the graph we want to rewrite *)
      val g = graphname |> RewriteGraph.get_vertex (get_rgraph rt)
                        |> fst
      (* Get the sequence of matches for that rule on that graph *)
      val (r',matches_seq) = (RSetRewrites.rule_matches rule g)
      val matches = Seq.list_of matches_seq 
      (* Take a match, rewrite it and insert it in the list according to the
       * metric *)
      fun rewrite (m,rws) =
        let
          fun insert (m,rwg) [] = [(m,rwg)]
            | insert (m,rwg) xs = 
                let
                  val (b,a) = List.partition (compare rwg) xs 
                in
                  List.concat [b, [(m,rwg)], a] 
                end 
        in 
          insert 
            (m, GraphSubst.rewrite g (Rule.get_lhs r') m (Rule.get_rhs r')) rws
        end

      (* Get the list of pairs (match, rewritten graph) *)
      fun add_to_rgraph_and_update_agenda rws =
        let
          (* rg is obtained from the 'local' var. rt which did not change so far *)
          val rg = get_rgraph rt
          (* the agenda that is going to be updated recursively *)
          val agenda = get_agenda rt
          (* get the agenda, a pair and the rewrite graph *)
          fun add_to_rgraph (m,rwg) (a,rgraph) =  
            let
              (* Add the graph to the rewrite graph *)
              val (name, rgraph') = rgraph |> RewriteGraph.add_vertex rwg
              (* Add an edge from the parent to it's rewritten graph *)
              val rgraph'' = rgraph' |> RewriteGraph.add_edge "other"
                                  ((graphname, name), (rulename, (r',m)))
                                     |> snd
                                     (* Unify1 checks if we have already
                                      * seen this graph before: avoid loops *)
                                     |> RewriteGraph.unify1 name
            in
              (* If the graph has not been unified, then it's name is still
               * in the rewrite graph and we need to add it to the agenda *)
              if RewriteGraph.has_vertex name rgraph'' then
                ((name::a),rgraph'')
              else
                (* This graph is just like something we've already discovered or
                 * explored : no need to add it *)
                (a, rgraph'')
            end
          (* Add vertices to the rewrite graph *)  
          val (a',rg') = fold (add_to_rgraph) rws (agenda,rg)
        in
          rt |> set_rgraph rg'
             |> set_agenda a'
        end
    in
      (* If no match then NONE -> the "next" fct will try the next active rule of the
       * rset *)
      if List.null matches then
        NONE
      else
        matches (* Rewrite the graph using the matches *)
                (* From a list of matches to a list of (match, rewritten graph) *)
                (* Sort that list according to the metric : greatest on the
                 * left *)
                |> List.foldl rewrite []
                (* Add the rewritten graphs to rgraph, add the edges, unify to
                 * avoid loops and modify the agenda accordingly *)
                |> add_to_rgraph_and_update_agenda
                |> SOME
    end;

  (* Core function: call it to get a tree with at least one more leaf. 
   * Returns (true,_) if there are still rewrites and (false,_) if we are done,
   * i.e. current_vertex = NONE*)
  fun next rt =
    let
      fun next' cv =
        let
          val ur = get_untried_rules rt
          val rset = get_rset rt
        in
          if List.null ur then
            rt |> set_untried_rules ((R.NSet.list_of o Ruleset.get_active) rset)
               |> pull_agenda
               |> next
          else
            case (explore_rewrites (List.hd ur) cv rt) of
              SOME rt => 
                (true, rt |> set_untried_rules (List.tl ur))
              | NONE =>
                rt |> set_untried_rules (List.tl ur)
                   |> next
        end 
      fun treeify rtree =
        let
          val rg = get_rgraph rtree
        in
          case RewriteGraph.get_root rg of
            SOME x => rtree |> set_rgraph (RewriteGraph.treeify x rg)
            | NONE => rtree
        end
    in
      case get_current_vertex rt of
        SOME cv => next' cv
        | NONE => (false, treeify rt)
    end;
end;

