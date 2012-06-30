(* For a given graph, explore the rewrite graph using a BFS and following a
 * given metric.
 * Since the whole process can be very time consuming we want to update the user
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

    val init : G.T -> Ruleset.T -> T
    
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
  structure M = MetricsController(structure Graph = G);

  datatype T = RewriteTree of {
    rset : Ruleset.T,
    rgraph : RewriteGraph.T,
    untried_rules : R.name list,
    agenda : GraphName.name list,
    current_vertex : GraphName.name option,
    mc : M.T
  }

  fun init root ruleset =
    let
      val (rn, rg) = RewriteGraph.init |> RewriteGraph.add_vertex root
    in
      RewriteTree {
        rset = ruleset,
        rgraph = rg,
        untried_rules = (R.NSet.list_of o Ruleset.get_active) ruleset,
        agenda = [],
        current_vertex = SOME rn,
        mc = M.init
      }
    end;
end;

