(**
  * Store a single rewrite step in a verifiable format.
  *
  * 
  *)

signature REWRITE_SHARING =
sig
  type T
  structure Graph : OGRAPH_SHARING
  structure GraphIso : BANG_GRAPH_ISO_SHARING
  structure Rule : OGRAPH_RULE_SHARING
  sharing Rule.Graph = GraphIso.Graph = Graph
end

signature REWRITE =
sig
  type T
  structure Graph : BANG_GRAPH
  structure Rule : BANG_GRAPH_RULE
  structure GraphIso : BANG_GRAPH_ISO
  structure Sharing : REWRITE_SHARING
  sharing type T = Sharing.T
  sharing Graph.Sharing = Sharing.Graph
  sharing GraphIso.Sharing = Sharing.GraphIso
  sharing Rule.Sharing = Sharing.Rule
end

functor Rewrite(
  structure GraphIso : BANG_GRAPH_ISO
  structure Rule : BANG_GRAPH_RULE
  sharing GraphIso.Sharing.Graph = Rule.Sharing.Graph
) : REWRITE =
struct
  structure GraphIso = GraphIso
  structure Rule  = Rule
  structure Graph = GraphIso.Graph

  datatype T = RW of {
    rule : Rule.T,
    subst : Graph.subst,
    bbox_ops : bbox_op list,
    lhs_inst : GraphIso.T,
    rhs_inst : GraphIso.T
  }

  

  structure Sharing =
  struct
    type T = T
    structure Graph = Graph.Sharing
    structure GraphIso = GraphIso.Sharing
    structure Rule = Rule.Sharing
  end
end
