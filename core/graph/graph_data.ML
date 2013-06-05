signature GRAPH_DATA =
sig
  type psubst  (* partial substitutions *)
  type subst   (* complete (aka solved) substitution *)
  type nvdata  (* node-vertex data *)
  type edata   (* edge data *)

  (* A partial substitution contains a collection of constraints on pattern
   * variables. The match_xxx functions update a psubst or return NONE if
   * no match is possible. *)
  val empty_psubst : psubst
  val match_node_vertex : nvdata * nvdata -> psubst -> psubst option
  val match_edge : edata * edata -> psubst -> psubst option

  (* For cases where node/edge data cannot be unified greedily (or uniquely),
   * this is called just before the match is finalised. One match will be
   * produced for each subst returned. In the case where this hook isn't
   * needed, like type subst = psubst, and "solve_psubst = Seq.single". *)
  val solve_psubst : psubst -> subst Seq.seq

  (* Apply substitutions to data. *)
  val subst_in_node_vertex : subst -> nvdata -> nvdata
  val subst_in_edge : subst -> edata -> edata
end