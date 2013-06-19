signature GRAPH_DATA =
sig
  type psubst  (* partial substitutions *)
  type subst   (* complete (aka solved) substitution *)
  type nvdata  (* node-vertex data *)
  type edata   (* edge data *)

  val pretty_theory_name : Pretty.T

  (* A partial substitution contains a collection of constraints on pattern
   * variables. It is initialised by passing in a table containing node/edge
   * data from the target graph. This can be used e.g. to ensure that names
   * introduced in the pattern are fresh. *)
  val init_psubst_from_data : nvdata VTab.T * edata ETab.T -> psubst

  (* The match_xxx functions update a psubst or return NONE if
   * no match is possible. *)
  val match_nvdata : nvdata * nvdata -> psubst -> psubst option
  val match_edata : edata * edata -> psubst -> psubst option

  (* equality for data. used for subgraph and graph equality *)
  val nvdata_eq : nvdata * nvdata -> bool
  val edata_eq : edata * edata -> bool

  (* default vertex and edge data *)
  val default_nvdata : nvdata
  val default_edata : edata

  val pretty_nvdata : nvdata -> Pretty.T
  val pretty_edata : edata -> Pretty.T

  (* For cases where node/edge data cannot be unified greedily (or uniquely),
   * this is called just before the match is finalised. One match will be
   * produced for each subst returned. In the case where this hook isn't
   * needed, let "type subst = psubst", and "solve_psubst = Seq.single". *)
  val solve_psubst : psubst -> subst Seq.seq

  (* Apply substitutions to data. *)
  val subst_in_nvdata : subst -> nvdata -> nvdata
  val subst_in_edata : subst -> edata -> edata
end