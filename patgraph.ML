(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Pattern Graphs (allows var_nodes and elipses edges) *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* *)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature PATTERN_GRAPH = 
sig

structure PatNE : PATTERN_NODES_AND_EDGES;

structure BGraph : BASIC_GRAPH sharing BGraph.NE = PatNE;

(* expression type for non-graphvar case of nodes *)
structure Expr : GDATA_EXPR where type T = PatNE.BasicNE.node;

(* where 
   type NE.node = PatNE.node
   and type NE.edge = PatNE.edge;
*)

include BASIC_GRAPH sharing NE = PatNE;

(* where
   type NE.node = PatNE.node
   and type NE.edge = PatNE.edge;
*)

val get_bgraph : T -> BGraph.T
val get_vars : T -> NName.Col.T
val get_datavars : T -> NName.NTab.N.T VName.NTab.T

(* a matching between var-nodes also with instantiations for data vars in nodes *)
type match
val match_node : (NName.T * node) ->  (NName.T * node) 
								 -> match -> match option
val instantiate_node_dvars : Expr.varexpr VName.NTab.T -> node -> node

(* instantiate a graph with some match *)
val instantiate : match -> T -> T

val pretty_match : match -> Pretty.T
val print_match : match -> unit

(* internal tools -- dangerous to use outside! *)
val update_bgraph  : (BGraph.T -> BGraph.T) -> T -> T
val update_vars  : (NName.Col.T -> NName.Col.T) -> T -> T

val set_bgraph : BGraph.T -> T -> T
val set_vars : NName.Col.T -> T -> T

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Quantum Graphs *)
functor PatGraphFun(
structure PatNE : PATTERN_NODES_AND_EDGES
structure Expr : GDATA_EXPR where type T = PatNE.BasicNE.node
) = 
struct

structure PatNE = PatNE;
structure NE = PatNE;
structure Expr = Expr;

structure BGraph = BasicGraphFun(NE);

type ename = EName.T;
type nname = NName.T;

type node = NE.node;
type edge = NE.edge;

(* a graph *)
datatype T = PatGraph of 
         {bgraph : BGraph.T, (* the basic graph *)
          (* the variable nodes - graph level pattern vars *)
          vars : NName.Col.T,
          (* map of data variables to the nodes they occur in *)
					datavars : NName.NTab.N.T VName.NTab.T
				 };

exception no_such_nname_exp of string * nname * T;
exception nname_already_exists_exp of nname * T;

exception no_such_ename_exp of string * ename * T;
exception ename_already_exists_exp of ename * T;

val empty = PatGraph {bgraph = BGraph.empty, vars = NName.Col.empty,
											datavars = VName.NTab.empty };

fun update_bgraph f (PatGraph rep) = 
    PatGraph { bgraph = f (#bgraph rep), 
               vars = #vars rep,
							 datavars = #datavars rep
						 }; 
fun update_vars f (PatGraph rep) = 
    PatGraph { bgraph = #bgraph rep, 
               vars = f (#vars rep),
							 datavars = #datavars rep
						 };
fun update_datavars f (PatGraph rep) = 
    PatGraph { bgraph = #bgraph rep, 
               vars = #vars rep,
							 datavars = f (#datavars rep)
						 };

val set_bgraph = update_bgraph o K;
val set_vars = update_vars o K;
val set_datavars = update_datavars o K;

val add_var = update_vars o NName.Col.union1;
val del_var = update_vars o NName.Col.delete;
val del_vars = update_vars o NName.Col.minus_set;

(* implicit arg: graph *)
fun add_datavars_dep vs n = 
		update_datavars 
			(VName.NTab.N.fold (fn v => fn dvars => 
								VName.NTab.map_entry (NName.NTab.N.add n) v dvars
						) vs);

fun del_datavars_dep vs n = 
		update_datavars 
			(VName.NTab.N.fold (fn v => fn dvars => 
								VName.NTab.map_entry (NName.NTab.N.delete n) v dvars
						) vs);

fun get_bgraph (PatGraph rep) = #bgraph rep;
fun get_vars (PatGraph rep) = #vars rep;
fun get_datavars (PatGraph rep) = #datavars rep;
val delete_datavar = update_datavars o VName.NTab.delete;

val get_nodes = BGraph.get_nodes o get_bgraph;
val get_edges = BGraph.get_edges o get_bgraph;
val get_node_list = NName.NTab.dest o get_nodes;
val get_edge_list = EName.NTab.dest o get_edges;

(* implicit args: q n *)
val get_node = BGraph.get_node o get_bgraph;
val get_edge = BGraph.get_edge o get_bgraph;
val lookup_node = BGraph.lookup_node o get_bgraph;
val lookup_edge = BGraph.lookup_edge o get_bgraph;

(* adding a node gives back new unique name for it and the new graph *)
fun add_named_node n (nd as NE.VarNode in_or_out) g =
    let val (n2,g2) = BGraph.add_named_node n nd (get_bgraph g)
    in (n2, g |> add_var n
              |> set_bgraph g2)
    end
  | add_named_node n (nd as NE.Node ndata) g =
    let val (n2,bg2) = BGraph.add_named_node n nd (get_bgraph g)
    in (n2, g |> set_bgraph bg2
							|> add_datavars_dep (Expr.get_vars ndata) n) 
		end;

(* new can raise an exception if name is not unique, returns new graph *)
fun new_named_node n (nd as NE.VarNode in_or_out) g =
    let val bg2 = BGraph.new_named_node n nd (get_bgraph g)
    in g |> add_var n
         |> set_bgraph bg2 end
  | new_named_node n (nd as NE.Node ndata) g =
    let val bg2 = BGraph.new_named_node n nd (get_bgraph g)
    in g |> set_bgraph bg2 
				 |> add_datavars_dep (Expr.get_vars ndata) n 
		end;

(* implicit args: nd q *)
val add_node = add_named_node NE.default_nname;
val doadd_node = snd oo add_node;

(* implicit args: en e n1 n2 q *)
fun add_named_edge en e n1 n2 g = 
    let val (en2,bg2) = BGraph.add_named_edge en e n1 n2 (get_bgraph g) 
    in (en2, set_bgraph bg2 g) end;
fun new_named_edge en e n1 n2 g = 
    let val bg2 = BGraph.new_named_edge en e n1 n2 (get_bgraph g) 
    in set_bgraph bg2 g end;

(* implicit args: e n1 n2 q *)
val add_edge = add_named_edge NE.default_ename;
val doadd_edge  = snd oooo add_edge;

fun delete_node_vardeps n g = 
		(case (get_node g n) 
      of (NE.VarNode _,_) => del_var n g
       | (NE.Node nd,_) => 
				 del_datavars_dep (Expr.get_vars nd) n g)

(* deleting node also removes all it's edges. *)
fun delete_node n g = 
    g |> delete_node_vardeps n
      |> update_bgraph (BGraph.delete_node n);

(* implicit args: en q *)
val delete_edge = update_bgraph o BGraph.delete_edge;

(* nname = suggested name *)
(* rename all occurances of old name (1st arg) with new name (2nd arg) 
   n2 must not exist already, n1 must exist *)
(* THINK: I think there is a slightly more efficient way to treat datavars *)
fun rename_nname n1 n2 g = 
    (case (get_node g n1)
      of (NE.VarNode _,_) => 
         g |> update_vars (NName.Col.delete n1 o NName.Col.union1 n2)
           |> update_bgraph (BGraph.rename_nname n1 n2)
       | (NE.Node nd,_) => 
				 g |> update_bgraph (BGraph.rename_nname n1 n2)
					 |> del_datavars_dep (Expr.get_vars nd) n1
					 |> add_datavars_dep (Expr.get_vars nd) n2
		);

(* val implicit args: en1 en2 q *)
val rename_ename = update_bgraph oo BGraph.rename_ename;
(* implicit arg: f n q *)
val update_node = update_bgraph oo BGraph.update_node;
(* implicit arg: f en q *)
val update_edge = update_bgraph oo BGraph.update_edge;
(* implicit arg: nd n q *)
val set_node = update_bgraph oo BGraph.set_node;
(* implicit arg: en e q *)
val set_edge = update_bgraph oo BGraph.set_edge;

(* useup names... *)
val useup_nnames = update_bgraph o BGraph.useup_nnames;
val useup_enames = update_bgraph o BGraph.useup_enames;

(* implicit args: g1 g2: rename g2 to avoid names in g1 *)
val rename_apart = 
		update_bgraph o (BGraph.rename_apart o get_bgraph);

datatype match = Match of 
				 {(* instantiations for graph vars *)
					ginsts: (NName.T * node) NName.NTab.T, 
					(* instantiations for node data varaiables *)
					dinsts: Expr.varexpr VName.NTab.T
				 };

fun update_match_ginsts f (Match rep) = 
		Match {ginsts = f (#ginsts rep), dinsts = #dinsts rep};

fun update_match_dinsts f (Match rep) = 
		Match {ginsts = #ginsts rep, dinsts = f (#dinsts rep)};

fun get_match_ginsts (Match rep) = #ginsts rep;
fun get_match_dinsts (Match rep) = #dinsts rep;
val set_match_dinsts = update_match_dinsts o K;



(* raises NName.NTab...DUP exception, if n1 is already insantiated *)
fun insnew_match_ginst n1 nnd2 = 
		update_match_ginsts (NName.NTab.ins (n1,nnd2));

(* you are not supposed to have vars in the target to be matched... *)
exception vars_in_match_target_exp of string;

(* FIXME: matching can create new names! *)

fun match_node (n1, patnd as NE.VarNode _) nnd2 m = 
		SOME (insnew_match_ginst n1 nnd2 m)
	| match_node _ (_, NE.VarNode _) m = 
		raise vars_in_match_target_exp "match_node"
	| match_node (n1, patnd as NE.Node nd1) (n2, NE.Node nd2) m = 
		(case (Expr.match nd1 nd2 (get_match_dinsts m)) 
			of NONE => NONE
			 | SOME dinsts2 => SOME (set_match_dinsts dinsts2 m));

exception var_node_exp of string;

fun instantiate_node_dvars dinsts (nd as NE.VarNode _) = 
		raise var_node_exp "instantiate_node_dvars"
	| instantiate_node_dvars dinsts (NE.Node nd2) = 
		NE.Node (Expr.subs dinsts nd2);

fun instantiate m g = 
		let 
			val dvars = get_datavars g
			val dinst = get_match_dinsts m
					(* create all node names to instantiate *)
			val update_ns = VName.NTab.N.fold 
											 (fn v => fn ns => 
											  NName.NTab.N.union_merge (VName.NTab.get dvars v) ns)
											 (VName.NTab.get_names dinst)
											 NName.NTab.N.empty;
		in
			g |> (NName.NTab.fold (fn (n,(n2,nd2)) => fn g => 
																g |> rename_nname n n2
																	|> set_node n2 nd2
														
														) (get_match_ginsts m))
				|> (NName.NTab.N.fold (update_node (instantiate_node_dvars dinst))
															update_ns)
		end;

(* pretty printing *)
val pretty_node = NE.pretty_node;
val pretty_edge = NE.pretty_edge;
val print_node = Pretty.writeln o pretty_node;
val print_edge = Pretty.writeln o pretty_edge;

fun pretty g = 
    Pretty.chunks 
    [ Pretty.str "PatGraph{",
      Pretty.block 
        [Pretty.str "  ", (* indent *)
         Pretty.chunks 
           [Pretty.block [Pretty.str "VarNodes: ", 
                          Pretty.list "{" "}" 
                            (map NName.pretty (NName.Col.dest (get_vars g)))],
						Pretty.block [Pretty.str "VarNodes: ", 
                          Pretty.list "{" "}" 
										      	(map NName.pretty (NName.Col.dest (get_vars g)))],
            BGraph.pretty (get_bgraph g)]],
      Pretty.str "}"
    ];

val print = Pretty.writeln o pretty;



fun pretty_match m = 
Pretty.chunks 
	([Pretty.str "Graph Var-Node instantiations:"]
	 @ (map (fn (n,(n2,nd2)) => 
							Pretty.block 
								[Pretty.str "NName: ", 
								 NName.pretty n,
								 Pretty.str " => ",
								 NName.pretty n,
								 Pretty.str " : ",
								 pretty_node nd2])
					(NName.NTab.dest (get_match_ginsts m)))
	 @ [Pretty.block [], (* empty line *)
			Pretty.str "Node Data Vars instantiations:"]
	 @ (map (fn (n,varexp) => 
							Pretty.block 
								[Pretty.str "VName: ", 
								 VName.pretty n,
								 Prett.str " => ",
								 Expr.pretty_varexp varexp])
					(NName.NTab.dest (get_match_ginsts m))));


val print_match = Pretty.writeln o pretty_match;


end;


structure DB_PatStrGraph = PatGraphFun(
  structure PatNE = PatStrNE
  structure Expr = StrExpr);
structure PatStrGraph :> PATTERN_GRAPH where 
type PatNE.node_pat = PatStrNE.node_pat 
 and type PatNE.edge_pat = PatStrNE.edge_pat 
= DB_PatStrGraph;
