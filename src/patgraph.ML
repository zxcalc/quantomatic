(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Pattern Graphs (allows var_nodes and elipses edges) *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* *)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature PATTERN_GRAPH = 
sig

structure PatNE : PATTERN_NODES_AND_EDGES;
structure BGraph : BGRAPH sharing BGraph.NE = PatNE;
structure Match : MATCH sharing Match.NE = PatNE;

include BASIC_GRAPH sharing NE = PatNE;

val get_bgraph : T -> BGraph.T
val get_vars : T -> NName.Col.T
val get_ctxt : T -> Match.ctxt (* global context, e.g. node data metavars *)

(* a matching between var-nodes also with instantiations for data vars in nodes *)
type match
val empty_match : match
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
structure Expr : GDATA_EXPR where type T = PatNE.BasicNE.node)
(* : PATTERN_GRAPH *)
= struct

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

val del_datavar = update_datavars o VName.NTab.delete;

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

(* Edge matching not Needed: one step later we check nodes and that
   enforced edge injection conditions needed for var-nodes/half-edges*)
(* instantiations *)
datatype match = Match of 
				 {ninsts: (NName.T * node) NName.NTab.T, (* pat to tgt *)
					invninsts : NName.T NName.NTab.T, (* tgt to pat *)
					(* instantiations for node data varaiables *)
					ndinsts: Expr.varexpr VName.NTab.T
				 };

val empty_match = Match {ninsts = NName.NTab.empty,
												 invninsts = NName.NTab.empty,
												 ndinsts = VName.NTab.empty
												};

fun update_match_ninsts f (Match rep) = 
		Match {ninsts = f (#ninsts rep), 
					 invninsts = #invninsts rep,
					 ndinsts = #ndinsts rep};
fun update_match_invninsts f (Match rep) = 
		Match {ninsts = #ninsts rep, 
					 invninsts = f (#invninsts rep),
					 ndinsts = #ndinsts rep};
fun update_match_ndinsts f (Match rep) = 
		Match {ninsts = #ninsts rep, 
					 invninsts = #invninsts rep,
					 ndinsts = f (#ndinsts rep)};

fun get_match_ninsts (Match rep) = #ninsts rep;
fun get_match_invninsts (Match rep) = #invninsts rep;
fun get_match_ndinsts (Match rep) = #ndinsts rep;
val set_match_ndinsts = update_match_ndinsts o K;


(* assumes patnd are uninstantiated *)
(* raises NName.NTab...DUP exception, if n1 is already insantiated *)
fun match_node (n1, patnd as NE.VarNode _) (nnd2 as (n2, NE.Node _)) m = 
		SOME (m |> update_match_ninsts (NName.NTab.ins (n1,nnd2)))
	| match_node (n1, patnd as NE.VarNode _) (nnd2 as (n2,NE.VarNode _)) m = 
		(case (NName.NTab.lookup (get_match_invninsts m) n2) of
			 NONE => SOME (m |> update_match_ninsts (NName.NTab.ins (n1,nnd2))
											 |> update_match_invninsts (NName.NTab.ins (n2,n1)))
		 | SOME _ => NONE) (* something else is matched to tgt nd already *)
	| match_node (n1, patnd as NE.Node nd1) (nnd2 as (n2, NE.Node nd2)) m = 
		(case (NName.NTab.lookup (get_match_invninsts m) n2) of
			 NONE => (* nothing is matched to target nd already *)
			 (case (Expr.match nd1 nd2 (get_match_ndinsts m)) 
				 of NONE => NONE (* nodes don't match *)
					| SOME ndinsts2 => 
						SOME (m |> set_match_ndinsts ndinsts2
										|> update_match_ninsts (NName.NTab.ins (n1,nnd2))
										|> update_match_invninsts (NName.NTab.ins (n2,n1))))
		 | SOME _ => NONE) (* something is matched to target nd already *)
	| match_node (_, NE.Node _) (_,NE.VarNode _) m = 
		NONE; (* instantiated node cannot match a varnode (half-edge) *)

(* FIXME: use an edge expression language and actually check edge
   data. At the moment we assume all edges are the same *) 
(* NOTE: matching ingores Edge direction, could easily be changed. *)
fun match_edgedata (e1, ed1) (e2,ed2) m = SOME m;

exception badly_formed_pattern_exp of string * T;


(* nds = future node-matching agenda, 
 (int,outt) = edges still left to be matched to, 
 m = match info so far *)
fun match_tgt_edge pat tgt ((e,ed),n') (nds,(int,outt),m) = 
		Seq.append 
			(Seq.maps (* or choices over tgt in-edges to match to *)
				 (fn te =>
						 let val (ted,(tn',_)) = get_edge tgt te in
							 case match_edgedata (e,ed) (te,ted) m
								of NONE => Seq.empty
								 | SOME m2 => 
									 Seq.single 
										 ((n',tn')::nds,
											(EName.NTab.N.delete te int, outt), 
											m2)
						 end)
				 (Seq.of_list (EName.NTab.N.dest int)))
			(Seq.maps (* or choices over tgt out-edges to match to *)
				 (fn te =>
						 let val (ted,(_,tn')) = get_edge tgt te 
						 in
							 case match_edgedata (e,ed) (te,ted) m
								of NONE => Seq.empty
								 | SOME m2 => 
									 Seq.single 
										 ((n',tn')::nds,
											(int, EName.NTab.N.delete te outt), 
											m2)
						 end)
				 (Seq.of_list (EName.NTab.N.dest outt)));


(* match against a variable node - var nodes can have extra edges to
   them in the tgt; *)
(* Match against non-half-edge node (varnode) - no extra edges into the tgt *)
fun match_varnode_edge pat tgt (n,(inn,outn)) (tn,(int,outt)) m =
		if EName.NTab.N.is_empty inn then 
			case EName.NTab.N.tryget_singleton outn of 
				NONE => raise badly_formed_pattern_exp ("match_varnode_edge:1",pat)
			| SOME e =>
				let val (ed,(_,n')) = get_edge pat e in
					match_tgt_edge pat tgt ((e,ed),n') ([],(int,outt),m)
				end
		else
			case EName.NTab.N.tryget_singleton inn of 
				NONE => raise badly_formed_pattern_exp ("match_varnode_edge:2",pat)
			| SOME e =>
				let val (ed,(n',_)) = get_edge pat e in
					match_tgt_edge pat tgt ((e,ed),n') ([],(int,outt),m)
				end;

(* matching edges of "n" to those of "tn" *)
fun match_edges_from_node pat tgt (n,(inn,outn)) (tn,(int,outt)) m =
		(Seq.single ([],(int,outt),m))
			|> (EName.NTab.N.fold (* for each edge going into n in pattern *)
						(fn e => (* match the pattern from-node to some target node *)
								let val (ed,(n',_)) = get_edge pat e in
									Seq.maps (match_tgt_edge pat tgt ((e,ed),n'))
								end)
						inn)
			|> (EName.NTab.N.fold (* for each edge going out of n in pattern *)
						(fn e => (* match the node to some target node *)
								let val (ed,(_,n')) = get_edge pat e in
									Seq.maps (match_tgt_edge pat tgt ((e,ed),n'))
								end)
						outn)
			|> (Seq.filter (* filter out case with extra edges in target *)
						(fn (nds,(ints,outts),m) => 
								EName.NTab.N.is_empty ints 
								andalso EName.NTab.N.is_empty outts));

(* assumes: pat_ns are all uninstantiated *)
fun match_tgt_node pat tgt (n,(nd,nedges)) ((pat_ns,tgt_ns),m) tn =
		let val (tnd,tedges) = get_node tgt tn in
			case match_node (n,nd) (tn,tnd) m of 
				NONE => Seq.empty (* node does not match *)
			| SOME m2 => (* continue matching *)
				(case nd 
					of (NE.VarNode _) => (* allow extra edges in tgt *)
						 (Seq.map 
								(fn m => 
										((NName.NTab.N.delete n pat_ns,
											case tnd 
											 of NE.VarNode _ => NName.NTab.N.delete tn tgt_ns
												| NE.Node _ => tgt_ns),m))
								(match_varnode_edge
									 pat tgt (n,nedges) (tn,tedges) m))
					 | (NE.Node _) => (* disallow extra edges in tgt *)
						 (Seq.map 
								(fn m => ((NName.NTab.N.delete n pat_ns,
													 NName.NTab.N.delete tn tgt_ns),m))
								(match_edges_from_node 
									 pat tgt (n,nedges) (tn,tedges) m)))
		end;

(* match from a list of node-pairs; nds = node pairs in agenda which
must be matched; pat_ns is the pattern node names still left to be
matched to something; tgt_ns are the target names left, to which some
node in the pattern might match; m is the match so far; *)
fun match' pat tgt [] ((pat_ns,tgt_ns),m) = 
		(* if no node pairs left, search over possibilities  *)
		(case (NName.NTab.N.local_bot_of pat_ns) of (* case empty then *)
			 NONE => Seq.single ((pat_ns,tgt_ns),m) (* finished matching! *)
		 | SOME n => (* case non empty *)
			(* pick a next pat node to match up against some tgt node *)
			 let val (nd,nedges) = get_node pat n
			 in
				 (Seq.of_list (NName.NTab.N.dest tgt_ns))
					 |> (Seq.maps (* try each tgt node: or-choices *)
								 (match_tgt_node pat tgt (n,(nd,nedges)) ((pat_ns,tgt_ns),m)))
					 |> (Seq.maps (* now match nodes we get two from an edge  *)
								 (fn ((pat_ns2,tgt_ns2),(nds,_,m2)) => 
										 (match' pat tgt nds ((pat_ns2,tgt_ns2),m2))))
			 end)
	| match' pat tgt ((n,tn)::nds) ((pat_ns,tgt_ns),m) = 
		(case (NName.NTab.lookup (get_match_ninsts m) n) of 
			 NONE => (* pattern is not yet matches to a tgt node *)
			 let val (pnd,pedges) = get_node pat n 
			 in 
				 (match_tgt_node pat tgt (n,(pnd,pedges)) ((pat_ns,tgt_ns),m) tn)
					 |> (Seq.maps (* now match nodes we get two from an edge  *)
								 (fn ((pat_ns2,tgt_ns2),(nds2,_,m2)) => 
										 (* continue breadth first: old nodes before new ones *)
										 (match' pat tgt nds ((pat_ns2,tgt_ns2),m2))
											 |> Seq.maps (match' pat tgt nds2)))
			 end
		 | SOME (n2,_) => (* pat node is already matched *)
			 if NName.eq n2 tn then 
				 (* already match to tn, so just continue*)
				 match' pat tgt nds ((pat_ns,tgt_ns),m)
			 else Seq.empty (* matched to something else, so fail *)
		); 

fun match pat tgt nds m = 
		let 
			val pat_ns = NName.NTab.get_names (get_nodes pat)
			val tgt_ns = NName.NTab.get_names (get_nodes tgt)
		in
			match' pat tgt nds ((pat_ns,tgt_ns),m)
		end;

exception var_node_exp of string;

fun instantiate_node_dvars ndinsts (nd as NE.VarNode _) = 
		raise var_node_exp "instantiate_node_dvars"
	| instantiate_node_dvars ndinsts (NE.Node nd2) = 
		NE.Node (Expr.subs ndinsts nd2);

fun update_node_and_adddeps f n g = 
		let 
			val (nd,_) = (get_node g n)
			val nd2 = f nd;
		in 
			case nd2 of NE.VarNode _ => set_node nd2 n g
							 | NE.Node ndata => 
								 g |> set_node nd2 n
									 |> add_datavars_dep (Expr.get_vars ndata) n
		end;

(* Note: instantiation deals with adding/removing variable
   dependencies tracking information *)
fun instantiate m g = 
		let 
			val dvars = get_datavars g
			val dinst = get_match_ndinsts m
			(* create all node names to instantiate *)
			val update_ns = VName.NTab.N.fold 
											 (fn v => fn ns => 
											  NName.NTab.N.union_merge (VName.NTab.get dvars v) ns)
											 (VName.NTab.get_names dinst)
											 NName.NTab.N.empty;
		in
			g |> (NName.NTab.fold (fn (n,(n2,nd2)) => fn g => 
																g |> rename_nname n n2
																	|> set_node nd2 n2
																	|> del_var n
														)
														(get_match_ninsts m))
			(* 	|> (NName.NTab.N.fold (update_node_and_adddeps 
																 (instantiate_node_dvars dinst))
															update_ns) *)
				|> VName.NTab.fold (del_datavar o fst) dinst
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
					(NName.NTab.dest (get_match_ninsts m)))
	 @ [Pretty.block [], (* empty line *)
			Pretty.str "Node Data Vars instantiations:"]
	 @ (map (fn (n,varexp) => 
							Pretty.block 
								[Pretty.str "VName: ", 
								 VName.pretty n,
								 Pretty.str " => ",
								 Expr.pretty_varexp varexp])
					(VName.NTab.dest (get_match_ndinsts m))));

val print_match = Pretty.writeln o pretty_match;

end;


structure DB_PatStrGraph = PatGraphFun(
  structure PatNE = PatStrNE
  structure Expr = StrExpr);
structure PatStrGraph :> PATTERN_GRAPH where 
type PatNE.node_pat = PatStrNE.node_pat 
 and type PatNE.edge_pat = PatStrNE.edge_pat 
= DB_PatStrGraph;
