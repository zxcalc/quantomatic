(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Representation for Pattern Graphs (allows var_nodes and elipses edges) *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* *)

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature PATTERN_GRAPH = 
sig

structure PatNE : PATTERN_NODES_AND_EDGES;

structure BGraph : BASIC_GRAPH sharing BGraph.NE = PatNE;

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
val get_datavars : T -> NName.Col.T VName.NTab.T
val get_datavars_at_node : T -> NName.T -> NName.Col.T

val instantiate : Expr.T VName.NTab.T -> T -> T

(* internal tools -- dangerous to use outside! *)
val update_bgraph  : (BGraph.T -> BGraph.T) -> T -> T
val update_vars  : (NName.Col.T -> NName.Col.T) -> T -> T

val set_bgraph : BGraph.T -> T -> T
val set_vars : NName.Col.T -> T -> T

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Quantum Graphs *)
functor PatGraphFun(PatNE : PATTERN_NODES_AND_EDGES) = 
struct

structure PatNE = PatNE;
structure NE = PatNE;

structure BGraph = BasicGraphFun(NE);

type ename = EName.T;
type nname = NName.T;

type node = NE.node;
type edge = NE.edge;

(* structure abbreviations *)
structure NnTab = NName.NTab;
structure NnCol = NName.Col;
structure EnTab = EName.NTab;
structure EnCol = EName.Col;

(* a graph *)
datatype T = PatGraph of 
         {bgraph : BGraph.T, (* the basic graph *)
          (* the variable nodes - graph level pattern vars *)
          vars : NName.Col.T,
          (* map of data variables to the nodes they occur in *)
					datavars : NName.Col.T VName.NTab.T
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

val set_bgraph = update_bgraph o K;
val set_vars = update_vars o K;
val add_var = update_vars o NnCol.union1;
val del_var = update_vars o NnCol.delete;
val del_vars = update_vars o NnCol.minus_set;

fun get_bgraph (PatGraph rep) = #bgraph rep;
fun get_vars (PatGraph rep) = #vars rep;
fun get_datavars (PatGraph rep) = #datavars rep;

val get_nodes = BGraph.get_nodes o get_bgraph;
val get_edges = BGraph.get_edges o get_bgraph;
val get_node_list = NName.NTab.dest o get_nodes;
val get_edge_list = EName.NTab.dest o get_edges;

(* implicit args: q n *)
val get_node = BGraph.get_node o get_bgraph;
val get_edge = BGraph.get_edge o get_bgraph;
val lookup_node = BGraph.lookup_node o get_bgraph;
val lookup_edge = BGraph.lookup_edge o get_bgraph;

(* adding a node gives back unique name for it and the new graph *)
fun add_named_node n (nd as NE.VarNode in_or_out) g =
    let val (n2,g2) = BGraph.add_named_node n nd (get_bgraph g)
    in (n2, g |> add_var n
              |> set_bgraph g2)
    end
  | add_named_node n nd g =
    let val (n2,g2) = BGraph.add_named_node n nd (get_bgraph g)
    in (n2, set_bgraph g2 g) end;

fun new_named_node n (nd as NE.VarNode in_or_out) g =
    let val bg2 = BGraph.new_named_node n nd (get_bgraph g)
    in g |> add_var n
         |> set_bgraph bg2 end
  | new_named_node n nd g =
    let val bg2 = BGraph.new_named_node n nd (get_bgraph g)
    in set_bgraph bg2 g end;

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

(* deleting node also removes all it's edges. *)
fun delete_node n g = 
    g |> (case (get_node g n) 
           of (NE.VarNode _,_) => del_var n
            | _ => I)
      |> update_bgraph (BGraph.delete_node n);

(* implicit args: en q *)
val delete_edge = update_bgraph o BGraph.delete_edge;

(* nname = suggested name *)
(* rename all occurances of old name (1st arg) with new name (2nd arg) 
   n2 must not exist already, n1 must exist *)
fun rename_nname n1 n2 g = 
    (case (get_node g n1)
      of (NE.VarNode _,_) => 
         g |> update_vars (NnCol.delete n1 o NnCol.union1 n2)
           |> update_bgraph (BGraph.rename_nname n1 n2)
       | _ => g |> update_bgraph (BGraph.rename_nname n1 n2));

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

(* pretty printing *)

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
                            (map NName.pretty (NnCol.dest (get_vars g)))],
            BGraph.pretty (get_bgraph g)]],
      Pretty.str "}"
    ];

val print = Pretty.writeln o pretty;

end;


structure DB_PatStrGraph = PatGraphFun(PatStrNE);
structure PatStrGraph :> PATTERN_GRAPH where 
type PatNE.node_pat = PatStrNE.node_pat 
 and type PatNE.edge_pat = PatStrNE.edge_pat 
= DB_PatStrGraph;
