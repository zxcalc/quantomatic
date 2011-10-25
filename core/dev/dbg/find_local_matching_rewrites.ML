(* load a set of rules *)
val ruleset = (RGRulesetInputXML.input o XMLReader.read_from_file) "test/dbg/default-rules.xml";
val rule_list = 
  map_filter 
    (fn (n,(r,b)) => if b then SOME(n,r) else NONE) 
    (Rule.Name.NTab.list_of ruleset);

(* load a graph of interest *)
val g = RGGraphLoaderSaver.load_graph "test/dbg/QFT2qubit_10.xml";

(* print vertex names in the graph *)
V.NSet.print (RGGraph.get_vnames g);

(*  *)
PolyML.Debug.breakEx 
(RGGraph.no_such_vname_exp ("",V.default_name,g));
PolyML.Debug.clearEx 
(RGGraph.no_such_vname_exp ("",V.default_name,g));

val vns = V.NSet.empty;
Seq.list_of (Ruleset.apply_rules_in vns g);

val tgt = RGGraph.cut_subgraph_of_selection vns g;


Seq.maps (fn (n,r) => Seq.map (fn r2 => (n,r2)) 
                         (Rule.rule_matches_within r vset g)
                         handle RGGraph.no_such_vname_exp (s,v,g) 
                           => (print n; raise RGGraph.no_such_vname_exp (s,v,g)) 
                         )
         (Seq.of_list rule_list)
         

open PolyML.Debug;


open MkG;

val g = G.empty;
val (b1, g) = g |> G.add_vertex bvert;

G.cut_subgraph_of_selection V.NSet.empty g;




G.delete_vertex b1 g;
