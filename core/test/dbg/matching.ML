(* An ML file to help debug matching *)

val theory_name = "../theories/default-rules.theory";
val problem_graph_name = "test/dbg/bbox_bug2.graph";

(* abbreviation *)
structure G = RGGraph;

(* load a set of rules *)
val ruletab = (RGRuleSetInputXML.input o XMLReader.read_from_file) theory_name;
val rule_list = 
  map_filter 
    (fn (n,(r,b)) => if b then SOME(n,r) else NONE) 
    (Rule.Name.NTab.list_of ruletab);

(* load the problem graph *)
val g = RGGraphLoaderSaver.load_graph problem_graph_name;
G.print g;

(* print vertex names in the graph *)
val vns = G.get_vnames g;
G.Vertex.NSet.print vns;

(* find the subset of rules which raise an exception *)
val problem_rules = 
  map_filter 
    (fn (n,r) => 
     (Seq.list_of (Rule.rule_matches_within r vns g); NONE)
       handle _ => SOME n
    )
  rule_list;

val first_problem_rule = 
  (case problem_rules of 
     [] => (writeln "No problem rules!"; raise ERROR "No problem rules!")
   | h::t => fst (Rule.Name.NTab.get ruletab h));

val tgt = g;
val (rn,rule) = Rule.rename_rule_apart tgt first_problem_rule;
val pat = Rule.get_lhs rule;

val _ = G.print pat;
val _ = G.print tgt;

val ms = 
 PolyML.exception_trace (fn () => 
  Seq.list_of (MatchState.match pat tgt RGVertex.NTab.empty)
);
val l = length ms;
val [m1,m2,m3,m4] = ms;
val _ = map Match.print ms;
val _ = Match.print m1;


(* *)
val match = m1;
val bbmatch = Match.get_bbmatch m1;

open DB_Rule;
val (rn,g) = (get_rhs rule) 
                  |> rename_interior_vs 
                       (RGGraph.get_boundary (get_rhs rule)) 
                       (BBMatch.get_new_xmap_names bbmatch);


val (bn1,bn2)::more = BBMap.Dom.NTab.list_of (BBMap.get_domtab (BBMatch.get_copies bbmatch));

PolyML.exception_trace (fn () =>
  RGGraph.copy1_box_given_ext_rnm bn1 bn2 ((vrn,ern),g));




PolyML.exception_trace (fn () =>
  RGGraph.copy1_box_given_ext_rnm bn1 bn2 ((vrnm,ernm),g));


(* *)
val r2 = 
  PolyML.exception_trace (fn () => Rule.inst_rule rule m1);

val match = m1;



val r2 = 
  PolyML.exception_trace (fn () => Rule.inst_rule rule m1);


