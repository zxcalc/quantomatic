(* An ML file to help debug matching *)

val theory_name = "../theories/default-rules.theory";
val problem_graph_name = "test/dbg/matching_bug1.graph";

(* abbreviation *)
structure G = RGGraph;

(* load a set of rules *)
val ruletab = (RGRulesetInputXML.input o XMLReader.read_from_file) theory_name;
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
     (Seq.list_of (Rule.rewrites r g); NONE)
       handle _ => SOME n
    )
  rule_list;

(* val first_problem_rule = 
  (case problem_rules of 
     [] => (writeln "No problem rules!"; raise ERROR "No problem rules!")
   | h::t => fst (Rule.Name.NTab.get ruletab h));
*)

val first_problem_rule = fst (Rule.Name.NTab.get ruletab 
                               (Rule.Name.mk "x_copy_0"));

val tgt = g;
val (rn,rule) = Rule.rename_rule_apart tgt first_problem_rule;
val pat = Rule.get_lhs rule;

val rhs = (Rule.get_rhs rule);
val vs = RGGraph.get_vnames rhs;
val es = RGGraph.get_enames rhs;
val bs = RGGraph.get_bbnames rhs;
val names = (vs,es,bs);

val m = (Match.init names pat tgt);
val init_st = (MatchSearch.init m pat tgt Vertex.NTab.empty);
val st = init_st;

open DB_MatchSearch;
print st;

(* match_search = repeat the following *)
print st;
val (Progress s) = do_next_match_choices st;
val l = Seq.list_of s;
length l;
val [st] = l;


val SOME ((pn,tn),st2) = pop_agenda st;
val st = st2;

val SOME st2 = match_vertex (pn,pd) (tn,td) st;


val (from_pv as (from_pn,from_pd)) = (pn,pd); (* pat vertex *)
val (from_tv as (from_tn,from_td)) = (tn,td); (* tgt vertex *)
val (ignore_arity,st) = (0,st2); (* state of matching so far *)

val [(to_tn,tes)] = Vertex.NTab.list_of tns;

val [to_pn1,to_pn2] = Vertex.NSet.list_of pns;
val to_pn = to_pn1;

(* edgematch_adj_2vs *)
val st2 = st |> then_try (match_bboxes to_pn to_tn)
                        (try_add1_to_agenda (to_pn,to_tn))
print st2;


(* ... *)
val [st1,st2] = l;


val ms = Seq.list_of (Rule.matches rule tgt);

PolyML.exception_trace (fn () => 
  Seq.list_of (Rule.rewrites rule tgt)
);

val rs = Seq.list_of (Rule.rule_matches rule tgt);

val problem_rule_matches = 
  map_filter 
    (fn r => 
     (Rule.do_rewrite tgt r; NONE)
       handle _ => SOME r
    )
  rs;

val r = hd rs;


val _ = map Match.print ms;
val _ = Match.print m1;

val match = m3;
val bbmatch = Match.get_bbmatch m3;

val r2 = 
  PolyML.exception_trace (fn () => Rule.inst_rule rule match);
Rule.print r2;

val r2 = 
  PolyML.exception_trace (fn () => Rule.inst_rule rule m1);
Rule.print r2;




val _ = G.print pat;
val _ = G.print tgt;


open DB_MatchSearch;

val rhs = (Rule.get_rhs rule);
val vs = RGGraph.get_vnames rhs;
val es = RGGraph.get_enames rhs;
val bs = RGGraph.get_bbnames rhs;
val names = (vs,es,bs);
val init_st = (init (Match.init names pat tgt) pat tgt Vertex.NTab.empty);
val st = init_st;
print st;

(* match_search = repeat the following *)
val (Progress s) = do_next_match_choices st;
val l = Seq.list_of s;
length l;
val [st1,st2,st3,st4] = l;
(match_search (Seq.single st1)) ();
(match_search (Seq.single st2)) ();
(match_search (Seq.single st3)) ();
(match_search (Seq.single st4)) ();

val st = st2;
val (Progress s) = do_next_match_choices st;
val l = Seq.list_of s;
length l;
val [st] = l;


(* *)
val SOME (UnBBoxed pn) = get_next_pat_vertex st;
val [tn1,tn2,tn3] = Seq.list_of (get_unboxed_tgt_vnseq st);
val tn = tn1;

      val pat = get_unm_pat st;
      val tgt = get_unm_tgt st;
      val pd = RGGraph.get_vertex_data pat pn;
      val td = RGGraph.get_vertex_data tgt tn;

val SOME st2 = match_vertex (pn,pd) (tn,td) st;
val st = st2;
val [_,_,(ignore_arity, st)] = 
  Seq.list_of (expand_pat_bboxes (pn,pd) (tn,td) st);

(* *)
val [(ignore_arity, st)] = Seq.list_of (match_self_loops (pn,pd) (tn,td) (ignore_arity, st));
val [(ltes,st)] = Seq.list_of (st |> match_all_pat_self_loops lpes ltes);


val (from_pv as (from_pn,from_pd)) = (pn,pd)
val (from_tv as (from_tn,from_td)) = (tn,td);
val (ignore_arity,st) = (0,st2);


(* into graph ... *)



(* *)
val (Progress s) = do_next_match_choices st;
val l = Seq.list_of s;
val st = hd l;
val (Progress s) = do_next_match_choices st;
val l = Seq.list_of s;







PolyML.exception_trace (fn () => 
  Seq.list_of (Rule.rewrites rule tgt)
);



val ms = 
 PolyML.exception_trace (fn () => 
  Seq.list_of (MatchSearch.match pat tgt RGVertex.NTab.empty)
);
val l = length ms;
val bad_ms = 
    map_filter 
      (fn m => 
       (Rule.inst_rule rule m; NONE)
         handle _ => SOME m
      )
      ms;
val bad_l = length bad_ms;
val match = hd bad_ms;

val rule' = 
  PolyML.exception_trace (fn () => Rule.inst_rule rule match);
Rule.print rule';



val [m1,m2,m3,m4] = ms;

val r2 = 
  PolyML.exception_trace (fn () => Rule.inst_rule rule match);
Rule.print r2;


val rs = Seq.list_of (Rule.rule_matches rule tgt);


val _ = map Match.print ms;
val _ = Match.print m1;

val match = m3;
val bbmatch = Match.get_bbmatch m3;

val r2 = 
  PolyML.exception_trace (fn () => Rule.inst_rule rule match);
Rule.print r2;

val r2 = 
  PolyML.exception_trace (fn () => Rule.inst_rule rule m1);
Rule.print r2;


(* *)

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


