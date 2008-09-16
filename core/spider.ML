(* make an instantiated spider rule *)
(* val mk_max_spider : RGGraph.T -> T Seq.seq;
val mk_max_spider_within : RGGraph.Vertex.NSet.T -> RGGraph.T -> T Seq.seq;
*)

structure Spider 
= struct

(* these are really in MatchState - this is just a local abbreviation *)
structure RGGraph = MatchState.RGGraph;
structure Vertex = MatchState.Vertex;
structure Edge = MatchState.Edge;

(* basic spider info *)
datatype T = Spider of (RGGraph.T * RGGraph.RGVertex.qnd)

fun pretty (Spider (g,qnd)) = 
    Pretty.chunks [Pretty.str "Spider lhs:",
                   RGGraph.pretty g,
                   Pretty.block [Pretty.str "Spider rhs angle: ", 
                                 RGGraph.IVertex.pretty_data qnd]];
val print = Pretty.writeln o pretty;

(* part of agenda when extrending a spider *)
datatype todo = 
         ToDo of { from_n : Vertex.name, 
                   to_n : Vertex.name,
                   eset : (Edge.name * 
                           (Edge.data * (Vertex.name * Vertex.name)))
                            list};

fun pretty_todo (ToDo rep) = 
    Pretty.chunks
    [Pretty.str "ToDo{", 
     Pretty.block [Pretty.str "to_n: ", Vertex.pretty_name (#to_n rep)],
     Pretty.block [Pretty.str "from_n: ", Vertex.pretty_name (#from_n rep)],
     Pretty.block [Pretty.str "eset:", 
                   Pretty.list 
                     "[" "]" (map (fn (en,(ed,(from_n2,to_n2))) => 
                                   Pretty.block [Edge.pretty_name en,
                                                 Pretty.str ": ",
                                                 Edge.pretty_data ed,
                                                 Pretty.str "; from: ",
                                                 Vertex.pretty_name from_n2,
                                                 Pretty.str " to: ",
                                                 Vertex.pretty_name to_n2])
                                  (#eset rep))],
     Pretty.str "}"];
val print_todo = Pretty.writeln o pretty_todo;

(* make an agenda (todo list) *)
fun add_to_adj_agenda g n agenda = 
    Vertex.NTab.fold 
      (fn (n2,adj_enames) => fn agenda => 
         (ToDo {to_n = n2,
                from_n = n,
                eset = map (fn en => (en,RGGraph.get_edge g en))
                           (Edge.NSet.dest adj_enames)})
         :: agenda)
(* 
         case Vertex.NTab.lookup ntab n2 
          of NONE => 
             Vertex.NTab.ins 
               (n2,ToDo {from_n = n,
                         eset = map (fn en => (en,RGGraph.get_edge g en))
                                    (Edge.NSet.dest adj_enames)}) ntab
           | SOME _ => ntab *) (* don't add duplicates *)
      (RGGraph.adj_vertices_with_enames g n)
      agenda;

(* state of searching for a maximal spider *)
datatype searchstate = 
         SearchState of {cur_spider : T,
                         cur_agenda : todo list,
                         leftoverg : RGGraph.T,
                         uncheckedvs : Vertex.NSet.T
                        };

fun pretty_st (SearchState rep) = 
    Pretty.chunks
    [Pretty.block [Pretty.str "cur:", pretty (#cur_spider rep)],
     Pretty.block [Pretty.str "agenda: ", 
                   Pretty.list "[" "]" (map pretty_todo (#cur_agenda rep))],
     Pretty.block [Pretty.str "leftover_g:", RGGraph.pretty (#leftoverg rep)],
     Pretty.block [Pretty.str "unchecked_vs:", Vertex.NSet.pretty (#uncheckedvs rep)]
    ];

val print_st = Pretty.writeln o pretty_st;

fun rule_of_spider_for_g g (Spider (spider_lhs,spider_d)) =
    let 
      val boundary = (RGGraph.get_boundary spider_lhs)
      (* ensure rhs center name is distinct from lhs boundary *)
      val fresh_vn = Vertex.NSet.new "spider" boundary
      val rhs = 
          (RGGraph.new_named_vertex 
             fresh_vn (RGGraph.BVertex.IVert spider_d) RGGraph.empty)
            |> (Vertex.NSet.fold 
                  (fn n => fn g => 
                   let val (vd,(from_es,to_es)) =
                           RGGraph.get_vertex spider_lhs n
                   in g |> RGGraph.new_named_vertex n vd
                        |> (Edge.NSet.fold 
                              (fn e => 
                                  let val (ed,_) = RGGraph.get_edge spider_lhs e
                                  in RGGraph.new_named_edge 
                                       e ed n fresh_vn end)
                              from_es)
                        |> (Edge.NSet.fold 
                              (fn e => 
                                  let val (ed,_) = RGGraph.get_edge spider_lhs e
                                  in RGGraph.new_named_edge 
                                       e ed fresh_vn n end)
                              to_es)
                   end)
                  boundary)
      val vrn = Vertex.mk_renaming (* rename central vertex *)
                  boundary (RGGraph.get_vnames g) Vertex.NTab.empty;
      val ern = Edge.mk_renaming (* avoid edge names in g *)
                  Edge.NSet.empty (RGGraph.get_enames g) Edge.NTab.empty;
      val (rn,rhs2) = RGGraph.rename (vrn,ern) rhs;
    in Rule.mk (spider_lhs, rhs2) end;


(* *)
fun start_spider_match (vs, g) = 
    (case (Vertex.NSet.pull_local_bot vs) of 
       NONE => NONE
     | SOME (n,vs2) => 
       (case fst (RGGraph.get_vertex g n) 
         of (d as (RGGraph.BVertex.IVert (RGGraph.RGVertex.Xnd a))) => 
            SOME (SearchState 
                    {cur_spider = 
                     Spider (RGGraph.new_named_vertex n d RGGraph.empty,
                             RGGraph.RGVertex.Xnd a),
                     cur_agenda = add_to_adj_agenda g n [], 
                     uncheckedvs = vs2,
                     leftoverg = RGGraph.delete_vertex n g})
          | (d as (RGGraph.BVertex.IVert (RGGraph.RGVertex.Znd a))) => 
            SOME (SearchState 
                    {cur_spider = 
                     Spider (RGGraph.new_named_vertex n d RGGraph.empty,
                             RGGraph.RGVertex.Znd a),
                     cur_agenda = add_to_adj_agenda g n [],
                     uncheckedvs = vs2,
                     leftoverg = RGGraph.delete_vertex n g})
          | _ => start_spider_match (vs2,g)))

fun try_merge_data (RGGraph.BVertex.IVert (RGGraph.RGVertex.Xnd a2)) 
                   (RGGraph.RGVertex.Xnd a) = 
    SOME (RGGraph.RGVertex.Xnd (AngleExpr.add_expr a a2))
  | try_merge_data (RGGraph.BVertex.IVert (RGGraph.RGVertex.Znd a2)) 
                   (RGGraph.RGVertex.Znd a) = 
    SOME (RGGraph.RGVertex.Znd (AngleExpr.add_expr a a2))
  | try_merge_data (_ : RGGraph.BVertex.data) _ = NONE;

(* singleton, if only 1 non-boundary vertex *)
fun is_singleton_spider (Spider (g,d)) = 
    Vertex.NSet.is_singleton
      (Vertex.NSet.subtract
         (Vertex.NTab.get_names (RGGraph.get_vertices g))
         (RGGraph.get_boundary g));

fun add_edgeset eset g = 
    g |> (fold 
            (fn (en,(ed,(from_n',to_n'))) =>
                RGGraph.new_named_edge 
                  en ed from_n' to_n')
            eset);

(* to_n - new vertex to add
   to_d - new vertex data
   eset - data of edges of how we got here 
   d2 - new summed spider rhs data 
   (Spider (g,d)) - old spider state *)
fun add_vertex_to_spider to_n to_d eset d2 (Spider (g,d)) = 
    Spider (g |> (RGGraph.new_named_vertex to_n to_d)
              |> add_edgeset eset,
            d2);

fun expand_spider (SearchState rep) = 
    let val (cur_spider as Spider (spider_g,spider_d)) = #cur_spider rep;
        val leftover_g = #leftoverg rep;
        val unchecked_vs = #uncheckedvs rep;
    in 
      case (#cur_agenda rep)
       of [] => (if is_singleton_spider cur_spider 
                 then NONE else SOME cur_spider, 
                 start_spider_match (unchecked_vs, leftover_g))
        | (ToDo {to_n, from_n, eset}::agenda) =>
          (NONE, SOME
           (case RGGraph.lookup_vertex spider_g to_n 
            of SOME d => (* already added form a different path *)
               (SearchState  (* add more edges ot lhs *)
                  {cur_spider = Spider (add_edgeset eset spider_g, spider_d),
                   cur_agenda = agenda, 
                   uncheckedvs = unchecked_vs,
                   leftoverg = leftover_g})
             | NONE => (* no considered already *)
               let val (d : RGGraph.BVertex.data,_) = 
                       RGGraph.get_vertex leftover_g to_n 
               in 
                 case (try_merge_data d spider_d) 
                  of NONE => (* add as a boundary - not part of the spider *)
                  SearchState 
                    {cur_spider = add_vertex_to_spider 
                                    to_n (RGGraph.RGVertex.BVert ())
                                    eset spider_d cur_spider,
                     cur_agenda = agenda, 
          (* IMPROVE: if d is boundary, then from it from unchecked_vs *)
                     uncheckedvs = unchecked_vs,
                     leftoverg = leftover_g}
                | SOME spider_d2 => 
                  SearchState 
                    {cur_spider = add_vertex_to_spider 
                                    to_n d eset spider_d2
                                    cur_spider,
                     cur_agenda = add_to_adj_agenda leftover_g to_n agenda, 
                     uncheckedvs = Vertex.NSet.delete to_n unchecked_vs,
                     leftoverg = RGGraph.delete_vertex to_n leftover_g}
               end))
    end;

fun mk_max_spider g = 
    let fun recf (SOME x, NONE) () = SOME (x, Seq.empty)
          | recf (SOME x, SOME st2) () = 
            SOME (x, Seq.make (recf (expand_spider st2)))
          | recf (NONE, SOME st2) () = recf (expand_spider st2) ()
          | recf (NONE,NONE) () = NONE
    in Seq.make (recf (NONE,start_spider_match (RGGraph.get_vnames g, g))) end;

fun mk_max_spider_within vns tgt = 
    mk_max_spider (RGGraph.subgraph_of_selection vns tgt);

fun mk_max_spider_rule g = 
    Seq.map (rule_of_spider_for_g g) (mk_max_spider g);

fun mk_max_spider_rule_within vset g = 
    Seq.map (rule_of_spider_for_g g) (mk_max_spider_within vset g);

end;
