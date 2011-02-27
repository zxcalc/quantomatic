

(* make an instantiated spider rule (assumes graph is connected, else
will only find one spider - for one of the connected subgraphs) *)
fun mk_max_spider g = 
    let 
      (* args to recf: matched spiders, (cur spider lhs, spider rhs single 
         dot sum), (still connected agenda, unseen vs, graph) *)  
      

      (* First case: end of spider case: no more adjacent vertcies *)
      and recf spiders (cur_spider as (spider_lhs : RGGraph.T , _ : RGGraph.RGVertex.qnd)) ([],vs) = 
            (* only add this as a spider application if more than 1
               vertex is in it; it can't be empty, so just check is
               singleton. *)
          let 
            val spiders2 = 
                
          in startf spiders2 vs end
        | recf spiders (spider_lhs,sipder_d : RGGraph.RGVertex.qnd) ((from_n,(to_n,edatas_and_dirs))::agenda,vs) = 
          let val (d : RGGraph.BVertex.data,_) = RGGraph.get_vertex g to_n 
          in
            case (try_merge_data d sipder_d) of 
              NONE => (* add as a boundary node - not part of the spider *)
              recf spiders (spider_lhs
                              |> (RGGraph.new_named_vertex 
              (* PolyML bug: RGGraph.BVertex.BVert should work here *)
                                    to_n (RGGraph.RGVertex.BVert ()))
                              |> (fold 
                                    (fn (en,(ed,(from_n',to_n'))) =>
                                        RGGraph.new_named_edge 
                                          en ed from_n' to_n')
                                    edatas_and_dirs),
                            sipder_d )
                   (agenda,vs) (* agenda tail, and same vs *)
            | SOME spider_d2 => 
              recf spiders (spider_lhs 
                              |> RGGraph.new_named_vertex to_n d
                              |> (fold 
                                    (fn (en,(ed,(from_n',to_n'))) =>
                                        RGGraph.new_named_edge 
                                          en ed from_n' to_n')
                                    edatas_and_dirs), 
                            spider_d2) 
                   ((get_adjs_list g to_n) @ agenda, 
                    (* remove one from spider start set *)
                    Vertex.NSet.delete to_n vs)
          end
    in 
      Seq.map rule_of_spider 
              (Seq.of_list 
                 (startf [] (RGGraph.get_vnames g)))
    end;



      (* search a possible spider start *)
      fun startf spiders vs =
          (case (Vertex.NSet.pull_local_bot vs) of 
             NONE => spiders
           | SOME (n,vs2) => 
             (case fst (RGGraph.get_vertex g n) 
               of (d as (RGGraph.BVertex.IVert (RGGraph.RGVertex.Xnd a))) => 
                  recf spiders (RGGraph.new_named_vertex n d RGGraph.empty, 
                                RGGraph.RGVertex.Xnd a)
                       (get_adjs_list g n, vs2)
                | (d as (RGGraph.BVertex.IVert (RGGraph.RGVertex.Znd a))) => 
                  recf spiders (RGGraph.new_named_vertex n d RGGraph.empty, 
                                RGGraph.RGVertex.Xnd a)
                       (get_adjs_list g n, vs2)
                | _ => startf spiders vs2))



local open RGGraph in 

(* two graphs are exactly the same, including data, edge direction and
   names of vertices and edges. *) 
fun exact_eq g1 g2 =
    let 
      val e1s = get_edges g1
      val e2s = get_edges g2
      val v1s = get_vertices g1
      val v2s = get_vertices g2
    in
      (* check same vertex names *)
      (Vertex.NSet.eq (Vertex.NTab.get_names v1s)
                      (Vertex.NTab.get_names v2s)) andalso 
      (* check same data, and same incident edges structure *)
      (Vertex.NTab.forall 
         (fn (n,(d,(fromes,toes))) => 
             (case Vertex.NTab.lookup v2s n of 
                NONE => false orelse (writeln "Missing in snd"; false)
              | SOME (d2,(fromes2,toes2)) => 
                ((Vertex.data_eq (d,d2))
                 orelse (writeln "Different data"; false)) andalso 
                (* now check structure *)
                (Edge.NSet.eq fromes fromes2
                 orelse (writeln "Different fromes"; false)
                ) andalso
                (Edge.NSet.eq toes toes2)
                orelse (writeln "Different toes"; false)) 
             orelse (writeln ("; different for Vertex: " ^ n); false))
         v1s) andalso 
      (* now check same edge data *)
      (Edge.NTab.forall (* note that we dont re-check structure *)
         (fn (e,(ed,_)) => 
             (* from above, we know we have same edge names: could use get *)
             (case Edge.NTab.lookup e2s e of 
                NONE => false
              (* now check have same edge data *)
              | SOME (ed2,_) => Edge.data_eq (ed,ed2))
             orelse (writeln ("missing or different data; for Edge: " ^ e);
                     false))
         e1s)
    end;

end;
