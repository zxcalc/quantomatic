
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
