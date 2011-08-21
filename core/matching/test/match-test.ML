structure DB_Test_Match = Match(Test_Graph);
structure Test_Match : MATCH = DB_Test_Match;

local 
open Testing;
open Test_MkG;
structure M = Test_Match;
in val _ = let 

(* Graph: (B - X) *)
val _ = test "merge_edgehalfs" 
  (fn () => 
  let
    (* Graph: (B - X - B) *)
    val g = G.empty;
    val (b1, g) = g |> G.add_vertex bvert;
    val (n1, g) = g |> G.add_vertex (mkX zero_angle);
    val (b2, g) = g |> G.add_vertex bvert;
    val (e0, g) = g |> G.add_edge undir_edge b1 n1;
    val (e1, g) = g |> G.add_edge undir_edge n1 b2;
    val g1 = g;
    
    (* Graph: loop(X) *)
    val g = G.empty;
    val (n1', g) = g |> G.add_vertex (mkX zero_angle);
    val (e0', g) = g |> G.add_edge undir_edge n1' n1';
    val g2 = g;
    
    val no_extra_names = (V.NSet.empty, E.NSet.empty, G.BBox.NSet.empty);

    val m = M.init no_extra_names g1 g2 
              |> M.match_edgehalf_name (E.Ehn.mk_start e0) (E.Ehn.mk_start e0')
              |> M.match_edgehalf_name (E.Ehn.mk_end e1) (E.Ehn.mk_end e0')
              |> M.match_vertex (n1, G.get_vertex_data g1 n1) 
                              (n1, G.get_vertex_data g2 n1') 
              |> M.match_boundary b1 n1'
              |> M.match_boundary b2 n1';
              
    (* val g1' = M.inst_replacement_graph m g1; *)
    val g1' = M.merge_edgehalfs m g1;
    val _ = M.print m;
    val _ = G.print g1';

    (* check details of resulting graph *)
    val _ = if E.NSet.is_empty (G.get_edgepoints g1') then ()  
        else raise ERROR "merge_edgehalfs: has left an edge-point";
    val SOME e = E.NSet.tryget_singleton (G.get_enames g1');
    val ((G.UnDirected,ed),(fromv,tov)) = G.get_edge g1' e; 
    val _ = if V.name_eq (fromv, n1) then () 
            else raise ERROR "merge_edgehalfs: bad from vertex name";
    val _ = if V.name_eq (tov, n1) then () 
            else raise ERROR "merge_edgehalfs: bad from vertex name";
    
  in () end) ();

  in
(assert_no_failed_tests (); 
 Pretty.writeln (Pretty.str "Tests passed: matching/test/match-test.ML"))
end  (* let *)
handle e => 
  (Pretty.writeln (Pretty.str "Test Failed: matching/test/match-test.ML"); raise e);
end; (* local *)
