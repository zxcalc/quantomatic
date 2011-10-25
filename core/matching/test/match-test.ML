structure DB_Test_Match = Match(Test_Graph);
structure Test_Match : MATCH = DB_Test_Match;

local 
open Testing;
open Test_MkG;
structure M = Test_Match;
in val _ = let 

(* Graph: (B - X) *)
val _ = test "simple match with 1 plugging on self-loop" 
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
    
    val no_extra_names = (V.NSet.empty, E.NSet.empty, B.NSet.empty);

    val m = (M.init no_extra_names g1 g2) 
              |> M.match_edgehalf_name (H.mk_start e0) (H.mk_start e0')
              |> M.match_edgehalf_name (H.mk_end e1) (H.mk_end e0')
              |> M.match_vertex (n1, G.get_vertex_data g1 n1) 
                                (n1', G.get_vertex_data g2 n1') 
              |> M.match_boundary b1 n1'
              |> M.match_boundary b2 n1'
              |> M.add_plugging b1 b2;
              
    (* val g1' = M.inst_replacement_graph m g1; *)
    val g1' = M.merge_edgehalfs m g1;
    val _ = M.print m;
    val _ = G.print g1';

    (* check details of resulting graph *)
    val _ = if V.NSet.is_empty (G.get_edgepoints g1') then ()  
        else raise ERROR "merge_edgehalfs: has an edge-point";
    val SOME e = E.NSet.tryget_singleton (G.get_enames g1');
    val ((G.UnDirected,ed),(fromv,tov)) = G.get_edge g1' e; 
    val _ = if V.name_eq (fromv, n1) then () 
            else raise ERROR "merge_edgehalfs: bad from vertex name";
    val _ = if V.name_eq (tov, n1) then () 
            else raise ERROR "merge_edgehalfs: bad from vertex name";
    
  in () end) ();

  
  
(* Graph: (B - X) *)
val _ = test "match pluggings between 2 vertices" 
  (fn () => 
  let
    (* Graph: (B - X - X - B) *)
    val g = G.empty;
    val (b1, g) = g |> G.add_vertex bvert;
    val (n1, g) = g |> G.add_vertex (mkX zero_angle);
    val (n2, g) = g |> G.add_vertex (mkX zero_angle);
    val (b2, g) = g |> G.add_vertex bvert;
    val (e0, g) = g |> G.add_edge undir_edge b1 n1;
    val (e1, g) = g |> G.add_edge undir_edge n2 b2;
    val (e2, g) = g |> G.add_edge undir_edge n1 n2;
    val pat = g;
    
    (* Graph: X = X *)
    val g = G.empty;
    val (n1', g) = g |> G.add_vertex (mkX zero_angle);
    val (n2', g) = g |> G.add_vertex (mkX zero_angle);
    val (e0', g) = g |> G.add_edge undir_edge n1' n2';
    val (e1', g) = g |> G.add_edge undir_edge n1' n2';
    val tgt = g;
    
    val no_extra_names = (V.NSet.empty, E.NSet.empty, B.NSet.empty);

    val m = (M.init no_extra_names pat tgt) 
              |> M.match_edgehalf_name (H.mk_start e0) (H.mk_start e0')
              |> M.match_edgehalf_name (H.mk_end e1) (H.mk_end e0')
              |> M.match_edgehalf_name (H.mk_start e2) (H.mk_start e1')
              |> M.match_edgehalf_name (H.mk_end e2) (H.mk_end e1')
              |> M.match_vertex (n1, G.get_vertex_data pat n1) 
                                (n1', G.get_vertex_data tgt n1') 
              |> M.match_vertex (n2, G.get_vertex_data pat n2) 
                                (n2', G.get_vertex_data tgt n2') 
              |> M.match_boundary b1 n1'
              |> M.match_boundary b2 n2';
    
    val SOME should_be_e0_start = M.other_plugging_half_of_same_edge 
          m (H.mk_end e1) (H.mk_end e0'); 
    
    val _ = if H.name_eq (should_be_e0_start, H.mk_start e0) then () 
            else raise ERROR "opposite edge-point is not e0_start!";
       
    val plug_v1 = (G.get_ehn_vname pat (H.mk_end e1));
    val plug_v2 = (G.get_ehn_vname pat should_be_e0_start);
   
    val SOME ((dir,ed),(fromv,tov)) = G.boundary_coherence pat plug_v1 plug_v2; 
    
    val m = M.add_plugging plug_v1 plug_v2 m;   

    (* val g1' = M.inst_replacement_graph m g1; *)
    val pat' = M.merge_edgehalfs m pat;
    val _ = G.print tgt;
    val _ = G.print pat;
    val _ = M.print m;
    val _ = G.print pat';

    (* check details of resulting graph *)
    val _ = if V.NSet.is_empty (G.get_edgepoints pat') then ()  
        else raise ERROR "merge_edgehalfs: has an edge-point";
    val [e1,e2] = E.NSet.list_of (G.get_enames pat');
    val ((G.UnDirected,ed),(fromv,tov)) = G.get_edge pat' e1; 
    val _ = if V.name_eq (fromv, n2) then () 
            else raise ERROR "merge_edgehalfs: bad from vertex name1";
    val _ = if V.name_eq (tov, n1) then () 
            else raise ERROR "merge_edgehalfs: bad from vertex name2";
    val ((G.UnDirected,ed),(fromv,tov)) = G.get_edge pat' e2; 
    val _ = if V.name_eq (fromv, n1) then () 
            else raise ERROR "merge_edgehalfs: bad from vertex name3";
    val _ = if V.name_eq (tov, n2) then () 
            else raise ERROR "merge_edgehalfs: bad from vertex name4";
  in () end) ();

  in
(assert_no_failed_tests (); 
 Pretty.writeln (Pretty.str "Tests passed: matching/test/match-test.ML"))
end  (* let *)
handle e => 
  (Pretty.writeln (Pretty.str "Test Failed: matching/test/match-test.ML"); raise e);
end; (* local *)
