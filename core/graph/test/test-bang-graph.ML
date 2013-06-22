local
  (* start by testing that things in the OGRAPH interface still work *)
  structure OGraph_Interface_Tests_Bang_Graph = OGraph_Interface_Tests(Test_Bang_Graph);
  structure Tools = Test_Bang_Graph_Tools(Test_Bang_Graph);
  open Tools;

  val [x1,x2,x3,x4] = map V.mk ["x1","x2","x3","x4"];
  val v2 = V.mk "v2";
  val [b0,b1,b2] = map B.mk ["B0","B1","B2"];

  local (* BBox functions *)
    val _ = Testing.test "G.get_bboxes - empty"
      (fn () => if B.NSet.is_empty (G.get_bboxes G.empty) then I
              else raise ERROR "get_bboxes (empty) incorrect") ()

    (*val _ = Testing.test "G.get_bbox_rel - empty"
      (fn () => if BVBRel.is_empty (G.get_bbox_rel G.empty) then I
              else raise ERROR "get_bbox_rel (empty) incorrect") ()

    val _ = Testing.test "G.get_bbox_table - empty"
      (fn () => if B.NTab.is_name_empty (G.get_bbox_table G.empty) then I
              else raise ERROR "get_bbox_table (empty) incorrect") ()

    val _ = Testing.test "G.get_vertex_bbox_table - empty"
      (fn () => if V.NTab.is_name_empty (G.get_vertex_bbox_table G.empty) then I
              else raise ERROR "get_vertex_bbox_table (empty) incorrect") ()*)

    val _ = Testing.test "G.get_bboxed - empty"
      (fn () => if V.NSet.is_empty (G.get_bboxed G.empty) then I
              else raise ERROR "get_bboxed (empty) incorrect") ()

    val _ = Testing.test "G.get_unbboxed - empty"
      (fn () => if V.NSet.is_empty (G.get_unbboxed G.empty) then I
              else raise ERROR "get_unbboxed (empty) incorrect") ()

    val _ = Testing.test "G.get_empty_bboxes - empty"
      (fn () => if B.NSet.is_empty (G.get_empty_bboxes G.empty) then I
              else raise ERROR "get_empty_bboxes (empty) incorrect") ()

    val _ = Testing.test "G.get_bboxes - empty"
      (fn () => if B.NSet.is_empty (G.get_bboxes G.empty)
                then ()
                else raise ERROR "get_bboxes (empty) incorrect") ()

    val _ = Testing.test "G.has_bbox - empty"
      (fn () => if not (G.has_bbox G.empty B.default_name) then I
              else raise ERROR "has_bbox (empty) incorrect") ()

    val _ = Testing.test "G.get_vertices_in_bbox - empty"
      (fn () => (G.get_vertices_in_bbox G.empty B.default_name;
              raise ERROR "get_vertices_in_bbox (empty) incorrect")
              handle G.no_such_bbox_exp _ => I) ()

    val _ = Testing.test "G.get_bboxes_containing_vertex - empty"
      (fn () => (G.get_bboxes_containing_vertex G.empty V.default_name;
              raise ERROR "get_bboxes_containing_vertex (empty) incorrect")
              handle G.no_such_vertex_exp _ => I) ()

    val _ = Testing.test "G.is_bboxed - empty"
      (fn () => (G.is_bboxed G.empty V.default_name;
              raise ERROR "is_bboxed (empty) incorrect")
              handle G.no_such_vertex_exp _ => I) ()

    val _ = Testing.test "G.rename_bbox - empty"
      (fn () => (
        G.rename_bbox (B.mk "B0") (B.mk "B1") G.empty;
        raise ERROR "rename_bbox - empty should raise"
        ) handle G.no_such_bbox_exp _ => I) ()

    (*val _ = Testing.test "G.try_rename1_bbox - empty"
      (fn () =>
        case G.try_rename1_bbox (B.mk "B0") (B.mk "B1") G.empty of
             NONE =>
               raise ERROR "try_rename1_bbox (empty) incorrect [got NONE]"
           | SOME g' =>
               (* no effect *)
               if B.NSet.is_empty (G.get_bboxes g') then I
               else raise ERROR "try_rename1_bbox (empty) incorrect [added bbox]"
           ) ()*)

    val _ = Testing.test "G.delete_bbox - empty"
      (fn () => G.delete_bbox B.default_name G.empty) ()

    val _ = Testing.test "g.add_to_bbox - empty"
      (fn () => (G.add_to_bbox B.default_name (V.NSet.single V.default_name) G.empty;
              raise error "add_to_bbox (empty) incorrect")
              handle G.no_such_vertex_exp _ => I
                   | G.no_such_bbox_exp _ => I) ()

    val _ = Testing.test "G.remove_from_bbox - empty"
      (fn () => (G.remove_from_bbox B.default_name (V.NSet.single V.default_name) G.empty;
              raise ERROR "remove_from_bbox (empty) incorrect")
              handle G.no_such_vertex_exp _ => I
                   | G.no_such_bbox_exp _ => I) ()

    val _ = Testing.test "G.kill_bbox - empty"
      (fn () => (G.kill_bbox B.default_name G.empty;
              raise ERROR "kill_bbox (empty) incorrect")
              handle G.no_such_bbox_exp _ => I) ()

    val _ = Testing.test "G.drop_bbox - empty"
      (fn () => (G.drop_bbox B.default_name G.empty;
              raise ERROR "drop_bbox (empty) incorrect")
              handle G.no_such_bbox_exp _ => I) ()

    val _ = Testing.test "G.merge_bboxes - empty"
      (fn () =>
      let
        val bs = B.NSet.of_list [B.mk "B0", B.mk "B1"]
      in
        (G.merge_bboxes bs G.empty;
              raise ERROR "merge_bboxes (empty) incorrect")
              handle G.no_such_bbox_exp _ => I
      end) ()

    (* FIXME: test copy1_bbox *)

    val _ = Testing.test "G.fresh_copy_bbox_anon - empty"
      (fn () => (G.fresh_copy_bbox_anon B.default_name G.empty;
              raise ERROR "fresh_copy_bbox_anon (empty) should throw exception")
              handle G.no_such_bbox_exp _ => I) ()

    (* FIXME: test rename_apart_incl_bboxes *)

    val gnv = test_force "G.add_named_bbox (1)"
          (fn g =>
          let
            val bexp = b0
            val g' = G.add_named_bbox bexp g
          in
            case B.NSet.tryget_singleton (G.get_bboxes g') of
                 NONE => raise ERROR "add_named_bbox did not add a !-box"
               | SOME b =>
                      if not (B.name_eq (b,bexp)) then
                        raise ERROR "add_named_bbox added the wrong name"
                      else g'
          end) G.empty

    val gnv = test_force "G.add_named_bbox (2)"
          (fn g =>
          let
            val bexp = b1
            val bs_old = G.get_bboxes g
            val g' = G.add_named_bbox bexp g
            val bs_new = G.get_bboxes g'
            val bs_added = B.NSet.subtract bs_new bs_old
          in
            case B.NSet.tryget_singleton bs_added of
                 NONE => raise ERROR "add_named_bbox did not add a !-box"
               | SOME b =>
                      if not (B.name_eq (b,bexp)) then
                        raise ERROR "add_named_bbox added the wrong name"
                      else g'
          end) gnv

    val _ = Testing.test "G.add_named_bbox (existing !-box)"
          (fn g => (G.add_named_bbox b0 g;
                    raise error "add_named_bbox (name clash) did not throw exception")
                   handle G.duplicate_bbox_exp _ => I) gnv

    val (b2,gnv) = test_force "G.add_bbox"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val (b,g') = G.add_bbox g
            val bs_new = G.get_bboxes g'
            val bs_added = B.NSet.subtract bs_new bs_old
          in
            case B.NSet.tryget_singleton bs_added of
                 NONE => raise ERROR "add_named_bbox did not add a !-box"
               | SOME b' =>
                      if not (B.name_eq (b,b')) then
                        raise ERROR "add_named_bbox returned the wrong name"
                      else (b,g')
          end) gnv

    val _ = Testing.test "G.delete_bbox (no such bb)"
          (fn g =>
          let
            val bexp = (B.mk "B9")
            val bs_old = G.get_bboxes g
            val g' = G.delete_bbox bexp g
            val bs_new = G.get_bboxes g'
          in
            if not (B.NSet.eq bs_old bs_new) then
              raise ERROR "delete_bbox managed to delete a !-box that didn't exist"
            else g'
          end) gnv

    val _ = Testing.test "G.delete_bbox (empty !-box)"
          (fn g =>
          let
            val bexp = b0
            val bs_old = G.get_bboxes g
            val g' = G.delete_bbox bexp g
            val bs_new = G.get_bboxes g'
            val bs_added = B.NSet.subtract bs_new bs_old
            val _ = if not (B.NSet.is_empty bs_added) then
                      raise ERROR "delete_bbox managed to add something"
                    else ()
            val bs_removed = B.NSet.subtract bs_old bs_new
          in
            case B.NSet.tryget_singleton bs_removed of
                 NONE => raise ERROR "delete_bbox did not delete exactly one item"
               | SOME b' =>
                      if not (B.name_eq (b',bexp)) then
                        raise ERROR "delete_bbox deleted the wrong !-box"
                      else g'
          end) gnv

    
    val _ = Testing.test "G.rename_bbox (no such bb)"
      (fn () => (
        gnv |> G.rename_bbox (B.mk "FOO") (B.mk "B10");
        raise ERROR "rename_bbox (no such bb) should raise"
        ) handle G.no_such_bbox_exp _ => I) ()

    val _ = Testing.test "G.add_to_bbox_anon (no such verts)"
          (fn g => (G.add_to_bbox_anon b0 (V.NSet.single V.default_name) g;
                    raise error "add_to_bbox_anon (no such verts) incorrect")
                   handle G.no_such_vertex_exp _ => I) gnv

    val gv = gnv |> add_vunit1 "x1"
                 |> add_vunit1 "x2"
                 |> add_vunit2 "x3"
                 |> add_vexpr1 "x4" "a+b"
                 |> add_dir_eunit1 "e1" "x1" "x2"
                 |> add_dir_eunit1 "e2" "x2" "x3"
                 |> add_dir_eunit1 "e3" "x3" "x1"
                 |> add_dir_eunit1 "e4" "x4" "x1"
                 |> add_dir_eunit1 "e5" "x4" "x2"
                 |> add_dir_eunit1 "e6" "x4" "x3";

    val _ = Testing.test "G.add_to_bbox_anon (no such bbox)"
          (fn g => (G.add_to_bbox_anon (B.mk "B9") (V.NSet.single x1) g;
                    raise error "add_to_bbox_anon (no such bbox) incorrect")
                   handle G.no_such_bbox_exp _ => I) gv

    val g_disjoint = test_force "G.add_to_bbox_anon/G.get_vertices_in_bbox (disjoint)"
          (fn g =>
          let
            val b0_exp = (V.NSet.single x1)
            val b1_exp = (V.NSet.of_list [x2,x3])
            val bs_old = G.get_bboxes g
            val g' = G.add_to_bbox_anon b0 b0_exp g
            val g'' = G.add_to_bbox_anon b1 b1_exp g'
            val bs_new = G.get_bboxes g''
            val _ = if not (B.NSet.eq bs_old bs_new) then
                      raise ERROR "add_to_bbox_anon changed the !-box set"
                    else ()
            val _ = if not (V.NSet.eq b0_exp (G.get_vertices_in_bbox g'' b0)) then
                      raise ERROR "add_to_bbox_anon did not set B0 contents properly"
                    else ()
            val _ = if not (V.NSet.eq b1_exp (G.get_vertices_in_bbox g'' b1)) then
                      raise ERROR "add_to_bbox_anon did not set B1 contents properly"
                    else ()
          in g'' end) gv

    val g_overlap = test_force "G.add_to_bbox_anon/G.get_vertices_in_bbox (overlapping)"
          (fn g =>
          let
            val b0_exp = (V.NSet.of_list [x1,x2])
            val b1_exp = (V.NSet.of_list [x2,x3])
            val bs_old = G.get_bboxes g
            val g' = G.add_to_bbox_anon b0 b0_exp g
            val g'' = G.add_to_bbox_anon b1 b1_exp g'
            val bs_new = G.get_bboxes g''
            val _ = if not (B.NSet.eq bs_old bs_new) then
                      raise ERROR "add_to_bbox_anon changed the !-box set"
                    else ()
            val _ = if not (V.NSet.eq b0_exp (G.get_vertices_in_bbox g'' b0)) then
                      raise ERROR "add_to_bbox_anon did not set B0 contents properly"
                    else ()
            val _ = if not (V.NSet.eq b1_exp (G.get_vertices_in_bbox g'' b1)) then
                      raise ERROR "add_to_bbox_anon did not set B1 contents properly"
                    else ()
          in g'' end) gv

    val _ = Testing.test "G.remove_from_bbox_anon (no such bbox)"
          (fn g => (G.remove_from_bbox_anon (B.mk "B9") (V.NSet.single x1) g;
                    raise error "remove_from_bbox_anon (no such bbox) incorrect")
                   handle G.no_such_bbox_exp _ => I) gv

    val _ = Testing.test "G.remove_from_bbox_anon (disjoint; 1)"
          (fn g =>
          let
            val b1_rem = (V.NSet.single x2)
            val bs_old = G.get_bboxes g
            val g' = G.remove_from_bbox_anon b1 b1_rem g
            val bs_new = G.get_bboxes g'
            val _ = if B.NSet.eq bs_old bs_new then ()
                    else raise ERROR "remove_from_bbox_anon changed the !-box set"
            val _ = if V.NSet.eq (V.NSet.single x3) (G.get_vertices_in_bbox g' b1) then ()
                    else raise ERROR "remove_from_bbox_anon did not set B1 contents properly"
          in g' end) g_disjoint

    val _ = Testing.test "G.remove_from_bbox_anon (disjoint; 2)"
          (fn g =>
          let
            val b1_rem = (V.NSet.of_list [x2,x3])
            val bs_old = G.get_bboxes g
            val g' = G.remove_from_bbox_anon b1 b1_rem g
            val bs_new = G.get_bboxes g'
            val _ = if B.NSet.eq bs_old bs_new then ()
                    else raise ERROR "remove_from_bbox_anon changed the !-box set"
            val _ = if V.NSet.is_empty (G.get_vertices_in_bbox g' b1) then ()
                    else raise ERROR "remove_from_bbox_anon did not set B1 contents properly"
          in g' end) g_disjoint

    val _ = Testing.test "G.remove_from_bbox_anon (disjoint; 3)"
          (fn g =>
          let
            val b1_rem = (V.NSet.of_list [x1,x2])
            val bs_old = G.get_bboxes g
            val g' = G.remove_from_bbox_anon b1 b1_rem g
            val bs_new = G.get_bboxes g'
            val _ = if B.NSet.eq bs_old bs_new then ()
                    else raise ERROR "remove_from_bbox_anon changed the !-box set"
            val _ = if V.NSet.eq (V.NSet.single x3) (G.get_vertices_in_bbox g' b1) then ()
                    else raise ERROR "remove_from_bbox_anon did not set B1 contents properly"
          in g' end) g_disjoint

    val _ = Testing.test "G.remove_from_bbox_anon (overlapping; 1)"
          (fn g =>
          let
            val b1_rem = (V.NSet.single x2)
            val bs_old = G.get_bboxes g
            val g' = G.remove_from_bbox_anon b1 b1_rem g
            val bs_new = G.get_bboxes g'
            val _ = if B.NSet.eq bs_old bs_new then ()
                    else raise ERROR "remove_from_bbox_anon changed the !-box set"
            val _ = if V.NSet.eq (V.NSet.single x3) (G.get_vertices_in_bbox g' b1) then ()
                    else raise ERROR "remove_from_bbox_anon did not set B1 contents properly"
            val _ = if V.NSet.eq (G.get_vertices_in_bbox g b0) (G.get_vertices_in_bbox g' b0) then ()
                    else raise ERROR "remove_from_bbox_anon(B1) changed B0"
          in g' end) g_overlap

    val _ = Testing.test "G.get_empty_bboxes (empty !-boxes)"
          (fn (g,blist) =>
          let
            val bs = G.get_empty_bboxes g
            val _ = if not (B.NSet.eq bs (B.NSet.of_list blist)) then
                      raise ERROR "get_empty_bboxes did not give correct !-boxes"
                    else ()
          in () end) (gv,[b0,b1,b2])

    val _ = Testing.test "G.get_empty_bboxes (disjoint)"
          (fn (g,blist) =>
          let
            val bs = G.get_empty_bboxes g
            val _ = if not (B.NSet.eq bs (B.NSet.of_list blist)) then
                      raise ERROR "get_empty_bboxes did not give correct !-boxes"
                    else ()
          in () end) (g_disjoint,[b2])

    val _ = Testing.test "G.get_bboxed (empty !-boxes)"
          (fn (g,vlist) =>
          let
            val vs = G.get_bboxed g
            val _ = if not (V.NSet.eq vs (V.NSet.of_list vlist)) then
                      raise ERROR "get_bboxed did not give correct verts"
                    else ()
          in () end) (gv,[])

    val _ = Testing.test "G.get_bboxed (disjoint)"
          (fn (g,vlist) =>
          let
            val vs = G.get_bboxed g
            val _ = if not (V.NSet.eq vs (V.NSet.of_list vlist)) then
                      raise ERROR "get_bboxed did not give correct verts"
                    else ()
          in () end) (g_disjoint,[x1,x2,x3])

    val _ = Testing.test "G.get_bboxed (overlapping)"
          (fn (g,vlist) =>
          let
            val vs = G.get_bboxed g
            val _ = if not (V.NSet.eq vs (V.NSet.of_list vlist)) then
                      raise ERROR "get_bboxed did not give correct verts"
                    else ()
          in () end) (g_overlap,[x1,x2,x3])

    val _ = Testing.test "G.get_unbboxed (empty !-boxes)"
          (fn (g,vlist) =>
          let
            val vs = G.get_unbboxed g
            val _ = if not (V.NSet.eq vs (V.NSet.of_list vlist)) then
                      raise ERROR "get_unbboxed did not give correct verts"
                    else ()
          in () end) (gv,[x1,x2,x3,x4])

    val _ = Testing.test "G.get_unbboxed (disjoint)"
          (fn (g,vlist) =>
          let
            val vs = G.get_unbboxed g
            val _ = if not (V.NSet.eq vs (V.NSet.of_list vlist)) then
                      raise ERROR "get_unbboxed did not give correct verts"
                    else ()
          in () end) (g_disjoint,[x4])

    val _ = Testing.test "G.get_unbboxed (overlapping)"
          (fn (g,vlist) =>
          let
            val vs = G.get_unbboxed g
            val _ = if not (V.NSet.eq vs (V.NSet.of_list vlist)) then
                      raise ERROR "get_unbboxed did not give correct verts"
                    else ()
          in () end) (g_overlap,[x4])

    val _ = Testing.test "G.get_bboxes_containing_vertex (disjoint)"
          (fn g =>
          let
            val bs1 = G.get_bboxes_containing_vertex g x1
            val b = case B.NSet.tryget_singleton bs1
                      of NONE => raise ERROR "get_bboxes_containing_vertex did not give correct number (1) of bboxes"
                       | SOME b' => b'
            val _ = if not (B.name_eq (b0,b)) then
                      raise ERROR "get_bboxes_containing_vertex did not give the correct !-box"
                    else ()
            val bs4 = G.get_bboxes_containing_vertex g x4
            val _ = if not (B.NSet.is_empty bs4) then
                      raise ERROR "get_bboxes_containing_vertex did not give correct number (0) of bboxes"
                    else ()
          in () end) g_disjoint

    val _ = Testing.test "G.get_bboxes_containing_vertex (overlapping)"
          (fn g =>
          let
            val bs3 = G.get_bboxes_containing_vertex g x3
            val b = case B.NSet.tryget_singleton bs3
                      of NONE => raise ERROR "get_bboxes_containing_vertex did not give correct number (1) of bboxes"
                       | SOME b' => b'
            val _ = if B.name_eq (b1,b) then ()
                    else raise ERROR "get_bboxes_containing_vertex did not give the correct !-box"
            val bs2 = G.get_bboxes_containing_vertex g x2
            val _ = if (B.NSet.cardinality bs2) = 2 then ()
                    else raise ERROR "get_bboxes_containing_vertex did not give correct number (2) of bboxes"
            val _ = if B.NSet.eq (B.NSet.of_list [b0,b1]) bs2 then ()
                    else raise ERROR "get_bboxes_containing_vertex did not give correct bboxes"
          in () end) g_overlap

    val _ = Testing.test "G.get_bboxes"
          (fn _ =>
          let
            val bs = G.get_bboxes g_disjoint
            val _ = if (B.NSet.cardinality bs) = 3 then ()
                    else raise ERROR "get_bboxes gave wrong length list"
            val _ = if B.NSet.eq bs (G.get_bboxes g_disjoint) then ()
                    else raise ERROR "get_bboxes gave different answer to get_bboxes"
          in () end) ()

    val _ = Testing.test "G.has_bbox"
          (fn _ =>
          let
            val _ = if G.has_bbox g_disjoint b0 then ()
                    else raise ERROR "has_bbox gave wrong answer for b0"
            val _ = if G.has_bbox g_disjoint b2 then ()
                    else raise ERROR "has_bbox gave wrong answer for b2"
            val _ = if not (G.has_bbox g_disjoint (B.mk "B9")) then ()
                    else raise ERROR "has_bbox gave wrong answer for 'B9'"
          in () end) ()

    val _ = Testing.test "G.is_bboxed"
          (fn _ =>
          let
            val _ = if G.is_bboxed g_disjoint x1 then ()
                    else raise ERROR "is_bboxed gave wrong answer for x1"
            val _ = if not (G.is_bboxed g_disjoint x4) then ()
                    else raise ERROR "is_bboxed gave wrong answer for x4"
            val _ = if G.is_bboxed g_overlap x2 then ()
                    else raise ERROR "is_bboxed gave wrong answer for x2 (overlapping)"
          in () end) ()

    val _ = Testing.test "G.delete_bbox (disjoint; no such bbox)"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val vs_old = G.get_vertices g
            val g' = G.delete_bbox (B.mk "B9") g
            val bs_new = G.get_bboxes g'
            val vs_new = G.get_vertices g'
            val _ = Testing.assert "bboxes unchanged" (B.NSet.eq bs_old bs_new)
            val _ = Testing.assert "verts unchanged" (V.NSet.eq vs_old vs_new)
          in g' end) g_disjoint

    val _ = Testing.test "G.delete_bbox (disjoint)"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val g' = G.delete_bbox b0 g
            val bs_new = G.get_bboxes g'
            val bs_added = B.NSet.subtract bs_new bs_old
            val _ = if not (B.NSet.is_empty bs_added) then
                      raise ERROR "delete_bbox managed to add something"
                    else ()
            val bs_removed = B.NSet.subtract bs_old bs_new
            val b = case B.NSet.tryget_singleton bs_removed of
                         NONE => raise ERROR "delete_bbox did not delete !-box"
                       | SOME b' => b'
            val _ = if not (B.name_eq (b0,b)) then
                      raise ERROR "delete_bbox did not delete the correct !-box"
                    else ()
            val _ = if not (V.NSet.eq (G.get_vertices g) (G.get_vertices g')) then
                      raise ERROR "delete_bbox changed vertices"
                    else ()
          in g' end) g_disjoint

    val _ = Testing.test "G.rename_bbox (disjoint; existing !-box)"
          (fn g => (G.rename_bbox b0 b1 g;
                    raise error "rename_bbox (name clash) did not throw exception")
                   handle BSub.name_clash_exp _ => I) g_disjoint

    val _ = Testing.test "G.rename_bbox (disjoint)"
          (fn g =>
          let
            val bnew = (B.mk "B9")
            val bs_old = G.get_bboxes g
            val g' = G.rename_bbox b0 bnew g
            val bs_new = G.get_bboxes g'
            val bs_added = B.NSet.subtract bs_new bs_old
            val badd = case B.NSet.tryget_singleton bs_added of
                         NONE => raise ERROR "rename_bbox did not add new !-box"
                       | SOME b' => b'
            val bs_removed = B.NSet.subtract bs_old bs_new
            val brem = case B.NSet.tryget_singleton bs_removed of
                         NONE => raise ERROR "rename_bbox did not remove old !-box"
                       | SOME b' => b'
            val _ = if not (B.name_eq (b0,brem)) then
                      raise ERROR "rename_bbox did not rename the correct !-box"
                    else ()
            val _ = if not (B.name_eq (bnew,badd)) then
                      raise ERROR "rename_bbox did not use the correct name"
                    else ()
            val _ = if not (V.NSet.eq (G.get_vertices_in_bbox g b0)
                                      (G.get_vertices_in_bbox g' bnew)) then
                      raise ERROR "rename_bbox did not preserve contents"
                    else ()
            val _ = if not (V.NSet.eq (G.get_vertices g) (G.get_vertices g')) then
                      raise ERROR "rename_bbox changed vertices"
                    else ()
          in g' end) g_disjoint

    (*val _ = Testing.test "G.try_rename1_bbox (disjoint; existing !-box)"
          (fn g => (case G.try_rename1_bbox b0 b1 g
                      of NONE => I
                       | SOME _ => raise error "try_rename1_bbox (name clash) did not return NONE")
                    ) g_disjoint

    val _ = Testing.test "G.try_rename1_bbox (disjoint)"
          (fn g =>
          let
            val bnew = (B.mk "B9")
            val bs_old = G.get_bboxes g
            val g' = case G.try_rename1_bbox b0 bnew g
                       of NONE => raise ERROR "try_rename1_bbox failed"
                        | SOME g' => g'
            val bs_new = G.get_bboxes g'
            val bs_added = B.NSet.subtract bs_new bs_old
            val badd = case B.NSet.tryget_singleton bs_added of
                         NONE => raise ERROR "try_rename1_bbox did not add new !-box"
                       | SOME b' => b'
            val bs_removed = B.NSet.subtract bs_old bs_new
            val brem = case B.NSet.tryget_singleton bs_removed of
                         NONE => raise ERROR "try_rename1_bbox did not remove old !-box"
                       | SOME b' => b'
            val _ = if not (B.name_eq (b0,brem)) then
                      raise ERROR "try_rename1_bbox did not rename the correct !-box"
                    else ()
            val _ = if not (B.name_eq (bnew,badd)) then
                      raise ERROR "try_rename1_bbox did not use the correct name"
                    else ()
            val _ = if not (V.NSet.eq (G.get_vertices_in_bbox g b0)
                                      (G.get_vertices_in_bbox g' bnew)) then
                      raise ERROR "try_rename1_bbox did not preserve contents"
                    else ()
            val _ = if not (V.NSet.eq (G.get_vertices g) (G.get_vertices g')) then
                      raise ERROR "try_rename1_bbox changed vertices"
                    else ()
          in g' end) g_disjoint*)

    val _ = Testing.test "G.add_named_bbox (name clash)"
          (fn g => (G.add_named_bbox b0 g;
                    raise error "add_named_bbox (name clash) did not throw exception")
                   handle G.duplicate_bbox_exp _ => I) gv

    val _ = Testing.test "G.add_named_bbox (new name)"
          (fn g =>
          let
            val bnew = (B.mk "B9")
            val bs_old = G.get_bboxes g
            val g' = G.add_named_bbox bnew g
            val bs_new = G.get_bboxes g'
            val bs_added = B.NSet.subtract bs_new bs_old
            val badd = case B.NSet.tryget_singleton bs_added of
                         NONE => raise ERROR "add_named_bbox did not add new !-box"
                       | SOME b' => b'
            val bs_removed = B.NSet.subtract bs_old bs_new
            val _ = if B.NSet.is_empty bs_removed then ()
                    else raise ERROR "add_named_bbox removed a !-box"
            val _ = if not (B.name_eq (bnew,badd)) then
                      raise ERROR "add_named_bbox did not use the correct name"
                    else ()
            val _ = if V.NSet.is_empty (G.get_vertices_in_bbox g' bnew) then ()
                    else raise ERROR "add_named_bbox put stuff in the new !-box"
            val _ = if V.NSet.eq (G.get_vertices g) (G.get_vertices g') then ()
                    else raise ERROR "add_named_bbox changed vertices"
          in g' end) gv

    val _ = Testing.test "G.add_bbox"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val (bnew,g') = G.add_bbox g
            val bs_new = G.get_bboxes g'
            val bs_added = B.NSet.subtract bs_new bs_old
            val badd = case B.NSet.tryget_singleton bs_added of
                         NONE => raise ERROR "add_bbox did not add new !-box"
                       | SOME b' => b'
            val bs_removed = B.NSet.subtract bs_old bs_new
            val _ = if B.NSet.is_empty bs_removed then ()
                    else raise ERROR "add_bbox removed a !-box"
            val _ = if not (B.name_eq (bnew,badd)) then
                      raise ERROR "add_bbox returned the wrong name"
                    else ()
            val _ = if V.NSet.is_empty (G.get_vertices_in_bbox g' bnew) then ()
                    else raise ERROR "add_bbox put stuff in the new !-box"
            val _ = if V.NSet.eq (G.get_vertices g) (G.get_vertices g') then ()
                    else raise ERROR "add_bbox changed vertices"
          in g' end) gv

    val _ = Testing.test "G.doadd_bbox"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val g' = G.add_bbox_anon g
            val bs_new = G.get_bboxes g'
            val bs_added = B.NSet.subtract bs_new bs_old
            val bnew = case B.NSet.tryget_singleton bs_added of
                         NONE => raise ERROR "doadd_bbox did not add new !-box"
                       | SOME b' => b'
            val bs_removed = B.NSet.subtract bs_old bs_new
            val _ = if B.NSet.is_empty bs_removed then ()
                    else raise ERROR "doadd_bbox removed a !-box"
            val _ = if V.NSet.is_empty (G.get_vertices_in_bbox g' bnew) then ()
                    else raise ERROR "doadd_bbox put stuff in the new !-box"
            val _ = if V.NSet.eq (G.get_vertices g) (G.get_vertices g') then ()
                    else raise ERROR "doadd_bbox changed vertices"
          in g' end) gv

    val _ = Testing.test "G.kill_bbox (no such bbox)"
          (fn g => (G.kill_bbox (B.mk "B9") g;
                    Testing.assert "kill_bbox throws exception" false)
                   handle G.no_such_bbox_exp _ => ()) g_disjoint

    val _ = Testing.test "G.kill_bbox (disjoint)"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val vs_old = G.get_vertices g
            val g' = G.kill_bbox b0 g
            val bs_new = G.get_bboxes g'
            val vs_new = G.get_vertices g'
            val bs_added = B.NSet.subtract bs_new bs_old
            val _ = Testing.assert "no !-boxes added" (B.NSet.is_empty bs_added)
            val bs_removed = B.NSet.subtract bs_old bs_new
            val b = case B.NSet.tryget_singleton bs_removed of
                         NONE => raise ERROR "kill_bbox did not delete !-box"
                       | SOME b' => b'
            val _ = Testing.assert "correct bbox killed" (B.name_eq (b0,b))
            val vs_added = V.NSet.subtract vs_new vs_old
            val _ = Testing.assert "no verts added" (V.NSet.is_empty vs_added)
            val vs_removed = V.NSet.subtract vs_old vs_new
            val _ = Testing.assert "correct verts removed"
                  (V.NSet.eq vs_removed (G.get_vertices_in_bbox g b0))
            val _ = Testing.assert "B1 unchanged"
                  (V.NSet.eq (G.get_vertices_in_bbox g b1) (G.get_vertices_in_bbox g' b1))
          in g' end) g_disjoint

    val _ = Testing.test "G.kill_bbox (overlapping)"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val vs_old = G.get_vertices g
            val g' = G.kill_bbox b0 g
            val bs_new = G.get_bboxes g'
            val vs_new = G.get_vertices g'
            val bs_added = B.NSet.subtract bs_new bs_old
            val _ = Testing.assert "no !-boxes added" (B.NSet.is_empty bs_added)
            val bs_removed = B.NSet.subtract bs_old bs_new
            val b = case B.NSet.tryget_singleton bs_removed of
                         NONE => raise ERROR "kill_bbox did not delete !-box"
                       | SOME b' => b'
            val _ = Testing.assert "correct bbox killed" (B.name_eq (b0,b))
            val vs_added = V.NSet.subtract vs_new vs_old
            val _ = Testing.assert "no verts added" (V.NSet.is_empty vs_added)
            val vs_removed = V.NSet.subtract vs_old vs_new
            val _ = Testing.assert "correct verts removed"
                  (V.NSet.eq vs_removed (G.get_vertices_in_bbox g b0))
            val exp_b1 = V.NSet.subtract (G.get_vertices_in_bbox g b1) vs_removed
            val _ = Testing.assert "Correct B1 verts removed"
                  (V.NSet.eq exp_b1 (G.get_vertices_in_bbox g' b1))
          in g' end) g_overlap

    val _ = Testing.test "G.drop_bbox (no such bbox)"
          (fn g => (G.drop_bbox (B.mk "B9") g;
                    Testing.assert "drop_bbox throws exception" false)
                   handle G.no_such_bbox_exp _ => ()) g_disjoint

    val _ = Testing.test "G.drop_bbox (disjoint)"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val g' = G.drop_bbox b0 g
            val bs_new = G.get_bboxes g'
            val bs_added = B.NSet.subtract bs_new bs_old
            val _ = if not (B.NSet.is_empty bs_added) then
                      raise ERROR "drop_bbox managed to add something"
                    else ()
            val bs_removed = B.NSet.subtract bs_old bs_new
            val b = case B.NSet.tryget_singleton bs_removed of
                         NONE => raise ERROR "drop_bbox did not delete !-box"
                       | SOME b' => b'
            val _ = if not (B.name_eq (b0,b)) then
                      raise ERROR "drop_bbox did not delete the correct !-box"
                    else ()
            val _ = if not (V.NSet.eq (G.get_vertices g) (G.get_vertices g')) then
                      raise ERROR "drop_bbox changed vertices"
                    else ()
          in g' end) g_disjoint

    val _ = Testing.test "G.merge_bboxes (empty set)"
          (fn g => (G.merge_bboxes B.NSet.empty g;
                    Testing.assert "merge_bboxes throws exception" false)
                   handle G.merge_bbox_exp _ => ()) g_disjoint

    val _ = Testing.test "G.merge_bboxes (some missing !-boxes)"
          (fn g => (G.merge_bboxes (B.NSet.of_list [b0,b1,B.mk "B9"]) g;
                    Testing.assert "merge_bboxes throws exception" false)
                   handle G.no_such_bbox_exp _ => ()) g_disjoint

    val _ = Testing.test "G.merge_bboxes (empty)"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val bs_mrg = (B.NSet.of_list [b0,b1])
            val (b,g') = G.merge_bboxes bs_mrg g
            val bs_new = G.get_bboxes g'
            val bs_exp = B.NSet.add b (B.NSet.subtract bs_old bs_mrg)
            val _ = Testing.assert "bboxes correct" (B.NSet.eq bs_exp bs_new)
            val _ = Testing.assert "verts unchanged"
                    (V.NSet.eq (G.get_vertices g') (G.get_vertices g))
            val _ = Testing.assert "!-boxes empty"
                    (B.NSet.eq (G.get_empty_bboxes g') bs_new)
          in g' end) gv

    val _ = Testing.test "G.merge_bboxes (disjoint)"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val bs_mrg = (B.NSet.of_list [b0,b1])
            val (b,g') = G.merge_bboxes bs_mrg g
            val bs_new = G.get_bboxes g'
            val bs_exp = B.NSet.add b (B.NSet.subtract bs_old bs_mrg)
            val _ = Testing.assert "bboxes correct" (B.NSet.eq bs_exp bs_new)
            val _ = Testing.assert "verts unchanged"
                    (V.NSet.eq (G.get_vertices g') (G.get_vertices g))
            val vexp = V.NSet.union_merge (G.get_vertices_in_bbox g b0)
                                          (G.get_vertices_in_bbox g b1)
            val _ = Testing.assert "correct verts"
                    (V.NSet.eq vexp (G.get_vertices_in_bbox g' b ))
          in g' end) g_disjoint

    val _ = Testing.test "G.merge_bboxes (overlapping)"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val bs_mrg = (B.NSet.of_list [b0,b1])
            val (b,g') = G.merge_bboxes bs_mrg g
            val bs_new = G.get_bboxes g'
            val bs_exp = B.NSet.add b (B.NSet.subtract bs_old bs_mrg)
            val _ = Testing.assert "bboxes correct" (B.NSet.eq bs_exp bs_new)
            val _ = Testing.assert "verts unchanged"
                    (V.NSet.eq (G.get_vertices g') (G.get_vertices g))
            val vexp = V.NSet.union_merge (G.get_vertices_in_bbox g b0)
                                          (G.get_vertices_in_bbox g b1)
            val _ = Testing.assert "correct verts"
                    (V.NSet.eq vexp (G.get_vertices_in_bbox g' b ))
          in g' end) g_overlap

    val _ = Testing.test "G.fresh_copy_bbox_anon (no such !-boxes)"
          (fn g => (G.fresh_copy_bbox_anon (B.mk "B9") g;
                    Testing.assert "fresh_copy_bbox_anon throws exception" false)
                   handle G.no_such_bbox_exp _ => ()) g_disjoint

    val _ = Testing.test "G.fresh_copy_bbox_anon (empty)"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val ((_,_,bsub),g') = G.fresh_copy_bbox b0 g
            val b = BSub.get bsub b0
            val bs_new = G.get_bboxes g'
            val bs_added = B.NSet.subtract bs_new bs_old
            val badd = case B.NSet.tryget_singleton bs_added of
                         NONE => raise ERROR "fresh_copy_bbox_anon did not add new !-box"
                       | SOME b' => b'
            val _ = Testing.assert "correct bbox added"
                    (B.name_eq (b,badd))
            val bs_removed = B.NSet.subtract bs_old bs_new
            val _ = Testing.assert "no bboxes removed"
                    (B.NSet.is_empty bs_removed)
            val _ = Testing.assert "verts unchanged"
                    (V.NSet.eq (G.get_vertices g') (G.get_vertices g))
            val _ = Testing.assert "!-boxes empty"
                    (B.NSet.eq (G.get_empty_bboxes g') bs_new)
          in g' end) gv

    val _ = Testing.test "G.fresh_copy_bbox_anon (disjoint)"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val ((_,_,bsub),g') = G.fresh_copy_bbox b0 g
            val b = BSub.get bsub b0
            val bs_new = G.get_bboxes g'
            val bs_added = B.NSet.subtract bs_new bs_old
            val badd = case B.NSet.tryget_singleton bs_added of
                         NONE => raise ERROR "fresh_copy_bbox_anon did not add new !-box"
                       | SOME b' => b'
            val _ = Testing.assert "correct bbox added"
                    (B.name_eq (b,badd))
            val bs_removed = B.NSet.subtract bs_old bs_new
            val _ = Testing.assert "no bboxes removed"
                    (B.NSet.is_empty bs_removed)
            val _ = Testing.assert "old verts remain"
                    (V.NSet.sub_set (G.get_vertices g) (G.get_vertices g'))
            val _ = Testing.assert "new verts are new"
                    (V.NSet.is_empty (V.NSet.intersect (G.get_vertices_in_bbox g' b) (G.get_vertices g)))
            val vs_exp = V.NSet.union_merge (G.get_vertices g) (G.get_vertices_in_bbox g' b)
            val _ = Testing.assert "new verts are as expected"
                    (V.NSet.eq vs_exp (G.get_vertices g'))
            val _ = Testing.assert "b1 untouched"
                    (V.NSet.eq (G.get_vertices_in_bbox g b1) (G.get_vertices_in_bbox g' b1))
          in g' end) g_disjoint

    val _ = Testing.test "G.fresh_copy_bbox_anon (overlapping)"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val ((_,_,bsub),g') = G.fresh_copy_bbox b0 g
            val b = BSub.get bsub b0
            val bs_new = G.get_bboxes g'
            val bs_added = B.NSet.subtract bs_new bs_old
            val badd = case B.NSet.tryget_singleton bs_added of
                         NONE => raise ERROR "fresh_copy_bbox_anon did not add new !-box"
                       | SOME b' => b'
            val _ = Testing.assert "correct bbox added"
                    (B.name_eq (b,badd))
            val bs_removed = B.NSet.subtract bs_old bs_new
            val _ = Testing.assert "no bboxes removed"
                    (B.NSet.is_empty bs_removed)
            val _ = Testing.assert "old verts remain"
                    (V.NSet.sub_set (G.get_vertices g) (G.get_vertices g'))
            val _ = Testing.assert "new verts are new"
                    (V.NSet.is_empty (V.NSet.intersect (G.get_vertices_in_bbox g' b) (G.get_vertices g)))
            val vs_exp = V.NSet.union_merge (G.get_vertices g) (G.get_vertices_in_bbox g' b)
            val _ = Testing.assert "new verts are as expected"
                    (V.NSet.eq vs_exp (G.get_vertices g'))
            val _ = Testing.assert "old b1 verts remain"
                    (V.NSet.sub_set (G.get_vertices_in_bbox g b1)
                                    (G.get_vertices_in_bbox g' b1))
            val b1s_new = V.NSet.subtract (G.get_vertices_in_bbox g' b1)
                                          (G.get_vertices_in_bbox g b1)
            val b1_new = case V.NSet.tryget_singleton b1s_new
                           of NONE => raise ERROR "b1 is not correct size"
                            | SOME x => x
            val _ = Testing.assert "b1 new vert correct"
                    (G.vdata_eq (G.get_vertex_data g' b1_new, G.get_vertex_data g x2))
          in g' end) g_overlap

    (* circle in a !-box *)
    val g = G.empty |> add_wv "v1"
                    |> add_dir_eunit1 "e1" "v1" "v1"
                    |> add_bbox "B0" ["v1"];
    val circles_pat = g

    val _ = Testing.test "G.fresh_copy_bbox_anon (circle)"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val ((_,_,bsub),g') = G.fresh_copy_bbox b0 g
            val b = BSub.get bsub b0
            val bs_new = G.get_bboxes g'
            val bs_added = B.NSet.subtract bs_new bs_old
            val badd = case B.NSet.tryget_singleton bs_added of
                         NONE => raise ERROR "fresh_copy_bbox_anon did not add new !-box"
                       | SOME b' => b'
            val _ = Testing.assert "correct bbox added"
                    (B.name_eq (b,badd))
            val bs_removed = B.NSet.subtract bs_old bs_new
            val _ = Testing.assert "no bboxes removed"
                    (B.NSet.is_empty bs_removed)
            val _ = Testing.assert "old verts remain"
                    (V.NSet.sub_set (G.get_vertices g) (G.get_vertices g'))
            val _ = Testing.assert "new verts are new"
                    (V.NSet.is_empty (V.NSet.intersect (G.get_vertices_in_bbox g' b) (G.get_vertices g)))
            val vs_exp = V.NSet.union_merge (G.get_vertices g) (G.get_vertices_in_bbox g' b)
            val _ = Testing.assert "new verts are as expected"
                    (V.NSet.eq vs_exp (G.get_vertices g'))
          in g' end) circles_pat

    val _ = Testing.test "G.fresh_expand_bbox_anon (circle)"
          (fn _ =>
          let
            val g = circles_pat
            val bs_old = G.get_bboxes g
            val g' = G.fresh_expand_bbox_anon b0 g
            val bs_new = G.get_bboxes g'
            val _ = Testing.assert "bboxes unchanged"
                    (B.NSet.eq bs_old bs_new)
            val _ = Testing.assert "old verts remain"
                    (V.NSet.sub_set (G.get_vertices g) (G.get_vertices g'))
            val _ = Testing.assert "old bbox the same"
                    (V.NSet.eq (G.get_vertices_in_bbox g b0)
                               (G.get_vertices_in_bbox g' b0))
            val _ = Testing.assert "correct number of verts"
                    ((V.NSet.cardinality (G.get_vertices g')) = 2)
            val _ = Testing.assert "correct number of edges"
                    ((E.NSet.cardinality (G.get_edges g')) = 2)
          in g' end) ()

    (* RHS of spider pattern *)
    val g = G.empty |> add_wv "v1"
                    |> add_vunit1 "v2"
                    |> add_wv "v3"
                    |> add_dir_eunit1 "e1" "v1" "v2"
                    |> add_dir_eunit2 "e2" "v2" "v3"
                    |> add_bbox "B0" ["v1"]
                    |> add_bbox "B1" ["v3"]
    val spider_rhs_pat = g

    val _ = Testing.test "G.fresh_copy_bbox_anon (spider)"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val ((_,_,bsub),g') = G.fresh_copy_bbox b0 g
            val b = BSub.get bsub b0
            val bs_new = G.get_bboxes g'
            val bs_added = B.NSet.subtract bs_new bs_old
            val badd = case B.NSet.tryget_singleton bs_added of
                         NONE => raise ERROR "fresh_copy_bbox_anon did not add new !-box"
                       | SOME b' => b'
            val _ = Testing.assert "correct bbox added"
                    (B.name_eq (b,badd))
            val bs_removed = B.NSet.subtract bs_old bs_new
            val _ = Testing.assert "no bboxes removed"
                    (B.NSet.is_empty bs_removed)
            val _ = Testing.assert "old verts remain"
                    (V.NSet.sub_set (G.get_vertices g) (G.get_vertices g'))
            val _ = Testing.assert "new verts are new"
                    (V.NSet.is_empty (V.NSet.intersect (G.get_vertices_in_bbox g' b) (G.get_vertices g)))
            val _ = Testing.assert "b0 unchanged"
                    (V.NSet.eq (G.get_vertices_in_bbox g b0)
                               (G.get_vertices_in_bbox g' b0))
            val _ = Testing.assert "b1 unchanged"
                    (V.NSet.eq (G.get_vertices_in_bbox g b1)
                               (G.get_vertices_in_bbox g' b1))
            val vadd = case V.NSet.tryget_singleton (G.get_vertices_in_bbox g' b) of
                         NONE => raise ERROR "new !-box has wrong number of vertices"
                       | SOME v' => v'
            val _ = Testing.assert "New !-box is connected"
                    (G.has_edges_between g' vadd v2)
          in g' end) spider_rhs_pat

    val _ = Testing.test "G.fresh_expand_bbox_anon (spider)"
          (fn g =>
          let
            val bs_old = G.get_bboxes g
            val g' = G.fresh_expand_bbox_anon b0 g
            val bs_new = G.get_bboxes g'
            val _ = Testing.assert "bboxes unchanged"
                    (B.NSet.eq bs_old bs_new)
            val _ = Testing.assert "old verts remain"
                    (V.NSet.sub_set (G.get_vertices g) (G.get_vertices g'))
            val _ = Testing.assert "b0 unchanged"
                    (V.NSet.eq (G.get_vertices_in_bbox g b0)
                               (G.get_vertices_in_bbox g' b0))
            val _ = Testing.assert "b1 unchanged"
                    (V.NSet.eq (G.get_vertices_in_bbox g b1)
                               (G.get_vertices_in_bbox g' b1))
            val newverts = V.NSet.subtract (G.get_vertices g') (G.get_vertices g)
            val vadd = case V.NSet.tryget_singleton newverts of
                         NONE => raise ERROR "wrong number of vertices added"
                       | SOME v' => v'
            val _ = Testing.assert "New vertex is connected"
                    (G.has_edges_between g' vadd v2)
          in g' end) spider_rhs_pat

    val g = G.empty |> add_vunit1 "l0"
                    |> add_vunit2 "l1-a"
                    |> add_vunit2 "l2-a"
                    |> add_dir_eunit1 "e1" "l0" "l1-a"
                    |> add_dir_eunit1 "e2" "l1-a" "l2-a"
                    |> add_bbox "B0" ["l1-a","l2-a"]
                    |> add_bbox "B1" ["l2-a"]
    val balanced_tree_pat = g
    val [lvl0,lvl1a] = map V.mk ["l0","l1-a"];

    val _ = Testing.test "G.kill_bbox (balanced tree; outer)"
          (fn g =>
          let
            val g' = G.kill_bbox b0 g
            val _ = Testing.assert "correct !-boxes"
                  (B.NSet.eq (B.NSet.single b1) (G.get_bboxes g'))
            val _ = Testing.assert "correct verts"
                  (V.NSet.eq (V.NSet.single lvl0) (G.get_vertices g'))
            val _ = Testing.assert "B1 is empty"
                  (V.NSet.is_empty (G.get_vertices_in_bbox g' b1))
          in g' end) balanced_tree_pat

    val _ = Testing.test "G.kill_bbox (balanced tree; inner)"
          (fn g =>
          let
            val g' = G.kill_bbox b1 g
            val _ = Testing.assert "correct !-boxes"
                  (B.NSet.eq (B.NSet.single b0) (G.get_bboxes g'))
            val _ = Testing.assert "correct verts"
                  (V.NSet.eq (V.NSet.of_list [lvl0,lvl1a]) (G.get_vertices g'))
            val _ = Testing.assert "B0 has correct verts"
                  (V.NSet.eq (V.NSet.single lvl1a) (G.get_vertices_in_bbox g' b0))
          in g' end) balanced_tree_pat

    val _ = Testing.test "G.fresh_expand_bbox_anon (balanced tree; outer)"
          (fn g =>
          let
            val g' = G.fresh_expand_bbox_anon b0 g
            val new_verts = V.NSet.subtract (G.get_vertices g') (G.get_vertices g)
          in
            (Testing.assert "bboxes unchanged"
               (B.NSet.eq (G.get_bboxes g) (G.get_bboxes g'));
             Testing.assert "old verts remain"
               (V.NSet.sub_set (G.get_vertices g) (G.get_vertices g'));
             Testing.assert "correct number of new verts"
               (V.NSet.cardinality new_verts = 2);
             Testing.assert "correct verts in b0"
               (V.NSet.eq (G.get_vertices_in_bbox g b0) (G.get_vertices_in_bbox g' b0));
             Testing.assert "correct number of verts in b1"
               (V.NSet.cardinality (G.get_vertices_in_bbox g' b1) = 2);
             g')
          end) balanced_tree_pat

    val _ = Testing.test "G.fresh_expand_bbox_anon (balanced tree; inner)"
          (fn g =>
          let
            val g' = G.fresh_expand_bbox_anon b1 g
            val new_verts = V.NSet.subtract (G.get_vertices g') (G.get_vertices g)
          in
            (Testing.assert "bboxes unchanged"
               (B.NSet.eq (G.get_bboxes g) (G.get_bboxes g'));
             Testing.assert "old verts remain"
               (V.NSet.sub_set (G.get_vertices g) (G.get_vertices g'));
             Testing.assert "correct number of new verts"
               (V.NSet.cardinality new_verts = 1);
             Testing.assert "correct number of new verts in b0"
               (V.NSet.cardinality (G.get_vertices_in_bbox g' b0) = 3);
             Testing.assert "correct verts in b1"
               (V.NSet.eq (G.get_vertices_in_bbox g b1) (G.get_vertices_in_bbox g' b1));
             g')
          end) balanced_tree_pat
  in val _ = () end; (* local *)


  (* testing wire functions with !-boxes *)
  local
    val w1 = V.mk "w1"
    val w2 = V.mk "w2"
    val w3 = V.mk "w3"
    val w4 = V.mk "w4"
    val w5 = V.mk "w5"
    val w6 = V.mk "w6"
    val x1 = V.mk "x1"
    val z1 = V.mk "z1"
    val z2 = V.mk "z2"
    val b1 = B.mk "B1"
    val b2 = B.mk "B2"
    val b3 = B.mk "B3"

    val g = G.empty |> add_wv "in1"
                    |> add_wv "in2"
                    |> add_wv "out1"
                    |> add_vunit1 "x1"
                    |> add_vunit1 "x2"
                    |> add_vunit2 "z1"
                    |> add_vunit2 "z2"
                    |> add_dir_eunit1 "e1" "in1" "x1"
                    |> add_dir_eunit1 "e2" "in2" "x1"
                    |> add_dir_eunit1 "e3" "z1" "x2"
                    |> add_dir_eunit1 "e4" "x1" "z2"
                    |> add_dir_eunit1 "e5" "x2" "z2"
                    |> add_dir_eunit1 "e6" "z2" "out1"
                    |> add_bbox "B1" ["in2"]
                    |> add_bbox "B2" ["z1"];
    val g1 = g

    val g = G.empty |> add_wv "in1"
                    |> add_wv "in2"
                    |> add_wv "out1"
                    |> add_wv "w1"
                    |> add_wv "w2"
                    |> add_wv "w3"
                    |> add_wv "w4"
                    |> add_wv "w5"
                    |> add_wv "w6"
                    |> add_vunit1 "x1"
                    |> add_vunit1 "x2"
                    |> add_vunit2 "z1"
                    |> add_vunit2 "z2"
                    |> add_dir_eunit1 "e1" "in1" "x1"
                    |> add_dir_eunit1 "e2" "in2" "x1"
                    |> add_dir_eunit1 "e3a" "z1" "w1"
                    |> add_dir_eunit1 "e3b" "w1" "w2"
                    |> add_dir_eunit1 "e3c" "w2" "x2"
                    |> add_dir_eunit1 "e4a" "x1" "w3"
                    |> add_dir_eunit1 "e4b" "w3" "w4"
                    |> add_dir_eunit1 "e4c" "w4" "z2"
                    |> add_dir_eunit1 "e5a" "x2" "w5"
                    |> add_dir_eunit1 "e5b" "w5" "w6"
                    |> add_dir_eunit1 "e5c" "w6" "z2"
                    |> add_dir_eunit1 "e6" "z2" "out1"
                    |> add_bbox "B1" ["in2"]
                    |> add_bbox "B2" ["z1","w1","w2"];
    val g1_norm = g

    (*val _ = Testing.test "G.normalise"
      (fn () =>
        let
          val g1_norm' = G.normalise g1
        in
          if GIso.test g1_norm' g1_norm then ()
          else (writeln "Expected"; G.print g1_norm;
                writeln "Got"; G.print g1_norm';
                raise ERROR "Normalise gave incorrect graph")
        end) () *)

    val g = G.empty |> add_wv "w1"
                    |> add_wv "w2"
                    |> add_wv "w3"
                    |> add_wv "w4"
                    |> add_wv "w5"
                    |> add_vunit1 "x1"
                    |> add_vunit1 "x2"
                    |> add_vunit2 "z1"
                    |> add_vunit2 "z2"
                    |> add_bbox "B1" ["w1","w3"]
                    |> add_bbox "B2" ["w4"]
                    |> add_bbox "B3" ["w1","w2","w3","w4","z1"]
                    |> add_bbox "B4" ["z2"]
                    |> add_bbox "B5" []
                    |> add_bbox "B6" [];

    (*val _ = Testing.test "G.tryget_bbox_extended_by_edge (1)"
      (fn () =>
        case G.tryget_bbox_extended_by_edge w1 w2 g
          of NONE => raise ERROR "tryget_bbox_extended_by_edge claimed edge would not extend !-box"
           | SOME b => Testing.assert "Correct !-box returned" (B.name_eq (b,b1))
      ) ()
    val _ = Testing.test "G.tryget_bbox_extended_by_edge (2)"
      (fn () =>
        case G.tryget_bbox_extended_by_edge w1 w3 g
          of NONE => ()
           | SOME _ => raise ERROR "tryget_bbox_extended_by_edge claimed edge would extend !-box"
      ) ()
    val _ = Testing.test "G.tryget_bbox_extended_by_edge (3)"
      (fn () =>
        case G.tryget_bbox_extended_by_edge w1 w4 g
          of NONE => raise ERROR "tryget_bbox_extended_by_edge claimed edge would not extend !-box"
           | SOME b => Testing.assert "Correct !-box returned"
                        ((B.name_eq (b,b1)) orelse ((B.name_eq (b,b2))))
      ) ()
    val _ = Testing.test "G.tryget_bbox_extended_by_edge (4)"
      (fn () =>
        case G.tryget_bbox_extended_by_edge x1 z1 g
          of NONE => ()
           | SOME _ => raise ERROR "tryget_bbox_extended_by_edge claimed edge would extend !-box"
      ) ()
    val _ = Testing.test "G.tryget_bbox_extended_by_edge (5)"
      (fn () =>
        case G.tryget_bbox_extended_by_edge x1 w1 g
          of NONE => ()
           | SOME _ => raise ERROR "tryget_bbox_extended_by_edge claimed edge would extend !-box"
      ) ()
    val _ = Testing.test "G.tryget_bbox_extended_by_edge (6)"
      (fn () =>
        case G.tryget_bbox_extended_by_edge z1 w1 g
          of NONE => ()
           | SOME _ => raise ERROR "tryget_bbox_extended_by_edge claimed edge would extend !-box"
      ) ()
    val _ = Testing.test "G.tryget_bbox_extended_by_edge (7)"
      (fn () =>
        case G.tryget_bbox_extended_by_edge w1 z1 g
          of NONE => ()
           | SOME _ => raise ERROR "tryget_bbox_extended_by_edge claimed edge would extend !-box"
      ) ()
    val _ = Testing.test "G.tryget_bbox_extended_by_edge (8)"
      (fn () =>
        case G.tryget_bbox_extended_by_edge x1 w5 g
          of NONE => ()
           | SOME _ => raise ERROR "tryget_bbox_extended_by_edge claimed edge would extend !-box"
      ) ()
    val _ = Testing.test "G.tryget_bbox_extended_by_edge (9)"
      (fn () =>
        case G.tryget_bbox_extended_by_edge z1 w5 g
          of NONE => raise ERROR "tryget_bbox_extended_by_edge claimed edge would not extend !-box"
           | SOME b => Testing.assert "Correct !-box returned"
                        (B.name_eq (b,b3))
      ) ()
    val _ = Testing.test "G.tryget_bbox_extended_by_edge (10)"
      (fn () =>
        case G.tryget_bbox_extended_by_edge w5 z1 g
          of NONE => raise ERROR "tryget_bbox_extended_by_edge claimed edge would not extend !-box"
           | SOME b => Testing.assert "Correct !-box returned"
                        (B.name_eq (b,b3))
      ) () *)

    val g = G.empty |> add_wv "w1"
                    |> add_wv "w2"
                    |> add_wv "w3"
                    |> add_wv "w4"
                    |> add_wv "w5"
                    |> add_wv "w6"
                    |> add_vunit1 "x1"
                    |> add_vunit1 "x2"
                    |> add_vunit2 "z1"
                    |> add_vunit2 "z2"
                    |> add_bbox "B1" []
                    |> add_bbox "B2" []
                    |> add_bbox "B3" []
                    |> add_bbox "B4" []
                    |> add_bbox "B5" []
                    |> add_bbox "B6" []
                    |> add_undir_eunit1 "e1" "w1" "w2"
                    |> add_undir_eunit1 "e2" "x1" "w3"
                    |> add_undir_eunit1 "e3" "w3" "w4"
                    |> add_undir_eunit1 "e4" "w4" "x2"
                    |> add_undir_eunit1 "e5" "z1" "w5"
                    |> add_undir_eunit1 "e6" "z2" "w6"
                    |> add_undir_eunit1 "e7" "z2" "w6";

    val _ = Testing.test "G.add_to_bbox_anon/G.remove_from_bbox_anon"
      (fn () =>
      let
        val g' = G.add_to_bbox_anon b1 (V.NSet.of_list [w1,x1,z1,z2]) g
        val g'' = G.remove_from_bbox_anon b1 (V.NSet.of_list [w2,w3,w4,w5,w6]) g'
        val g''' = G.remove_from_bbox_anon b1 (V.NSet.of_list [x1,z1,z2]) g'
        val g'''' = G.remove_from_bbox_anon b1 (V.NSet.of_list [w1,w2,x1,w3,w4,z1,w5,z2,w6]) g'
        val b1vs = G.get_vertices_in_bbox g' b1
      in
        Testing.assert "b1 has right verts"
          (V.NSet.eq b1vs (V.NSet.of_list [w1,w2,x1,w3,w4,z1,w5,z2,w6]));
        Testing.assert "removal had no effect"
          (V.NSet.eq b1vs (G.get_vertices_in_bbox g'' b1));
        Testing.assert "b1 has right verts after removal of nvs"
          (V.NSet.eq (G.get_vertices_in_bbox g''' b1)
                     (V.NSet.of_list [w1,w2,w3,w4,w5,w6]));
        Testing.assert "b1 is empty after removing everything"
          (V.NSet.is_empty (G.get_vertices_in_bbox g'''' b1))
      end
      ) ()
  in val _ = () end; (* local *)


  (* testing nested !-boxes *)

  local
    (* balanced tree of depth 3 *)
    val g = G.empty |> add_vunit1 "v1"
                    |> add_vunit1 "v2"
                    |> add_vunit1 "v3"
                    |> add_dir_eunit1 "e1" "v1" "v2"
                    |> add_dir_eunit1 "e2" "v2" "v3"
                    |> add_bbox "B1" ["v2","v3"]
                    |> add_bbox "B2" ["v3"]
    val balanced = g

    (* unbalanced tree of depth 3 *)
    val unbalanced = balanced |> G.add_child_to_bbox b1 b2

    val unbalanced_norm = G.normalise unbalanced
    
    val _ = Testing.test "G.bbox_has_parents" (fn() =>
      Testing.assert "b2 has a parent" (G.bbox_has_parents unbalanced b2)) ()
    val _ = Testing.test "G.bbox_has_parents" (fn() =>
      Testing.assert "b1 does not have a parent" (not (G.bbox_has_parents unbalanced b1))) ()
    
    val _ = Testing.test "drop_bbox on parent box" (fn() =>
      let val utree_drop = unbalanced |> G.drop_bbox b1 in
        Testing.assert "b2 has no parent" (not (G.bbox_has_parents utree_drop b2))
      end) ()
    
    val _ = Testing.test "unbalanced tree: kill_bbox on parent box" (fn() =>
      let val g = unbalanced |> G.kill_bbox b1 in
        Testing.assert "no !-boxes" (B.NSet.is_empty (G.get_bboxes g))
      end) ()
    
    val _ = Testing.test "unbalanced tree: kill_bbox on child box" (fn() =>
      let val g = unbalanced |> G.kill_bbox b2 in
        Testing.assert "only b1 exists"
           (B.NSet.eq (B.NSet.single b1) (G.get_bboxes g))
      end) ()
    
    val _ = Testing.test "unbalanced tree: copy_bbox on parent box" (fn() =>
      let
        val oldg = unbalanced
        val ((_,_,bsub),g) = G.fresh_copy_bbox b1 oldg
        val b1' = BSub.get bsub b1
        val _ = Testing.assert "4 !-boxes" (B.NSet.cardinality (G.get_bboxes g) = 4)
        val new_bboxes = B.NSet.subtract (G.get_bboxes g) (G.get_bboxes oldg)
        val b2' = (the o B.NSet.tryget_singleton) (B.NSet.delete b1' new_bboxes)
      in
        Testing.assert "b1' looks right"
          (V.NSet.cardinality (G.get_vertices_in_bbox g b1) =
           V.NSet.cardinality (G.get_vertices_in_bbox g b1'));
        Testing.assert "b2' looks right"
          (V.NSet.cardinality (G.get_vertices_in_bbox g b2) =
           V.NSet.cardinality (G.get_vertices_in_bbox g b2'))
      end) ()
    
    val _ = Testing.test "unbalanced tree: copy_bbox on child box" (fn() =>
      let
        val oldg = unbalanced
        val ((_,_,bsub),g) = G.fresh_copy_bbox b2 oldg
        val b2' = BSub.get bsub b2
        val _ = Testing.assert "3 !-boxes" (B.NSet.cardinality (G.get_bboxes g) = 3)
      in
        Testing.assert "b2' looks right"
          (V.NSet.cardinality (G.get_vertices_in_bbox g b2) =
           V.NSet.cardinality (G.get_vertices_in_bbox g b2'))
      end) ()
    
    val _ = Testing.test "normed unbalanced tree: kill_bbox on parent box" (fn() =>
      let val g = unbalanced_norm |> G.kill_bbox b1 in
        Testing.assert "no !-boxes" (B.NSet.is_empty (G.get_bboxes g))
      end) ()
    
    val _ = Testing.test "normed unbalanced tree: kill_bbox on child box" (fn() =>
      let val g = unbalanced_norm |> G.kill_bbox b2 in
        Testing.assert "only b1 exists"
           (B.NSet.eq (B.NSet.single b1) (G.get_bboxes g))
      end) ()
    
    val _ = Testing.test "normed unbalanced tree: copy_bbox on parent box" (fn() =>
      let
        val oldg = unbalanced_norm
        val ((_,_,bsub),g) = G.fresh_copy_bbox b1 oldg
        val b1' = BSub.get bsub b1
        val _ = Testing.assert "4 !-boxes" (B.NSet.cardinality (G.get_bboxes g) = 4)
        val new_bboxes = B.NSet.subtract (G.get_bboxes g) (G.get_bboxes oldg)
        val b2' = (the o B.NSet.tryget_singleton) (B.NSet.delete b1' new_bboxes)
      in
        Testing.assert ("b1' looks right " ^
          Int.toString (V.NSet.cardinality (G.get_vertices_in_bbox g b1)) ^ " != " ^
          Int.toString (V.NSet.cardinality (G.get_vertices_in_bbox g b1')))
          (V.NSet.cardinality (G.get_vertices_in_bbox g b1) =
           V.NSet.cardinality (G.get_vertices_in_bbox g b1'));
        Testing.assert "b2' looks right"
          (V.NSet.cardinality (G.get_vertices_in_bbox g b2) =
           V.NSet.cardinality (G.get_vertices_in_bbox g b2'))
      end) ()
    
    val _ = Testing.test "normed unbalanced tree: copy_bbox on child box" (fn() =>
      let 
        val g = G.fresh_copy_bbox_anon b2 unbalanced_norm
      in
        Testing.assert "4 !-boxes" (B.NSet.cardinality (G.get_bboxes g) = 3)
      end) ()
  in val _ = () end; (* local *)
in
  val _ = Testing.assert_no_failed_tests()
end;

