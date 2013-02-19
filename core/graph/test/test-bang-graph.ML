local
  (* start by testing that things in the OGRAPH interface still work *)
  structure OGraph_Interface_Tests_Bang_Graph = OGraph_Interface_Tests(Test_Bang_Graph);

  open Test_Bang_Graph_Tools;
in
  val _ = ()
end;


(* Test basic !-box methods *)
local
  open Test_RG_BBox_Util;

  val ge = G.empty
  val _ = Testing.test "G.get_bboxes - empty"
    (fn () => if B.NSet.is_empty (G.get_bboxes ge) then I
            else raise ERROR "get_bboxes (empty) incorrect") ()

  val _ = Testing.test "G.get_bbox_rel - empty"
    (fn () => if BVBRel.is_empty (G.get_bbox_rel ge) then I
            else raise ERROR "get_bbox_rel (empty) incorrect") ()

  val _ = Testing.test "G.get_bbox_table - empty"
    (fn () => if B.NTab.is_name_empty (G.get_bbox_table ge) then I
            else raise ERROR "get_bbox_table (empty) incorrect") ()

  val _ = Testing.test "G.get_vertex_bbox_table - empty"
    (fn () => if V.NTab.is_name_empty (G.get_vertex_bbox_table ge) then I
            else raise ERROR "get_vertex_bbox_table (empty) incorrect") ()

  val _ = Testing.test "G.get_bboxed - empty"
    (fn () => if V.NSet.is_empty (G.get_bboxed ge) then I
            else raise ERROR "get_bboxed (empty) incorrect") ()

  val _ = Testing.test "G.get_unbboxed - empty"
    (fn () => if V.NSet.is_empty (G.get_unbboxed ge) then I
            else raise ERROR "get_unbboxed (empty) incorrect") ()

  val _ = Testing.test "G.get_empty_bboxes - empty"
    (fn () => if B.NSet.is_empty (G.get_empty_bboxes ge) then I
            else raise ERROR "get_empty_bboxes (empty) incorrect") ()

  val _ = Testing.test "G.get_bbox_list - empty"
    (fn () => case G.get_bbox_list ge of
                   [] => I
                 | _ => raise ERROR "get_bbox_list (empty) incorrect") ()

  val _ = Testing.test "G.has_bbox - empty"
    (fn () => if not (G.has_bbox ge B.default_name) then I
            else raise ERROR "has_bbox (empty) incorrect") ()

  val _ = Testing.test "G.get_bbox_contents - empty"
    (fn () => (G.get_bbox_contents ge B.default_name;
            raise ERROR "get_bbox_contents (empty) incorrect")
            handle G.no_such_bbox_exp _ => I) ()

  val _ = Testing.test "G.get_bboxes_of - empty"
    (fn () => (G.get_bboxes_of ge V.default_name;
            raise ERROR "get_bboxes_of (empty) incorrect")
            handle G.no_such_vname_exp _ => I) ()

  val _ = Testing.test "G.is_bboxed - empty"
    (fn () => (G.is_bboxed ge V.default_name;
            raise ERROR "is_bboxed (empty) incorrect")
            handle G.no_such_vname_exp _ => I) ()

  val _ = Testing.test "G.rename_bbox - empty"
    (fn () => let
                val g' = G.rename_bbox (B.mk "B0") (B.mk "B1") ge
              in
                (* no effect *)
                if B.NSet.is_empty (G.get_bboxes g') then I
                else raise ERROR "rename_bbox (empty) incorrect"
              end) ()

  val _ = Testing.test "G.try_rename1_bbox - empty"
    (fn () =>
      case G.try_rename1_bbox (B.mk "B0") (B.mk "B1") ge of
           NONE =>
             raise ERROR "try_rename1_bbox (empty) incorrect [got NONE]"
         | SOME g' =>
             (* no effect *)
             if B.NSet.is_empty (G.get_bboxes g') then I
             else raise ERROR "try_rename1_bbox (empty) incorrect [added bbox]"
         ) ()

  val _ = Testing.test "G.delete_bbox - empty"
    (fn () => G.delete_bbox B.default_name ge) ()

  val _ = Testing.test "g.doadd_to_bbox - empty"
    (fn () => (G.doadd_to_bbox B.default_name (V.NSet.single V.default_name) ge;
            raise error "doadd_to_bbox (empty) incorrect")
            handle G.no_such_vname_exp _ => I
                 | G.no_such_bbox_exp _ => I) ()

  val _ = Testing.test "G.doremove_from_bbox - empty"
    (fn () => (G.doremove_from_bbox B.default_name (V.NSet.single V.default_name) ge;
            raise ERROR "doremove_from_bbox (empty) incorrect")
            handle G.no_such_vname_exp _ => I
                 | G.no_such_bbox_exp _ => I) ()

  val _ = Testing.test "G.kill_bbox - empty"
    (fn () => (G.kill_bbox B.default_name ge;
            raise ERROR "kill_bbox (empty) incorrect")
            handle G.no_such_bbox_exp _ => I) ()

  val _ = Testing.test "G.drop_bbox - empty"
    (fn () => (G.drop_bbox B.default_name ge;
            raise ERROR "drop_bbox (empty) incorrect")
            handle G.no_such_bbox_exp _ => I) ()

  val _ = Testing.test "G.merge_bboxes - empty"
    (fn () =>
    let
      val bs = B.NSet.of_list [B.mk "B0", B.mk "B1"]
    in
      (G.merge_bboxes bs ge;
            raise ERROR "merge_bboxes (empty) incorrect")
            handle G.no_such_bbox_exp _ => I
    end) ()

  (* FIXME: test copy1_bbox *)

  val _ = Testing.test "G.fresh_copy1_bbox - empty"
    (fn () => (G.fresh_copy1_bbox B.default_name ge;
            raise ERROR "fresh_copy1_bbox (empty) incorrect")
            handle G.no_such_bbox_exp _ => I) ()

  (* FIXME: test rename_apart_incl_bboxes *)

  val b0 = B.mk "B0"
  val b1 = B.mk "B1"
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
        end) ge

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
                 handle G.bbox_already_exists_exp _ => I) gnv

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

  val _ = Testing.test "G.rename_bbox (no such bb; new tgt)"
        (fn g =>
        let
          val bfrom = (B.mk "B8")
          val bto = (B.mk "B9")
          val bs_old = G.get_bboxes g
          val g' = G.rename_bbox bfrom bto g
          val bs_new = G.get_bboxes g'
        in
          if not (B.NSet.eq bs_old bs_new) then
            raise ERROR "rename_bbox changed the graph"
          else g'
        end) gnv

  val _ = Testing.test "G.rename_bbox (no such bb; existing tgt)"
        (fn g =>
        let
          val bfrom = (B.mk "B8")
          val bto = b0
          val bs_old = G.get_bboxes g
          val g' = G.rename_bbox bfrom bto g
          val bs_new = G.get_bboxes g'
        in
          if not (B.NSet.eq bs_old bs_new) then
            raise ERROR "rename_bbox changed the graph"
          else g'
        end) gnv

  val _ = Testing.test "G.doadd_to_bbox (no such verts)"
        (fn g => (G.doadd_to_bbox b0 (V.NSet.single V.default_name) g;
                  raise error "doadd_to_bbox (no such verts) incorrect")
                 handle G.no_such_vnames_exp _ => I) gnv

  val gv = gnv
  val (x1,gv) = gv |> G.add_vertex (mkX  zero_angle);
  val (x2,gv) = gv |> G.add_vertex mkH;
  val (x3,gv) = gv |> G.add_vertex (mkZ  zero_angle);
  val (x4,gv) = gv |> G.add_vertex (mkZ  (parse_angle "a + b"));
  val (_ ,gv) = gv |> G.add_edge dir_edge x1 x2
  val (_ ,gv) = gv |> G.add_edge dir_edge x2 x3
  val (_ ,gv) = gv |> G.add_edge dir_edge x3 x1
  val (_ ,gv) = gv |> G.add_edge dir_edge x4 x1
  val (_ ,gv) = gv |> G.add_edge dir_edge x4 x2
  val (_ ,gv) = gv |> G.add_edge dir_edge x4 x3

  val _ = Testing.test "G.doadd_to_bbox (no such bbox)"
        (fn g => (G.doadd_to_bbox (B.mk "B9") (V.NSet.single x1) g;
                  raise error "doadd_to_bbox (no such bbox) incorrect")
                 handle G.no_such_bbox_exp _ => I) gv

  val g_disjoint = test_force "G.doadd_to_bbox/G.get_bbox_contents (disjoint)"
        (fn g =>
        let
          val b0_exp = (V.NSet.single x1)
          val b1_exp = (V.NSet.of_list [x2,x3])
          val bs_old = G.get_bboxes g
          val g' = G.doadd_to_bbox b0 b0_exp g
          val g'' = G.doadd_to_bbox b1 b1_exp g'
          val bs_new = G.get_bboxes g''
          val _ = if not (B.NSet.eq bs_old bs_new) then
                    raise ERROR "doadd_to_bbox changed the !-box set"
                  else ()
          val _ = if not (V.NSet.eq b0_exp (G.get_bbox_contents g'' b0)) then
                    raise ERROR "doadd_to_bbox did not set B0 contents properly"
                  else ()
          val _ = if not (V.NSet.eq b1_exp (G.get_bbox_contents g'' b1)) then
                    raise ERROR "doadd_to_bbox did not set B1 contents properly"
                  else ()
        in g'' end) gv

  val g_overlap = test_force "G.doadd_to_bbox/G.get_bbox_contents (overlapping)"
        (fn g =>
        let
          val b0_exp = (V.NSet.of_list [x1,x2])
          val b1_exp = (V.NSet.of_list [x2,x3])
          val bs_old = G.get_bboxes g
          val g' = G.doadd_to_bbox b0 b0_exp g
          val g'' = G.doadd_to_bbox b1 b1_exp g'
          val bs_new = G.get_bboxes g''
          val _ = if not (B.NSet.eq bs_old bs_new) then
                    raise ERROR "doadd_to_bbox changed the !-box set"
                  else ()
          val _ = if not (V.NSet.eq b0_exp (G.get_bbox_contents g'' b0)) then
                    raise ERROR "doadd_to_bbox did not set B0 contents properly"
                  else ()
          val _ = if not (V.NSet.eq b1_exp (G.get_bbox_contents g'' b1)) then
                    raise ERROR "doadd_to_bbox did not set B1 contents properly"
                  else ()
        in g'' end) gv

  val _ = Testing.test "G.doremove_from_bbox (no such bbox)"
        (fn g => (G.doremove_from_bbox (B.mk "B9") (V.NSet.single x1) g;
                  raise error "doremove_from_bbox (no such bbox) incorrect")
                 handle G.no_such_bbox_exp _ => I) gv

  val _ = Testing.test "G.doremove_from_bbox (disjoint; 1)"
        (fn g =>
        let
          val b1_rem = (V.NSet.single x2)
          val bs_old = G.get_bboxes g
          val g' = G.doremove_from_bbox b1 b1_rem g
          val bs_new = G.get_bboxes g'
          val _ = if B.NSet.eq bs_old bs_new then ()
                  else raise ERROR "doremove_from_bbox changed the !-box set"
          val _ = if V.NSet.eq (V.NSet.single x3) (G.get_bbox_contents g' b1) then ()
                  else raise ERROR "doremove_from_bbox did not set B1 contents properly"
        in g' end) g_disjoint

  val _ = Testing.test "G.doremove_from_bbox (disjoint; 2)"
        (fn g =>
        let
          val b1_rem = (V.NSet.of_list [x2,x3])
          val bs_old = G.get_bboxes g
          val g' = G.doremove_from_bbox b1 b1_rem g
          val bs_new = G.get_bboxes g'
          val _ = if B.NSet.eq bs_old bs_new then ()
                  else raise ERROR "doremove_from_bbox changed the !-box set"
          val _ = if V.NSet.is_empty (G.get_bbox_contents g' b1) then ()
                  else raise ERROR "doremove_from_bbox did not set B1 contents properly"
        in g' end) g_disjoint

  val _ = Testing.test "G.doremove_from_bbox (disjoint; 3)"
        (fn g =>
        let
          val b1_rem = (V.NSet.of_list [x1,x2])
          val bs_old = G.get_bboxes g
          val g' = G.doremove_from_bbox b1 b1_rem g
          val bs_new = G.get_bboxes g'
          val _ = if B.NSet.eq bs_old bs_new then ()
                  else raise ERROR "doremove_from_bbox changed the !-box set"
          val _ = if V.NSet.eq (V.NSet.single x3) (G.get_bbox_contents g' b1) then ()
                  else raise ERROR "doremove_from_bbox did not set B1 contents properly"
        in g' end) g_disjoint

  val _ = Testing.test "G.doremove_from_bbox (overlapping; 1)"
        (fn g =>
        let
          val b1_rem = (V.NSet.single x2)
          val bs_old = G.get_bboxes g
          val g' = G.doremove_from_bbox b1 b1_rem g
          val bs_new = G.get_bboxes g'
          val _ = if B.NSet.eq bs_old bs_new then ()
                  else raise ERROR "doremove_from_bbox changed the !-box set"
          val _ = if V.NSet.eq (V.NSet.single x3) (G.get_bbox_contents g' b1) then ()
                  else raise ERROR "doremove_from_bbox did not set B1 contents properly"
          val _ = if V.NSet.eq (G.get_bbox_contents g b0) (G.get_bbox_contents g' b0) then ()
                  else raise ERROR "doremove_from_bbox(B1) changed B0"
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

  val _ = Testing.test "G.get_bboxes_of (disjoint)"
        (fn g =>
        let
          val bs1 = G.get_bboxes_of g x1
          val b = case B.NSet.tryget_singleton bs1
                    of NONE => raise ERROR "get_bboxes_of did not give correct number (1) of bboxes"
                     | SOME b' => b'
          val _ = if not (B.name_eq (b0,b)) then
                    raise ERROR "get_bboxes_of did not give the correct !-box"
                  else ()
          val bs4 = G.get_bboxes_of g x4
          val _ = if not (B.NSet.is_empty bs4) then
                    raise ERROR "get_bboxes_of did not give correct number (0) of bboxes"
                  else ()
        in () end) g_disjoint

  val _ = Testing.test "G.get_bboxes_of (overlapping)"
        (fn g =>
        let
          val bs3 = G.get_bboxes_of g x3
          val b = case B.NSet.tryget_singleton bs3
                    of NONE => raise ERROR "get_bboxes_of did not give correct number (1) of bboxes"
                     | SOME b' => b'
          val _ = if B.name_eq (b1,b) then ()
                  else raise ERROR "get_bboxes_of did not give the correct !-box"
          val bs2 = G.get_bboxes_of g x2
          val _ = if (B.NSet.cardinality bs2) = 2 then ()
                  else raise ERROR "get_bboxes_of did not give correct number (2) of bboxes"
          val _ = if B.NSet.eq (B.NSet.of_list [b0,b1]) bs2 then ()
                  else raise ERROR "get_bboxes_of did not give correct bboxes"
        in () end) g_overlap

  val _ = Testing.test "G.get_bbox_list"
        (fn _ =>
        let
          val bs = G.get_bbox_list g_disjoint
          val _ = if (length bs) = 3 then ()
                  else raise ERROR "get_bbox_list gave wrong length list"
          val _ = if B.NSet.eq (B.NSet.of_list bs) (G.get_bboxes g_disjoint) then ()
                  else raise ERROR "get_bbox_list gave different answer to get_bboxes"
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
          val vs_old = G.get_vnames g
          val g' = G.delete_bbox (B.mk "B9") g
          val bs_new = G.get_bboxes g'
          val vs_new = G.get_vnames g'
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
          val _ = if not (V.NSet.eq (G.get_vnames g) (G.get_vnames g')) then
                    raise ERROR "delete_bbox changed vertices"
                  else ()
        in g' end) g_disjoint

  val _ = Testing.test "G.rename_bbox (disjoint; existing !-box)"
        (fn g => (G.rename_bbox b0 b1 g;
                  raise error "rename_bbox (name clash) did not throw exception")
                 handle G.bbox_already_exists_exp _ => I) g_disjoint

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
          val _ = if not (V.NSet.eq (G.get_bbox_contents g b0)
                                    (G.get_bbox_contents g' bnew)) then
                    raise ERROR "rename_bbox did not preserve contents"
                  else ()
          val _ = if not (V.NSet.eq (G.get_vnames g) (G.get_vnames g')) then
                    raise ERROR "rename_bbox changed vertices"
                  else ()
        in g' end) g_disjoint

  val _ = Testing.test "G.try_rename1_bbox (disjoint; existing !-box)"
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
          val _ = if not (V.NSet.eq (G.get_bbox_contents g b0)
                                    (G.get_bbox_contents g' bnew)) then
                    raise ERROR "try_rename1_bbox did not preserve contents"
                  else ()
          val _ = if not (V.NSet.eq (G.get_vnames g) (G.get_vnames g')) then
                    raise ERROR "try_rename1_bbox changed vertices"
                  else ()
        in g' end) g_disjoint

  val _ = Testing.test "G.add_named_bbox (name clash)"
        (fn g => (G.add_named_bbox b0 g;
                  raise error "add_named_bbox (name clash) did not throw exception")
                 handle G.bbox_already_exists_exp _ => I) gv

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
          val _ = if V.NSet.is_empty (G.get_bbox_contents g' bnew) then ()
                  else raise ERROR "add_named_bbox put stuff in the new !-box"
          val _ = if V.NSet.eq (G.get_vnames g) (G.get_vnames g') then ()
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
          val _ = if V.NSet.is_empty (G.get_bbox_contents g' bnew) then ()
                  else raise ERROR "add_bbox put stuff in the new !-box"
          val _ = if V.NSet.eq (G.get_vnames g) (G.get_vnames g') then ()
                  else raise ERROR "add_bbox changed vertices"
        in g' end) gv

  val _ = Testing.test "G.doadd_bbox"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val g' = G.doadd_bbox g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val bnew = case B.NSet.tryget_singleton bs_added of
                       NONE => raise ERROR "doadd_bbox did not add new !-box"
                     | SOME b' => b'
          val bs_removed = B.NSet.subtract bs_old bs_new
          val _ = if B.NSet.is_empty bs_removed then ()
                  else raise ERROR "doadd_bbox removed a !-box"
          val _ = if V.NSet.is_empty (G.get_bbox_contents g' bnew) then ()
                  else raise ERROR "doadd_bbox put stuff in the new !-box"
          val _ = if V.NSet.eq (G.get_vnames g) (G.get_vnames g') then ()
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
          val vs_old = G.get_vnames g
          val g' = G.kill_bbox b0 g
          val bs_new = G.get_bboxes g'
          val vs_new = G.get_vnames g'
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
                (V.NSet.eq vs_removed (G.get_bbox_contents g b0))
          val _ = Testing.assert "B1 unchanged"
                (V.NSet.eq (G.get_bbox_contents g b1) (G.get_bbox_contents g' b1))
        in g' end) g_disjoint

  val _ = Testing.test "G.kill_bbox (overlapping)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val vs_old = G.get_vnames g
          val g' = G.kill_bbox b0 g
          val bs_new = G.get_bboxes g'
          val vs_new = G.get_vnames g'
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
                (V.NSet.eq vs_removed (G.get_bbox_contents g b0))
          val exp_b1 = V.NSet.subtract (G.get_bbox_contents g b1) vs_removed
          val _ = Testing.assert "Correct B1 verts removed"
                (V.NSet.eq exp_b1 (G.get_bbox_contents g' b1))
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
          val _ = if not (V.NSet.eq (G.get_vnames g) (G.get_vnames g')) then
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
                  (V.NSet.eq (G.get_vnames g') (G.get_vnames g))
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
                  (V.NSet.eq (G.get_vnames g') (G.get_vnames g))
          val vexp = V.NSet.union_merge (G.get_bbox_contents g b0)
                                        (G.get_bbox_contents g b1)
          val _ = Testing.assert "correct verts"
                  (V.NSet.eq vexp (G.get_bbox_contents g' b ))
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
                  (V.NSet.eq (G.get_vnames g') (G.get_vnames g))
          val vexp = V.NSet.union_merge (G.get_bbox_contents g b0)
                                        (G.get_bbox_contents g b1)
          val _ = Testing.assert "correct verts"
                  (V.NSet.eq vexp (G.get_bbox_contents g' b ))
        in g' end) g_overlap

  val _ = Testing.test "G.fresh_copy1_bbox (no such !-boxes)"
        (fn g => (G.fresh_copy1_bbox (B.mk "B9") g;
                  Testing.assert "fresh_copy1_bbox throws exception" false)
                 handle G.no_such_bbox_exp _ => ()) g_disjoint

  val _ = Testing.test "G.fresh_copy1_bbox (empty)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val (b,g') = G.fresh_copy1_bbox b0 g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val badd = case B.NSet.tryget_singleton bs_added of
                       NONE => raise ERROR "fresh_copy1_bbox did not add new !-box"
                     | SOME b' => b'
          val _ = Testing.assert "correct bbox added"
                  (B.name_eq (b,badd))
          val bs_removed = B.NSet.subtract bs_old bs_new
          val _ = Testing.assert "no bboxes removed"
                  (B.NSet.is_empty bs_removed)
          val _ = Testing.assert "verts unchanged"
                  (V.NSet.eq (G.get_vnames g') (G.get_vnames g))
          val _ = Testing.assert "!-boxes empty"
                  (B.NSet.eq (G.get_empty_bboxes g') bs_new)
        in g' end) gv

  val _ = Testing.test "G.fresh_copy1_bbox (disjoint)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val (b,g') = G.fresh_copy1_bbox b0 g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val badd = case B.NSet.tryget_singleton bs_added of
                       NONE => raise ERROR "fresh_copy1_bbox did not add new !-box"
                     | SOME b' => b'
          val _ = Testing.assert "correct bbox added"
                  (B.name_eq (b,badd))
          val bs_removed = B.NSet.subtract bs_old bs_new
          val _ = Testing.assert "no bboxes removed"
                  (B.NSet.is_empty bs_removed)
          val _ = Testing.assert "old verts remain"
                  (V.NSet.sub_set (G.get_vnames g) (G.get_vnames g'))
          val _ = Testing.assert "new verts are new"
                  (V.NSet.is_empty (V.NSet.intersect (G.get_bbox_contents g' b) (G.get_vnames g)))
          val vs_exp = V.NSet.union_merge (G.get_vnames g) (G.get_bbox_contents g' b)
          val _ = Testing.assert "new verts are as expected"
                  (V.NSet.eq vs_exp (G.get_vnames g'))
          val _ = Testing.assert "b1 untouched"
                  (V.NSet.eq (G.get_bbox_contents g b1) (G.get_bbox_contents g' b1))
        in g' end) g_disjoint

  val _ = Testing.test "G.fresh_copy1_bbox (overlapping)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val (b,g') = G.fresh_copy1_bbox b0 g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val badd = case B.NSet.tryget_singleton bs_added of
                       NONE => raise ERROR "fresh_copy1_bbox did not add new !-box"
                     | SOME b' => b'
          val _ = Testing.assert "correct bbox added"
                  (B.name_eq (b,badd))
          val bs_removed = B.NSet.subtract bs_old bs_new
          val _ = Testing.assert "no bboxes removed"
                  (B.NSet.is_empty bs_removed)
          val _ = Testing.assert "old verts remain"
                  (V.NSet.sub_set (G.get_vnames g) (G.get_vnames g'))
          val _ = Testing.assert "new verts are new"
                  (V.NSet.is_empty (V.NSet.intersect (G.get_bbox_contents g' b) (G.get_vnames g)))
          val vs_exp = V.NSet.union_merge (G.get_vnames g) (G.get_bbox_contents g' b)
          val _ = Testing.assert "new verts are as expected"
                  (V.NSet.eq vs_exp (G.get_vnames g'))
          val _ = Testing.assert "old b1 verts remain"
                  (V.NSet.sub_set (G.get_bbox_contents g b1)
                                  (G.get_bbox_contents g' b1))
          val b1s_new = V.NSet.subtract (G.get_bbox_contents g' b1)
                                        (G.get_bbox_contents g b1)
          val b1_new = case V.NSet.tryget_singleton b1s_new
                         of NONE => raise ERROR "b1 is not correct size"
                          | SOME x => x
          val _ = Testing.assert "b1 new vert correct"
                  (G.VData.data_eq (G.get_vertex_data g' b1_new,
                                    G.get_vertex_data g x2))
        in g' end) g_overlap

  val g = G.empty
  (* circle in a !-box *)
  val (v1,g) = g |> G.add_vertex bvert
  val (_ ,g) = g |> G.add_edge dir_edge v1 v1
  val g      = g |> G.add_named_bbox b0
  val g      = g |> G.doadd_to_bbox b0 (V.NSet.single v1)
  val circles_pat = g

  val _ = Testing.test "G.fresh_copy1_bbox (circle)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val (b,g') = G.fresh_copy1_bbox b0 g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val badd = case B.NSet.tryget_singleton bs_added of
                       NONE => raise ERROR "fresh_copy1_bbox did not add new !-box"
                     | SOME b' => b'
          val _ = Testing.assert "correct bbox added"
                  (B.name_eq (b,badd))
          val bs_removed = B.NSet.subtract bs_old bs_new
          val _ = Testing.assert "no bboxes removed"
                  (B.NSet.is_empty bs_removed)
          val _ = Testing.assert "old verts remain"
                  (V.NSet.sub_set (G.get_vnames g) (G.get_vnames g'))
          val _ = Testing.assert "new verts are new"
                  (V.NSet.is_empty (V.NSet.intersect (G.get_bbox_contents g' b) (G.get_vnames g)))
          val vs_exp = V.NSet.union_merge (G.get_vnames g) (G.get_bbox_contents g' b)
          val _ = Testing.assert "new verts are as expected"
                  (V.NSet.eq vs_exp (G.get_vnames g'))
        in g' end) circles_pat

  val _ = Testing.test "G.fresh_expand1_bbox (circle)"
        (fn _ =>
        let
          val g = circles_pat
          val bs_old = G.get_bboxes g
          val g' = G.fresh_expand1_bbox b0 g
          val bs_new = G.get_bboxes g'
          val _ = Testing.assert "bboxes unchanged"
                  (B.NSet.eq bs_old bs_new)
          val _ = Testing.assert "old verts remain"
                  (V.NSet.sub_set (G.get_vnames g) (G.get_vnames g'))
          val _ = Testing.assert "old bbox the same"
                  (V.NSet.eq (G.get_bbox_contents g b0)
                             (G.get_bbox_contents g' b0))
          val _ = Testing.assert "correct number of verts"
                  ((V.NSet.cardinality (G.get_vnames g')) = 2)
          val _ = Testing.assert "correct number of edges"
                  ((E.NSet.cardinality (G.get_enames g')) = 2)
        in g' end) ()

  val g = G.empty
  (* RHS of spider pattern *)
  val (v1,g) = g |> G.add_vertex bvert
  val (v2,g) = g |> G.add_vertex (mkX zero_angle)
  val (v3,g) = g |> G.add_vertex bvert
  val (_ ,g) = g |> G.add_edge dir_edge v1 v2
  val (_ ,g) = g |> G.add_edge dir_edge v2 v3
  val g      = g |> G.add_named_bbox b0
                 |> G.add_named_bbox b1
                 |> G.doadd_to_bbox b0 (V.NSet.single v1)
                 |> G.doadd_to_bbox b1 (V.NSet.single v3)
  val spider_rhs_pat = g

  val _ = Testing.test "G.fresh_copy1_bbox (spider)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val (b,g') = G.fresh_copy1_bbox b0 g
          val bs_new = G.get_bboxes g'
          val bs_added = B.NSet.subtract bs_new bs_old
          val badd = case B.NSet.tryget_singleton bs_added of
                       NONE => raise ERROR "fresh_copy1_bbox did not add new !-box"
                     | SOME b' => b'
          val _ = Testing.assert "correct bbox added"
                  (B.name_eq (b,badd))
          val bs_removed = B.NSet.subtract bs_old bs_new
          val _ = Testing.assert "no bboxes removed"
                  (B.NSet.is_empty bs_removed)
          val _ = Testing.assert "old verts remain"
                  (V.NSet.sub_set (G.get_vnames g) (G.get_vnames g'))
          val _ = Testing.assert "new verts are new"
                  (V.NSet.is_empty (V.NSet.intersect (G.get_bbox_contents g' b) (G.get_vnames g)))
          val _ = Testing.assert "b0 unchanged"
                  (V.NSet.eq (G.get_bbox_contents g b0)
                             (G.get_bbox_contents g' b0))
          val _ = Testing.assert "b1 unchanged"
                  (V.NSet.eq (G.get_bbox_contents g b1)
                             (G.get_bbox_contents g' b1))
          val vadd = case V.NSet.tryget_singleton (G.get_bbox_contents g' b) of
                       NONE => raise ERROR "new !-box has wrong number of vertices"
                     | SOME v' => v'
          val _ = Testing.assert "New !-box is connected"
                  (G.has_edges_between g' vadd v2)
        in g' end) spider_rhs_pat

  val _ = Testing.test "G.fresh_expand1_bbox (spider)"
        (fn g =>
        let
          val bs_old = G.get_bboxes g
          val g' = G.fresh_expand1_bbox b0 g
          val bs_new = G.get_bboxes g'
          val _ = Testing.assert "bboxes unchanged"
                  (B.NSet.eq bs_old bs_new)
          val _ = Testing.assert "old verts remain"
                  (V.NSet.sub_set (G.get_vnames g) (G.get_vnames g'))
          val _ = Testing.assert "b0 unchanged"
                  (V.NSet.eq (G.get_bbox_contents g b0)
                             (G.get_bbox_contents g' b0))
          val _ = Testing.assert "b1 unchanged"
                  (V.NSet.eq (G.get_bbox_contents g b1)
                             (G.get_bbox_contents g' b1))
          val newverts = V.NSet.subtract (G.get_vnames g') (G.get_vnames g)
          val vadd = case V.NSet.tryget_singleton newverts of
                       NONE => raise ERROR "wrong number of vertices added"
                     | SOME v' => v'
          val _ = Testing.assert "New vertex is connected"
                  (G.has_edges_between g' vadd v2)
        in g' end) spider_rhs_pat

  val lvl0 = V.mk "l0"
  val lvl1a = V.mk "l1-a"
  val lvl2a = V.mk "l2-a"

  val g = G.empty
  val g = g |> G.add_named_vertex lvl0 (mkZ zero_angle)
            |> G.add_named_vertex lvl1a (mkX zero_angle)
            |> G.add_named_vertex lvl2a (mkX zero_angle)
            |> G.doadd_edge dir_edge lvl0 lvl1a
            |> G.doadd_edge dir_edge lvl1a lvl2a
            |> G.add_named_bbox b0
            |> G.add_named_bbox b1
            |> G.doadd_to_bbox b0 (V.NSet.of_list [lvl1a,lvl2a])
            |> G.doadd_to_bbox b1 (V.NSet.single lvl2a)
  val balanced_tree_pat = g

  val _ = Testing.test "G.kill_bbox (balanced tree; outer)"
        (fn g =>
        let
          val g' = G.kill_bbox b0 g
          val _ = Testing.assert "correct !-boxes"
                (B.NSet.eq (B.NSet.single b1) (G.get_bboxes g'))
          val _ = Testing.assert "correct verts"
                (V.NSet.eq (V.NSet.single lvl0) (G.get_vnames g'))
          val _ = Testing.assert "B1 is empty"
                (V.NSet.is_empty (G.get_bbox_contents g' b1))
        in g' end) balanced_tree_pat

  val _ = Testing.test "G.kill_bbox (balanced tree; inner)"
        (fn g =>
        let
          val g' = G.kill_bbox b1 g
          val _ = Testing.assert "correct !-boxes"
                (B.NSet.eq (B.NSet.single b0) (G.get_bboxes g'))
          val _ = Testing.assert "correct verts"
                (V.NSet.eq (V.NSet.of_list [lvl0,lvl1a]) (G.get_vnames g'))
          val _ = Testing.assert "B0 has correct verts"
                (V.NSet.eq (V.NSet.single lvl1a) (G.get_bbox_contents g' b0))
        in g' end) balanced_tree_pat

  val _ = Testing.test "G.fresh_expand1_bbox (balanced tree; outer)"
        (fn g =>
        let
          val g' = G.fresh_expand1_bbox b0 g
          val new_verts = V.NSet.subtract (G.get_vnames g') (G.get_vnames g)
        in
          (Testing.assert "bboxes unchanged"
             (B.NSet.eq (G.get_bboxes g) (G.get_bboxes g'));
           Testing.assert "old verts remain"
             (V.NSet.sub_set (G.get_vnames g) (G.get_vnames g'));
           Testing.assert "correct number of new verts"
             (V.NSet.cardinality new_verts = 2);
           Testing.assert "correct verts in b0"
             (V.NSet.eq (G.get_bbox_contents g b0) (G.get_bbox_contents g' b0));
           Testing.assert "correct number of verts in b1"
             (V.NSet.cardinality (G.get_bbox_contents g' b1) = 2);
           g')
        end) balanced_tree_pat

  val _ = Testing.test "G.fresh_expand1_bbox (balanced tree; inner)"
        (fn g =>
        let
          val g' = G.fresh_expand1_bbox b1 g
          val new_verts = V.NSet.subtract (G.get_vnames g') (G.get_vnames g)
        in
          (Testing.assert "bboxes unchanged"
             (B.NSet.eq (G.get_bboxes g) (G.get_bboxes g'));
           Testing.assert "old verts remain"
             (V.NSet.sub_set (G.get_vnames g) (G.get_vnames g'));
           Testing.assert "correct number of new verts"
             (V.NSet.cardinality new_verts = 1);
           Testing.assert "correct number of new verts in b0"
             (V.NSet.cardinality (G.get_bbox_contents g' b0) = 3);
           Testing.assert "correct verts in b1"
             (V.NSet.eq (G.get_bbox_contents g b1) (G.get_bbox_contents g' b1));
           g')
        end) balanced_tree_pat
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR !-BOX FUNCTIONS PASSED!"
end; (* ends local namespace used for testing *)


(* testing wire functions with !-boxes *)
local
  open Test_RG_BBox_Util;
  
  val in1 = V.mk "in1"
  val in2 = V.mk "in2"
  val out1 = V.mk "out1"
  val w1 = V.mk "w1"
  val w2 = V.mk "w2"
  val w3 = V.mk "w3"
  val w4 = V.mk "w4"
  val w5 = V.mk "w5"
  val w6 = V.mk "w6"
  val x1 = V.mk "x1"
  val x2 = V.mk "x2"
  val z1 = V.mk "z1"
  val z2 = V.mk "z2"
  val b1 = B.mk "B1"
  val b2 = B.mk "B2"
  val b3 = B.mk "B3"
  val b4 = B.mk "B4"
  val b5 = B.mk "B5"
  val b6 = B.mk "B6"

  val g = G.empty
  val g = g |> G.add_named_vertex in1 bvert
            |> G.add_named_vertex in2 bvert
            |> G.add_named_vertex out1 bvert
            |> G.add_named_vertex x1 (mkX zero_angle)
            |> G.add_named_vertex x2 (mkX zero_angle)
            |> G.add_named_vertex z1 (mkZ zero_angle)
            |> G.add_named_vertex z2 (mkZ zero_angle)
            |> G.doadd_edge dir_edge in1 x1
            |> G.doadd_edge dir_edge in2 x1
            |> G.doadd_edge dir_edge z1 x2
            |> G.doadd_edge dir_edge x1 z2
            |> G.doadd_edge dir_edge x2 z2
            |> G.doadd_edge dir_edge z2 out1
            |> G.add_named_bbox b1
            |> G.add_named_bbox b2
            |> G.doadd_to_bbox b1 (V.NSet.single in2)
            |> G.doadd_to_bbox b2 (V.NSet.single z1)
  val g1 = g

  val g = G.empty
  val g = g |> G.add_named_vertex in1 bvert
            |> G.add_named_vertex in2 bvert
            |> G.add_named_vertex out1 bvert
            |> G.add_named_vertex w1 bvert
            |> G.add_named_vertex w2 bvert
            |> G.add_named_vertex w3 bvert
            |> G.add_named_vertex w4 bvert
            |> G.add_named_vertex w5 bvert
            |> G.add_named_vertex w6 bvert
            |> G.add_named_vertex x1 (mkX zero_angle)
            |> G.add_named_vertex x2 (mkX zero_angle)
            |> G.add_named_vertex z1 (mkZ zero_angle)
            |> G.add_named_vertex z2 (mkZ zero_angle)
            |> G.doadd_edge dir_edge in1 x1
            |> G.doadd_edge dir_edge in2 x1
            |> G.doadd_edge dir_edge z1 w1
            |> G.doadd_edge dir_edge w1 w2
            |> G.doadd_edge dir_edge w2 x2
            |> G.doadd_edge dir_edge x1 w3
            |> G.doadd_edge dir_edge w3 w4
            |> G.doadd_edge dir_edge w4 z2
            |> G.doadd_edge dir_edge x2 w5
            |> G.doadd_edge dir_edge w5 w6
            |> G.doadd_edge dir_edge w6 z2
            |> G.doadd_edge dir_edge z2 out1
            |> G.add_named_bbox b1
            |> G.add_named_bbox b2
            |> G.doadd_to_bbox b1 (V.NSet.single in2)
            |> G.doadd_to_bbox b2 (V.NSet.of_list [z1,w1,w2])
  val g1_norm = g

  val _ = Testing.test "G.normalise"
    (fn () =>
      let
        val g1_norm' = G.normalise g1
      in
        if GIso.test g1_norm' g1_norm then ()
        else (writeln "Expected"; G.print g1_norm;
              writeln "Got"; G.print g1_norm';
              raise ERROR "Normalise gave incorrect graph")
      end) ()

  val g = G.empty
  val g = g |> G.add_named_vertex w1 bvert
            |> G.add_named_vertex w2 bvert
            |> G.add_named_vertex w3 bvert
            |> G.add_named_vertex w4 bvert
            |> G.add_named_vertex w5 bvert
            |> G.add_named_vertex x1 (mkX zero_angle)
            |> G.add_named_vertex x2 (mkX zero_angle)
            |> G.add_named_vertex z1 (mkZ zero_angle)
            |> G.add_named_vertex z2 (mkZ zero_angle)
            |> G.add_named_bbox b1
            |> G.add_named_bbox b2
            |> G.add_named_bbox b3
            |> G.add_named_bbox b4
            |> G.add_named_bbox b5
            |> G.add_named_bbox b6
            |> G.doadd_to_bbox b1 (V.NSet.of_list [w1,w3])
            |> G.doadd_to_bbox b2 (V.NSet.single w4)
            |> G.doadd_to_bbox b3 (V.NSet.of_list [w1,w2,w3,w4,z1])
            |> G.doadd_to_bbox b4 (V.NSet.single z2)
  val _ = Testing.test "G.tryget_bbox_extended_by_edge (1)"
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
    ) ()

  val g = G.empty
  val g = g |> G.add_named_vertex w1 bvert
            |> G.add_named_vertex w2 bvert
            |> G.add_named_vertex w3 bvert
            |> G.add_named_vertex w4 bvert
            |> G.add_named_vertex w5 bvert
            |> G.add_named_vertex w6 bvert
            |> G.add_named_vertex x1 (mkX zero_angle)
            |> G.add_named_vertex x2 (mkX zero_angle)
            |> G.add_named_vertex z1 (mkZ zero_angle)
            |> G.add_named_vertex z2 (mkZ zero_angle)
            |> G.add_named_bbox b1
            |> G.add_named_bbox b2
            |> G.add_named_bbox b3
            |> G.add_named_bbox b4
            |> G.add_named_bbox b5
            |> G.add_named_bbox b6
            |> G.doadd_edge undir_edge w1 w2
            |> G.doadd_edge undir_edge x1 w3
            |> G.doadd_edge undir_edge w3 w4
            |> G.doadd_edge undir_edge w4 x2
            |> G.doadd_edge undir_edge z1 w5
            |> G.doadd_edge undir_edge z2 w6
            |> G.doadd_edge undir_edge z2 w6
  val _ = Testing.test "G.doadd_to_bbox/G.doremove_from_bbox"
    (fn () =>
    let
      val g' = G.doadd_to_bbox b1 (V.NSet.of_list [w1,x1,z1,z2]) g
      val g'' = G.doremove_from_bbox b1 (V.NSet.of_list [w2,w3,w4,w5,w6]) g'
      val g''' = G.doremove_from_bbox b1 (V.NSet.of_list [x1,z1,z2]) g'
      val g'''' = G.doremove_from_bbox b1 (V.NSet.of_list [w1,w2,x1,w3,w4,z1,w5,z2,w6]) g'
      val b1vs = G.get_bbox_contents g' b1
    in
      Testing.assert "b1 has right verts"
        (V.NSet.eq b1vs (V.NSet.of_list [w1,w2,x1,w3,w4,z1,w5,z2,w6]));
      Testing.assert "removal had no effect"
        (V.NSet.eq b1vs (G.get_bbox_contents g'' b1));
      Testing.assert "b1 has right verts after removal of nvs"
        (V.NSet.eq (G.get_bbox_contents g''' b1)
                   (V.NSet.of_list [w1,w2,w3,w4,w5,w6]));
      Testing.assert "b1 is empty after removing everything"
        (V.NSet.is_empty (G.get_bbox_contents g'''' b1))
    end
    ) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR WIRE FUNCTIONS WITH !-BOXES PASSED!"
end; (* ends local namespace used for testing *)


(* testing nested !-boxes *)

local
  open Test_RG_BBox_Util;
  
  val g = G.empty
  (* balanced tree of depth 3 *)
  val (v1,g) = g |> G.add_vertex (mkZ zero_angle)
  val (v2,g) = g |> G.add_vertex (mkZ zero_angle)
  val (v3,g) = g |> G.add_vertex (mkZ zero_angle)
  val (_ ,g) = g |> G.add_edge undir_edge v1 v2
  val (_ ,g) = g |> G.add_edge undir_edge v2 v3
  val (b1,g) = g |> G.add_bbox
  val (b2,g) = g |> G.add_bbox
  val g      = g |> G.doadd_to_bbox b1 (V.NSet.of_list [v2,v3])
  val g      = g |> G.doadd_to_bbox b2 (V.NSet.of_list [v3])
  val balanced = g

  (* unbalenced tree of depth 3 *)
  val unbalanced = balanced |> G.set_bbox_parent b2 b1

  val unbalanced_norm = G.normalise unbalanced
  val _ = G.print unbalanced_norm
  
  val _ = Testing.test "G.bbox_has_parent" (fn() =>
    Testing.assert "b2 has a parent" (G.bbox_has_parent unbalanced b2)) ()
  val _ = Testing.test "G.bbox_has_parent" (fn() =>
    Testing.assert "b1 does not have a parent" (not (G.bbox_has_parent unbalanced b1))) ()
  
  val _ = Testing.test "drop_bbox on parent box" (fn() =>
    let val utree_drop = unbalanced |> G.drop_bbox b1 in
      Testing.assert "b2 has no parent" (not (G.bbox_has_parent utree_drop b2))
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
      val (b1',g) = G.fresh_copy1_bbox b1 oldg
      val _ = Testing.assert "4 !-boxes" (B.NSet.cardinality (G.get_bboxes g) = 4)
      val new_bboxes = B.NSet.subtract (G.get_bboxes g) (G.get_bboxes oldg)
      val b2' = (the o B.NSet.tryget_singleton) (B.NSet.delete b1' new_bboxes)
    in
      G.print oldg;
      G.print g;
      Testing.assert "b1' looks right"
        (V.NSet.cardinality (G.get_bbox_contents g b1) =
         V.NSet.cardinality (G.get_bbox_contents g b1'));
      Testing.assert "b2' looks right"
        (V.NSet.cardinality (G.get_bbox_contents g b2) =
         V.NSet.cardinality (G.get_bbox_contents g b2'))
    end) ()
  
  val _ = Testing.test "unbalanced tree: copy_bbox on child box" (fn() =>
    let
      val oldg = unbalanced
      val (b2',g) = G.fresh_copy1_bbox b2 oldg
      val _ = Testing.assert "3 !-boxes" (B.NSet.cardinality (G.get_bboxes g) = 3)
    in
      G.print g;
      Testing.assert "b2' looks right"
        (V.NSet.cardinality (G.get_bbox_contents g b2) =
         V.NSet.cardinality (G.get_bbox_contents g b2'))
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
      val (b1',g) = G.fresh_copy1_bbox b1 oldg
      val _ = Testing.assert "4 !-boxes" (B.NSet.cardinality (G.get_bboxes g) = 4)
      val new_bboxes = B.NSet.subtract (G.get_bboxes g) (G.get_bboxes oldg)
      val b2' = (the o B.NSet.tryget_singleton) (B.NSet.delete b1' new_bboxes)
    in
      Testing.assert "b1' looks right"
        (V.NSet.cardinality (G.get_bbox_contents g b1) =
         V.NSet.cardinality (G.get_bbox_contents g b1'));
      Testing.assert "b2' looks right"
        (V.NSet.cardinality (G.get_bbox_contents g b2) =
         V.NSet.cardinality (G.get_bbox_contents g b2'))
    end) ()
  
  val _ = Testing.test "normed unbalanced tree: copy_bbox on child box" (fn() =>
    let val (_,g) = G.fresh_copy1_bbox b2 unbalanced_norm in
      Testing.assert "4 !-boxes" (B.NSet.cardinality (G.get_bboxes g) = 3)
    end) ()
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR NESTED BANG-BOX EXPANSION PASSED!"
end
